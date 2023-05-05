#' @name import_fisheye
#' @aliases import_fisheye list.cameras
#' @title Import a fisheye image as a single channel raster, and apply a circular mask
#' @param filename Character. The input image filename.
#' @param channel Character. Either the band number corresponding to an image channel or a mixing channel method (Available options are: 'first','GLA','Luma','2BG','BtoRG','B','GEI','RGB'). Default is 3 (Blue channel).
#' @param gamma Numeric. It indicates the input gamma, which is then back-corrected to unity. Default is 2.2 (typical in jpeg images). If no gamma is required, just set gamma=1.
#' @param stretch Logical. It indicates if a linear stretch should be applied to enhance contrast. Default FALSE.
#' @param circ.mask List. The circular mask parameters (xc,yc,rc) to be applied to the image. It can be created from a list of available cameras using the [camera_fisheye()] function. If omitted, it is created automatically in circular images, and corresponds to half the lower image side.
#' @param circular Logical. It indicates if the fisheye image is circular (circular=TRUE) or fullframe (circular=FALSE) type. This influences the way the radius is calculated if circ.mask is not inserted. Default is circular.
#' @param display Logical. If is set to TRUE, it plots the image along with the applied mask and a circle radius. Default to FALSE.
#' @param message Logical. If is set to TRUE, it prints the mask used for importing the image. Default to TRUE.
#'
#' @return A single-channel raster image.
#' @importFrom raster plot extent values nlayers stack  stretch rasterToPoints
#' @importFrom plotrix draw.circle
#' @importFrom graphics segments
#' @importFrom grDevices gray.colors
#' @importFrom scales rescale
#'
#' @description
#' This function imports fisheye images using [raster::raster()] functionality, by selecting a single channel, or a combination of channels.
#' The default option (blue channel) is generally preferred for canopy image analysis as it enables high contrast between canopy and sky pixels, which ease image thresholding.
#' A circular mask is then applied to mask outside pixel in case of circular fisheye images. It can be manually inserted, or retrieved using the [camera_fisheye()] function.
#' Alternatively, it is automatically calculated. Additional functions include a gamma correction and a contrast stretch.
#'
#' @examples
#' \donttest{
#' c.im<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#'
#' #list of cameras for circular mask:
#' list.cameras
#'
#' #set the circular mask automatically:
#' import_fisheye(c.im,circ.mask=list(xc=1136,yc=850,rc=754),channel='B',gamma=2.2,display=TRUE)
#' f.im<-system.file('extdata/fullframe_D90_Nikkor-10.5_beech.jpg',package='hemispheR')
#' #set the circular mask using camera_fisheye():
#' import_fisheye(c.im,circ.mask=camera_fisheye('Coolpix4500+FC-E8'), gamma=2.2)
#'
#' #automatic calculating circular mask:
#' import_fisheye(c.im,channel='B',gamma=2.2,display=TRUE)
#'
#' #import a fullframe image:
#' import_fisheye(f.im,circular=FALSE,channel='B',gamma=2.2,display=TRUE)
#' }
#' @export

import_fisheye<-function(filename,channel=3, circ.mask=NULL,circular=TRUE,gamma=2.2, stretch=FALSE, display=FALSE,message=TRUE){

mxk <- suppressWarnings({

  if(!is.numeric(gamma)){
    warning('Wrong gamma input. Gamma is assumed as 1 (no correction applied)')
    gamma=1
  }

# oldw <- getOption("warn")
# options(warn = -1)

  channel.list=base::sort(c('first','GLA','Luma','2BG','BtoRG','B','GEI','RGB'))

  if(!is.numeric(channel)&length(setdiff(channel, channel.list)) > 0 & is.null(channel)){
    stop(cat('Unknown channel method. You can select a number corresponding to a image channel or:\n"',sep='',
             paste0(channel.list,collapse='",\n"'),'".'))
  }

  if (is.numeric(channel)){
    nly<-raster::nlayers(raster::stack(filename))
    if (nly< channel){
      stop(paste0('The selected channel number: ', channel, ' exceed the number of image channels (',nly  ,'). Please correct.'))
    }
    if(nly>= channel){
      img.values <- raster::values(raster::raster(x = filename, band = channel))
      if (is.numeric(gamma) & gamma !=1){
        minm<-min(img.values,na.rm=TRUE)
        maxm<-max(img.values,na.rm=TRUE)
        img.values=(maxm-minm)*((img.values/(maxm-minm))^gamma)
      }
    }
  }

  if (!is.numeric(channel) & channel=='first') {
    img.values <- raster::values(raster::raster(x = filename, band = 1))
    channel=1

    if (is.numeric(gamma) & gamma !=1){
      minm<-min(img.values,na.rm=TRUE)
      maxm<-max(img.values,na.rm=TRUE)
      img.values=(maxm-minm)*((img.values/(maxm-minm))^gamma)
    }
  }

  if (!is.numeric(channel) & channel!='first') {
    imgB <- raster::raster(x = filename, band = 3)
    imgG <- raster::raster(x = filename, band = 2)
    imgR <- raster::raster(x = filename, band = 1)

    if (is.numeric(gamma) & gamma !=1){
      img.valuesB<-raster::values(imgB)
      img.valuesG<-raster::values(imgG)
      img.valuesR<-raster::values(imgR)

      minmB<-min(img.valuesB,na.rm=TRUE)
      maxmB<-max(img.valuesB,na.rm=TRUE)
      img.valuesB=(maxmB-minmB)*((img.valuesB/(maxmB-minmB))^gamma)

      minmG<-min(img.valuesG,na.rm=TRUE)
      maxmG<-max(img.valuesG,na.rm=TRUE)
      img.valuesG=(maxmG-minmG)*((img.valuesG/(maxmG-minmG))^gamma)

      minmR<-min(img.valuesR,na.rm=TRUE)
      maxmR<-max(img.valuesR,na.rm=TRUE)
      img.valuesR=(maxmR-minmR)*((img.valuesR/(maxmR-minmR))^gamma)

      raster::values(imgB)<-img.valuesB
      raster::values(imgG)<-img.valuesG
      raster::values(imgR)<-img.valuesR
    }


    if (channel=='Luma') {
      img.values <- (0.3* raster::values(imgR) + 0.59 * raster::values(imgG) +0.11 * raster::values(imgB))
    }

    if (channel=='GLA') {
      img.values <- -((-1 * raster::values(imgR) + 2 * raster::values(imgG) - 1 * raster::values(imgB)) / (1 * raster::values(imgR) + 2 * raster::values(imgG) + 1 * raster::values(imgB)))

    }


    if (channel=='2BG') {
      img.values <- -1 * raster::values(imgG) + 2 * raster::values(imgB)

    }

    if (channel == "BtoRG") {
      img.values <- raster::values(imgB) * (1 + (2*raster::values(imgB) - raster::values(imgB) - raster::values(imgR)) / (raster::values(imgR) + 2 * raster::values(imgB) + raster::values(imgG)) + raster::values(imgB))/2
      # length(img.values)
    }

    if (channel == "B") {
      img.values <- raster::values(imgB)
      channel=3
    }

    if (channel == "GEI") {
      img.values <-- (-1 * raster::values(imgR) + 2 * raster::values(imgG) - 1 * raster::values(imgB))
    }

    if (channel == "RGB") {
      img.values <- (1 * raster::values(imgR) + 1 * raster::values(imgG) + 1 * raster::values(imgB))/3

    }
  }

  #fill NAs due to channel mixing (important):
  img.values[is.na(img.values)] <- 0



  #create the target raster and fill with values:
  img<-raster::raster(filename,channel=1)
  raster::values(img)<-img.values

  if (stretch==TRUE){
    img<-raster::stretch(img,minv=0,maxv=255,minq=0.01,maxq=0.99)
  }

  if (stretch==FALSE){
    raster::values(img)<-scales::rescale(raster::values(img),c(0,255))#needed for auto_thresh later
  }

  base::names(img) <- basename(filename)
  img@data@attributes$channel<-channel
  img@data@attributes$stretch<-stretch
  img@data@attributes$gamma<-gamma
  base::names(img) <- basename(filename)

  if(!is.null(circ.mask)& length(circ.mask)<3){
    stop('Check circ.mask entry. It should contains 3 parameters: xc,yc,rc')
  }

  if(!is.null(circ.mask)&circular==TRUE){
    xc=circ.mask$xc
    yc=circ.mask$yc
    rc=circ.mask$rc
    if(message==TRUE){
      message(paste0('It is a circular fisheye, where xc, yc and radius are ',xc,', ',yc,', ',rc))
    }}

  if(is.null(circ.mask)&circular==TRUE){
    xc=raster::extent(img)[2]/2
    yc=raster::extent(img)[4]/2
    rc=min(xc,yc)-2
    if(message==TRUE){
      message(paste0('It is a circular fisheye, where xc, yc and radius are ',xc,', ',yc,', ',rc))
    }}

  if(is.null(circ.mask)&circular==FALSE){
    xc=raster::extent(img)[2]/2
    yc=raster::extent(img)[4]/2
    rc=round(sqrt(xc^2+yc^2))-2
    if(message==TRUE){
      message(paste0('It is a fullframe fisheye, where xc, yc and radius are ',xc,', ',yc,', ',rc))
    }}
  if(!is.null(circ.mask)&circular==FALSE){
    xc=circ.mask$xc
    yc=circ.mask$yc
    rc=circ.mask$rc
    if(message==TRUE){
      message(paste0('It is a fullframe fisheye, where xc, yc and radius are ',xc,', ',yc,', ',rc))
    }}
  circular.mask = (raster::rasterToPoints(img)[,1] - xc)^2 + (raster::rasterToPoints(img)[,2] - yc)^2 <= rc^2

  if (display==TRUE){
    raster::plot(img,col=gray.colors(55), main='circular mask')
    plotrix::draw.circle(xc,yc,rc,border='red',lwd=3)
    if (circular==TRUE){
    graphics::segments(xc,yc,xc+rc,yc,col='red',lwd=3)}
    if (circular==FALSE){
      graphics::segments(xc,yc,raster::extent(img)[2],raster::extent(img)[4],col='red',lwd=3)}
  }

  if((circular==TRUE) | (circular==FALSE & !is.null(circ.mask))) {
    raster::values(img)[!circular.mask] <- NA
  }

})
  # options(warn = oldw)
  return(img)
}

