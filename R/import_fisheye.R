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
#' @return A single-channel image (SpatRaster).
#' @importFrom terra nlyr rast values stretch ext xyFromCell ncell plot crs metags
#' @importFrom dismo circles
#' @importFrom graphics segments
#' @importFrom grDevices gray.colors
#' @importFrom scales rescale
#'
#' @description
#' This function imports fisheye images using [terra::rast()] functionality, by selecting a single channel, or a combination of channels.
#' The default option (blue channel) is generally preferred for canopy image analysis as it enables high contrast between canopy and sky pixels, which ease image thresholding.
#' A circular mask is then applied to mask outside pixel in case of circular fisheye images. It can be manually inserted, or retrieved using the [camera_fisheye()] function.
#' Alternatively, it is automatically calculated. Additional functions include a gamma correction and a contrast stretch.
#'
#' @examples
#' \donttest{
#' c.im<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#'
#' #set the circular mask automatically:
#' import_fisheye(c.im,circ.mask=list(xc=1136,yc=850,rc=754),channel='B',gamma=2.2,display=TRUE)
#'
#' #list of cameras for circular mask:
#' list.cameras
#'
#' #set the circular mask using camera_fisheye():
#' import_fisheye(c.im,circ.mask=camera_fisheye('Coolpix4500+FC-E8'), gamma=2.2)
#'
#' #automatic calculating circular mask:
#' import_fisheye(c.im,channel='B',gamma=2.2,display=TRUE)
#'
#' #import a fullframe image:
#' f.im<-system.file('extdata/fullframe_D90_Nikkor-10.5_beech.jpg',package='hemispheR')
#' import_fisheye(f.im,circular=FALSE,channel='B',gamma=2.2,display=TRUE)
#' }
#' @export

import_fisheye <- function(filename, channel=3, circ.mask=NULL,circular=TRUE,gamma=2.2, stretch=FALSE, display=FALSE,message=TRUE){



  if(!is.numeric(gamma)){
    warning('Wrong gamma input. Gamma is assumed as 1 (no correction applied)')
    gamma=1
  }

  # oldw <- getOption("warn")
  # options(warn = -1)

  channel.list=base::sort(c('first','GLA','Luma','2BG','BtoRG','B','GEI','RGB'))

  if(!is.numeric(channel) & length(setdiff(channel,channel.list)) > 0 & !is.null(channel)){
    stop(cat('Unknown channel method. You can select a number corresponding to a image channel or:\n"',sep='',
             paste0(channel.list,collapse='",\n"'),'".'))
  }

  mxk <- suppressWarnings({
    if (is.numeric(channel)){
      nly<-terra::nlyr(terra::rast(filename))
      if (nly< channel){
        stop(paste0('The selected channel number: ', channel, ' exceed the number of image channels (',nly  ,'). Please correct.'))
      }
      if(nly>= channel){
        img.values <- terra::values(terra::rast(x = filename, lyr = channel))
        if (is.numeric(gamma) & gamma !=1){
          minm<-min(img.values,na.rm=TRUE)
          maxm<-max(img.values,na.rm=TRUE)
          img.values=(maxm-minm)*((img.values/(maxm-minm))^gamma)
        }
      }
    }

    if (!is.numeric(channel) & channel=='first') {
      img.values <- terra::values(terra::rast(x = filename, lyr = 1))
      channel=1

      if (is.numeric(gamma) & gamma !=1){
        minm<-min(img.values,na.rm=TRUE)
        maxm<-max(img.values,na.rm=TRUE)
        img.values=(maxm-minm)*((img.values/(maxm-minm))^gamma)
      }
    }

    if (!is.numeric(channel) & channel!='first') {
      imgB <- terra::rast(x = filename, lyr = 3)
      imgG <- terra::rast(x = filename, lyr = 2)
      imgR <- terra::rast(x = filename, lyr = 1)

      if (is.numeric(gamma) & gamma !=1){
        img.valuesB<-terra::values(imgB)
        img.valuesG<-terra::values(imgG)
        img.valuesR<-terra::values(imgR)

        minmB<-min(img.valuesB,na.rm=TRUE)
        maxmB<-max(img.valuesB,na.rm=TRUE)
        img.valuesB=(maxmB-minmB)*((img.valuesB/(maxmB-minmB))^gamma)

        minmG<-min(img.valuesG,na.rm=TRUE)
        maxmG<-max(img.valuesG,na.rm=TRUE)
        img.valuesG=(maxmG-minmG)*((img.valuesG/(maxmG-minmG))^gamma)

        minmR<-min(img.valuesR,na.rm=TRUE)
        maxmR<-max(img.valuesR,na.rm=TRUE)
        img.valuesR=(maxmR-minmR)*((img.valuesR/(maxmR-minmR))^gamma)

        terra::values(imgB)<-img.valuesB
        terra::values(imgG)<-img.valuesG
        terra::values(imgR)<-img.valuesR
      }


      if (channel=='Luma') {
        img.values <- (0.3* terra::values(imgR) + 0.59 * terra::values(imgG) +0.11 * terra::values(imgB))
      }

      if (channel=='GLA') {
        img.values <- -((-1 * terra::values(imgR) + 2 * terra::values(imgG) - 1 * terra::values(imgB)) / (1 * terra::values(imgR) + 2 * terra::values(imgG) + 1 * terra::values(imgB)))

      }


      if (channel=='2BG') {
        img.values <- -1 * terra::values(imgG) + 2 * terra::values(imgB)

      }

      if (channel == "BtoRG") {
        img.values <- terra::values(imgB) * (1 + (2*terra::values(imgB) - terra::values(imgB) - terra::values(imgR)) / (terra::values(imgR) + 2 * terra::values(imgB) + terra::values(imgG)) + terra::values(imgB))/2
        # length(img.values)
      }

      if (channel == "B") {
        img.values <- terra::values(imgB)
        channel=3
      }

      if (channel == "GEI") {
        img.values <-- (-1 * terra::values(imgR) + 2 * terra::values(imgG) - 1 * terra::values(imgB))
      }

      if (channel == "RGB") {
        img.values <- (1 * terra::values(imgR) + 1 * terra::values(imgG) + 1 * terra::values(imgB))/3

      }
    }

    #fill NAs due to channel mixing (important):
    img.values[is.na(img.values)] <- 0



    #create the target raster and fill with values:
    img<-terra::rast(filename, lyr=1)
    terra::values(img)<-img.values

    if (stretch==TRUE){
      img<-terra::stretch(img,minv=0,maxv=255,minq=0.01,maxq=0.99)
    }

    if (stretch==FALSE){
      terra::values(img)<-scales::rescale(terra::values(img),c(0,255))#needed for auto_thresh later
    }

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
      xc=terra::ext(img)[2]/2
      yc=terra::ext(img)[4]/2
      rc=min(xc,yc)-2
      if(message==TRUE){
        message(paste0('It is a circular fisheye, where xc, yc and radius are ',xc,', ',yc,', ',rc))
      }}

    if(is.null(circ.mask)&circular==FALSE){
      xc=terra::ext(img)[2]/2
      yc=terra::ext(img)[4]/2
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

    #create the circular mask
    xy <- terra::xyFromCell(img,1:terra::ncell(img))
    circular.mask = (xy[,1] - xc)^2 + (xy[,2] - yc)^2 <= rc^2

    if(!is.null(circ.mask) & (xc + rc > terra::ext(img)[2] | rc > xc)){
      message('The applied xc+rc parameters fall outside the image area.
              It will affect subsequent mask calculations.')
    }

    if(!is.null(circ.mask) & (yc + rc > terra::ext(img)[4] | rc > yc)){
      message('The applied yc+rc parameters fall outside the image area.
              It will affect subsequent mask calculations.')
    }

    # xc=terra::ext(img)[2]/2
    # yc=terra::ext(img)[4]/2

    if (display==TRUE){
      terra::crs(img) <-NULL
      terra::ext(img) <- terra::ext(terra::rast(x=filename))
      terra::plot(img,col=gray.colors(55), main='circular mask')



      if (circular==TRUE){
        dd <- dismo::circles(cbind(xc,yc), rc, lonlat=F)
        plot(dd,  add=TRUE, border='red', lwd=3)
        graphics::segments(xc,yc,xc+rc,yc,col='red',lwd=3)
        }
      if (circular==FALSE){
        graphics::segments(xc,yc,terra::ext(img)[2],terra::ext(img)[4],col='red',lwd=3)
        }
    }

    if((circular==TRUE) | (circular==FALSE & !is.null(circ.mask))) {
      terra::values(img)[!circular.mask] <- NA
    }

  })

  # options(warn = oldw)
  terra::values(img) <- round(terra::values(img))

  terra::metags(img) <- cbind(c('channel','stretch','gamma'),
                              c(channel, as.character(stretch), gamma))


  return(img)
}


