#' @name binarize_fisheye
#' @aliases binarize_fisheye
#' @title Compute the threshold of a single-channel fisheye image, and return a binary fisheye image of canopy (0) and gap (1) pixels
#' @param img Raster. Fisheye image imported by [import_fisheye()] as a single-band raster using the [raster::raster()] functionality.
#' @param method Character. The method used to threshold the image, using the [autothresholdr::auto_thresh()] function. For details, see <https://imagej.net/plugins/auto-threshold>. Default = 'Otsu'.
#' @param zonal Logical. If is set to TRUE, it divides the images in four (N, W, S, E) regions and classify each region separately. Useful in case of uneven illumination condition in the image.
#' @param manual Numeric. It uses a manual thresholding instead of automatic one. If selected, it overrides automatic thresholding.
#' @param display Logical. If is set to TRUE, it plots the classified binary image. Default to FALSE.
#' @param export Logical. If is set to TRUE, it saves the binary fisheye image as tif file. Default to FALSE.
#'
#' @returns A binary raster image
#' @importFrom raster crs raster stack bandnr getValues reclassify extent rasterToPoints nlayers writeRaster merge
#' @importFrom autothresholdr auto_thresh
#'
#' @seealso
#' <https://imagej.net/plugins/auto-threshold>
#'
#' @description
#' The function calculates a single threshold of a single-channel raster image using  the [autothresholdr::auto_thresh()] functionality.
#' The single thresholding is also applied at sub-image level if zonal=TRUE.
#' The available methods are described at <https://imagej.net/plugins/auto-threshold>.
#' The thresholding value is then used to make a binary raster image of canopy (0) and gap (1) pixels.
#'
#' @examples
#' \donttest{
#' c.im<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#'
#' c.im |>
#' import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'  binarize_fisheye(display=TRUE)
#'
#' #zonal thresholding:
#' c.im |>
#' import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'  binarize_fisheye(zonal=TRUE,display=TRUE)
#'
#' #manual thresholding:
#' c.im |>
#' import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#' binarize_fisheye(manual=55,display=TRUE)
#' }
#' @export


binarize_fisheye<-function(img, method='Otsu', zonal=FALSE, manual=NULL, display=FALSE, export=FALSE) {
  mystack <- raster::stack(img)

  if (raster::nlayers(mystack) > 1) {
    stop("Error: please select a single channel image using the import_fisheye() function")}

  #calculate back xc,yc,rc:
  xc=raster::extent(img)[2]/2
  yc=raster::extent(img)[4]/2
  if(length(which(is.na(raster::getValues(img))))==0){
    rc=round(sqrt(xc^2+yc^2))-2
  }

  if(length(which(is.na(raster::getValues(img))))>0){
    rc<-round(max(sqrt((raster::rasterToPoints(img)[,1] - xc)^2 + (raster::rasterToPoints(img)[,2] - yc)^2)))
  }

  if(zonal==TRUE & !is.null(manual)){
    stop('You selected zonal analysis AND manual thresholding: please decide!')
  }

  if(zonal==TRUE & is.null(manual)){

    spmk<-zonal_mask(img)

    img.s1<-raster::raster(spmk,layer=1)
    img.s2<-raster::raster(spmk,layer=2)
    img.s3<-raster::raster(spmk,layer=3)
    img.s4<-raster::raster(spmk,layer=4)

    img.mat1 <- as.matrix(raster::getValues(img.s1,format='matrix'))
    th1 <- autothresholdr::auto_thresh(round(img.mat1), method=method,ignore_na=TRUE)
    th1 <- unlist(th1)
    img.bw1 <- raster::reclassify(img.s1, c(-Inf, th1, 0, th1, Inf, 1))

    img.mat2 <- as.matrix(raster::getValues(img.s2,format='matrix'))
    th2 <- autothresholdr::auto_thresh(round(img.mat2), method=method,ignore_na=TRUE)
    th2 <- unlist(th2)
    img.bw2 <- raster::reclassify(img.s2, c(-Inf, th2, 0, th2, Inf, 1))

    img.mat3 <- as.matrix(raster::getValues(img.s3,format='matrix'))
    th3 <- autothresholdr::auto_thresh(round(img.mat3), method=method,ignore_na=TRUE)
    th3 <- unlist(th3)
    img.bw3 <- raster::reclassify(img.s3, c(-Inf, th3, 0, th3, Inf, 1))

    img.mat4 <- as.matrix(raster::getValues(img.s4,format='matrix'))
    th4 <- autothresholdr::auto_thresh(round(img.mat4), method=method,ignore_na=TRUE)
    th4 <- unlist(th4)
    img.bw4 <- raster::reclassify(img.s4, c(-Inf, th4, 0, th4, Inf, 1))

    th=c(th1,th2,th3,th4)

    img.bw<-raster::merge(img.bw1,img.bw2,img.bw3,img.bw4)
    base::names(img.bw) <- base::names(img)
  }

  if (zonal==FALSE & is.null(manual)){
    img.mat <- as.matrix(raster::getValues(img,format='matrix'))
    th <- autothresholdr::auto_thresh(round(img.mat), method=method,ignore_na=TRUE)
    th <- unlist(th)
    img.bw <- raster::reclassify(img, c(-Inf, th, 0, th, Inf, 1))
    base::names(img.bw) <- base::names(img)
  }

  if (!is.null(manual)){
    method='manual'

    if(!is.numeric(manual)){
      stop('Provide a numeric manual value to threshold image or consider an automated thresholding')
    }
    if(is.numeric(manual)& (manual>max(raster::getValues(img),na.rm=TRUE)|manual<min(raster::getValues(img),na.rm=TRUE))){
      stop('The manual value provided is outside the numeric range of the image values.')
    }
    img.mat <- as.matrix(raster::getValues(img,format='matrix'))
    th=manual
    img.bw <- raster::reclassify(img, c(-Inf, th, 0, th, Inf, 1))
    base::names(img.bw) <- base::names(img)
  }

  if (display==TRUE){
    raster::plot(img.bw,col=c('black','white'),legend=FALSE,
                 main=ifelse(length(th)>1,
                             paste0('multi-thresholding values:\n',paste0(th,collapse=',')),
                             paste0('single-thresholding value:\n',th)))
  }

  img.bw@data@attributes$channel<-img@data@attributes$channel
  img.bw@data@attributes$stretch<-img@data@attributes$stretch
  img.bw@data@attributes$gamma<-img@data@attributes$gamma
  img.bw@data@attributes$zonal<-zonal
  img.bw@data@attributes$thd<-ifelse(length(th)>1,paste0(th,collapse='_'),th)
  img.bw@data@attributes$method<-method

  if (export == TRUE) {
    dir.create(base::file.path(base::getwd(), "results"),showWarnings = FALSE)
    jpeg::writeJPEG(as.matrix(raster::getValues(img.bw,format='matrix')),target=base::paste0(base::file.path(base::getwd()), '/results/class_',base::names(img.bw)))
    # raster::writeRaster(img.bw,base::paste0(base::file.path(base::getwd()),"/results/class_", base::names(img.bw),'.tif'),overwrite=TRUE)
    }

  return(img.bw)
}


