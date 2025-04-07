#' @name binarize_fisheye
#' @aliases binarize_fisheye
#' @title Compute the threshold of a single-channel fisheye image, and return a binary fisheye image of canopy (0) and gap (1) pixels
#' @param img SpatRaster. A single layer fisheye image imported by [import_fisheye()] using the [terra::rast()] functionality.
#' @param method Character. The method used to threshold the image, using the [autothresholdr::auto_thresh()] function. For details, see <https://imagej.net/plugins/auto-threshold>. Default = 'Otsu'.
#' @param zonal Logical. If is set to TRUE, it divides the images in four (N, W, S, E) regions and classify each region separately. Useful in case of uneven illumination condition in the image.
#' @param manual Numeric. It uses a manual thresholding instead of automatic one. If selected, it overrides automatic thresholding.
#' @param display Logical. If is set to TRUE, it plots the classified binary image. Default to FALSE.
#' @param export Logical. If is set to TRUE, it saves the binary fisheye image as tif file. Default to FALSE.
#'
#' @returns A binary single-layer image (SpatRaster)
#' @importFrom terra rast nlyr ext values classify merge plot metags
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


binarize_fisheye <- function(img,
                             method='Otsu',
                             zonal=FALSE,
                             manual=NULL,
                             display=FALSE,
                             export=FALSE) {


  if (terra::nlyr(img) > 1) {
    stop("Error: please select a single channel image using the import_fisheye() function")}


  if(zonal==TRUE & !is.null(manual)){
    stop('You selected zonal analysis AND manual thresholding: please decide!')
  }

  if(zonal==TRUE & is.null(manual)){

    spmk<-zonal_mask(img)

    img.s1<-terra::rast(matrix(spmk[,,1],nrow = nrow(img),byrow=T))
    img.s2<-terra::rast(matrix(spmk[,,2],nrow = nrow(img),byrow=T))
    img.s3<-terra::rast(matrix(spmk[,,3],nrow = nrow(img),byrow=T))
    img.s4<-terra::rast(matrix(spmk[,,4],nrow = nrow(img),byrow=T))

    img.mat1 <- matrix(terra::values(img.s1),nrow = nrow(img.s1),byrow=T)
    th1 <- autothresholdr::auto_thresh(round(img.mat1), method=method,ignore_na=TRUE)
    th1 <- unlist(th1)
    img.bw1 <- terra::classify(img.s1, rbind(c(-Inf,th1,0),c( th1,Inf,1)))

    img.mat2 <- matrix(terra::values(img.s2),nrow = nrow(img.s2),byrow=T)
    th2 <- autothresholdr::auto_thresh(round(img.mat2), method=method,ignore_na=TRUE)
    th2 <- unlist(th2)
    img.bw2 <- terra::classify(img.s2, rbind(c(-Inf,th2,0),c( th2,Inf,1)))

    img.mat3 <- matrix(terra::values(img.s3),nrow = nrow(img.s4),byrow=T)
    th3 <- autothresholdr::auto_thresh(round(img.mat3), method=method,ignore_na=TRUE)
    th3 <- unlist(th3)
    img.bw3 <- terra::classify(img.s3, rbind(c(-Inf,th3,0),c( th3,Inf,1)))

    img.mat4 <- matrix(terra::values(img.s4),nrow = nrow(img.s4),byrow=T)
    th4 <- autothresholdr::auto_thresh(round(img.mat4), method=method,ignore_na=TRUE)
    th4 <- unlist(th4)
    img.bw4 <- terra::classify(img.s4, rbind(c(-Inf,th4,0),c( th4,Inf,1)))

    th=c(as.numeric(th1),as.numeric(th2),as.numeric(th3),as.numeric(th4))

    img.bw<-terra::merge(img.bw1,img.bw2,img.bw3,img.bw4)
    base::names(img.bw) <- base::names(img)
  }

  if (zonal==FALSE & is.null(manual)){
    img.mat <- matrix(terra::values(img),nrow = nrow(img),byrow=T)
    th <- autothresholdr::auto_thresh(round(img.mat), method=method,ignore_na=TRUE)
    th <- unlist(th)
    img.bw <- terra::classify(img, rbind(c(-Inf,th,0),c( th,Inf,1)))
    base::names(img.bw) <- base::names(img)
  }

  if (!is.null(manual)){
    method='manual'

    if(!is.numeric(manual)){
      stop('Provide a numeric manual value to threshold image or consider an automated thresholding')
    }
    if(is.numeric(manual)& (manual>max(terra::values(img),na.rm=TRUE)|manual<min(terra::values(img),na.rm=TRUE))){
      stop('The manual value provided is outside the numeric range of the image values.')
    }
    img.mat <- matrix(terra::values(img),nrow = nrow(img),byrow=T)
    th=manual
    img.bw <- terra::classify(img, rbind(c(-Inf,th,0),c( th,Inf,1)))
    base::names(img.bw) <- base::names(img)
  }

  if (display==TRUE){
    terra::plot(img.bw,col=c('black','white'),legend=FALSE,
                 main=ifelse(length(th)>1,
                             paste0('multi-thresholding values:\n',paste0(th,collapse=',')),
                             paste0('single-thresholding value:\n',th)))
  }





  tags <- tryCatch(
    terra::metags(img),
    error = function(e) {
      message("Unable to retrieve metadata using terra::metags(). This may be due to a change in the terra package or the image format.")
      return(NULL)
    }
  )

  if (is.null(tags)) {
    message("Metadata is not available or could not be read from the image. Please ensure you are using a supported version of the 'terra' package or a valid image file.")
  } else {

    terra::metags(img.bw) <- terra::metags(img)
    terra::metags(img.bw)<-c(zonal=zonal, thd=ifelse(length(th)>1,paste0(th,collapse='_'),th), method=method)

  }



  if (export == TRUE) {
    dir.create(base::file.path(base::getwd(), "results"),showWarnings = FALSE)
    jpeg::writeJPEG(matrix(terra::values(img.bw), nrow = nrow(img.bw), byrow=T),target=base::paste0(base::file.path(base::getwd()), '/results/class_',base::names(img.bw)))
  }

  mk<- is.na(img.bw)
  img.bw[mk]<-NA
  return(img.bw)
}


