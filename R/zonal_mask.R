#' @name zonal_mask
#' @aliases zonal_mask
#' @title Divide a raster image into four stacks which are used as masks.
#' @param img SpatRaster. The input single layer image generated from [terra::rast()].
#' @return A 4-layers stacks of image masks
#' @importFrom terra nlyr ext mask vect varnames
#' @importFrom sf st_polygon
#'
#' @description
#' This function imports a SpatRaster image using [terra::rast()] functionality, and divide into four masks, using the image centre and borders as vertices.
#' The four zonal masks are then returned as a RasterStack.
#'
#' @examples
#' image<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#' zmsk<-zonal_mask(terra::rast(image, lyrs=3))
#' terra::plot(zmsk,col=gray.colors(5),main=c('N','W','S','E'))
#'
#' @export

zonal_mask<-function(img){

suppressWarnings(
  if(terra::nlyr(img)>1)
    stop('The zonal masking function consider only 1-layer SpatRaster')
)

  xc=terra::ext(img)[2]/2
  yc=terra::ext(img)[4]/2

  Sr1 <- sf::st_polygon(list(cbind(rbind(c(0,max(terra::ext(img)[4])),c(xc,yc),c(max(terra::ext(img)[2]),max(terra::ext(img)[4])),c(0,max(terra::ext(img)[4]))))))
  Rst1<-terra::mask(img, terra::vect(Sr1))
  terra::varnames(Rst1) <- 'N'

  Sr2<-sf::st_polygon(list(cbind(rbind(c(0,max(terra::ext(img)[4])),c(0,0),c(xc,yc),c(0,max(terra::ext(img)[4]))))))
  Rst2<-terra::mask(img, terra::vect(Sr2))
  terra::varnames(Rst2) <- 'W'

  Sr3<-sf::st_polygon(list(cbind(rbind(c(0,0),c(xc,yc),c(terra::ext(img)[2],0),c(0,0)))))
  Rst3<-terra::mask(img, terra::vect(Sr3))
  terra::varnames(Rst3) <- 'S'

  Sr4<-sf::st_polygon(list(cbind(rbind(c(terra::ext(img)[2],0),c(terra::ext(img)[2],terra::ext(img)[4]),c(xc,yc),c(terra::ext(img)[2],0)))))
  Rst4<-terra::mask(img, terra::vect(Sr4))
  terra::varnames(Rst4) <- 'E'

out.rst <- c(Rst1, Rst2, Rst3, Rst4)
  return(out.rst)
}
