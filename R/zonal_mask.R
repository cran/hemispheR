#' @name zonal_mask
#' @aliases zonal_mask
#' @title Divide a raster image into four stacks which are used as masks.
#' @param img Raster. The input raster image
#' @return A 4-layers stacks of image masks
#' @importFrom raster extent rasterize stack
#' @importFrom sp Polygon SpatialPolygons
#'
#' @description
#' This function imports a raster image using [raster::raster()] functionality, and divide into four masks, using the image centre and borders as vertices.
#' The four zonal masks are then returned as a raster stack.
#'
#' @examples
#' image<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#' zmsk<-zonal_mask(raster::raster(image))
#' raster::plot(zmsk,col=gray.colors(5),main=c('N','W','S','E'))
#'
#' @export

zonal_mask<-function(img){

mxk <- suppressWarnings({
  # oldw <- getOption("warn")
  # options(warn = -1)

  xc=raster::extent(img)[2]/2
  yc=raster::extent(img)[4]/2

  # Sr1<-sp::Polygon(rbind(c(xc,yc),c(max(raster::extent(img)[2]),yc),c(max(raster::extent(img)[2]),max(raster::extent(img)[4])),c(xc,max(raster::extent(img)[4]))))
  Sr1<-sp::Polygon(rbind(c(0,max(raster::extent(img)[4])),c(xc,yc),c(max(raster::extent(img)[2]),max(raster::extent(img)[4])),c(0,max(raster::extent(img)[4]))))
  Srs1=sp::Polygons(list(Sr1),'s1')
  SpP1=sp::SpatialPolygons(list(Srs1))

  # Sr2<-sp::Polygon(rbind(c(xc,yc),c(max(raster::extent(img)[2]),yc),c(max(raster::extent(img)[2]),0),c(xc,0)))
  Sr2<-sp::Polygon(rbind(c(0,max(raster::extent(img)[4])),c(0,0),c(xc,yc),c(0,max(raster::extent(img)[4]))))
  Srs2=sp::Polygons(list(Sr2),'s2')
  SpP2=sp::SpatialPolygons(list(Srs2))

  # Sr3<-sp::Polygon(rbind(c(xc,yc),c(xc,0),c(0,0),c(0,yc)))
  Sr3<-sp::Polygon(rbind(c(0,0),c(xc,yc),c(raster::extent(img)[2],0),c(0,0)))
  Srs3=sp::Polygons(list(Sr3),'s3')
  SpP3=sp::SpatialPolygons(list(Srs3))


  # Sr4<-sp::Polygon(rbind(c(xc,yc),c(0,yc),c(0,max(raster::extent(img)[4])),c(xc,max(raster::extent(img)[4]))))
  Sr4<-sp::Polygon(rbind(c(raster::extent(img)[2],0),c(raster::extent(img)[2],raster::extent(img)[4]),c(xc,yc),c(raster::extent(img)[2],0)))
  Srs4=sp::Polygons(list(Sr4),'s4')
  SpP4=sp::SpatialPolygons(list(Srs4))


  Rst1<-raster::rasterize(SpP1,img,mask=T)
  Rst2<-raster::rasterize(SpP2,img,mask=T)
  Rst3<-raster::rasterize(SpP3,img,mask=T)
  Rst4<-raster::rasterize(SpP4,img,mask=T)

  # options(warn = oldw)
})
  return(raster::stack(Rst1,Rst2,Rst3,Rst4))
}
