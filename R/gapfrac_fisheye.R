#' @name gapfrac_fisheye
#' @aliases gapfrac_fisheye list.lenses
#' @title Derive angular gap fraction from a classified fisheye image
#' @importFrom raster raster getValues rasterToPoints
#' @importFrom dplyr group_by nth right_join summarise mutate relocate ungroup select filter case_when bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom graphics segments
#' @importFrom plotrix draw.circle
#'
#' @param img.bw Raster. A binary fisheye image generated from [binarize_fisheye()].
#' @param maxVZA Numeric. The maximum Zenith angle (in degrees) corresponding to the image radius. Default= 90.
#' @param lens Character. The lens type for fisheye-lens correction. A list of lenses is available by typing *list.lenses*. If missing, it is assumed equidistant.
#' @param startVZA Numeric. The minimum Zenith angle (in degrees) considered in the analysis. Default is 0.
#' @param endVZA Numeric. The maximum Zenith angle (in degrees) considered in the analysis. Default is 70.
#' @param nrings Numeric. The number of equiangular zenith rings considered in the analysis. Default is 7.
#' @param nseg Numeric. The number of azimuth segments considered in the analysis. Default is 8.
#' @param display Logical. If set to TRUE, it desplays the zenith rings and azimuth segments overlaid on the fisheye image.
#' @param message Logical. If set to TRUE, it reports the circular mask used in the analysis.
#'
#' @return A dataframe of gap fraction (GF) for zenith rings (rows) and azimuth segments (columns).
#'
#' @description
#' The function calculates the gap fraction for a number of zenith annuli (rings) and azimuth sectors (segments).
#' A list of lens is available for correcting for lens distorsion. Type 'list.lenses'.
#'
#' @author
#' Francesco Chianucci
#'
#' @seealso
#' Lens correction functions have been retrieved from the following sources:
#'
#' Pekin and Macfarlane 2009: \doi{10.3390/rs1041298}
#'
#' Paul Bourke:<http://www.paulbourke.net/dome/fisheyecorrect/>
#'
#' Hemisfer: <https://www.schleppi.ch/patrick/hemisfer/index.php>
#'
#' @examples
#' \donttest{c.im<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#'
#' #List of lenses for fisheye projection correction:
#' list.lenses
#'
#' #Zenith rings similar to LAI-2000/2200:
#' c.im |>
#'  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'   binarize_fisheye() |>
#'   gapfrac_fisheye(lens='FC-E8',nrings=5,nseg=8,endVZA=75,display=TRUE)
#'
#' #The hinge angle method close to 1 radian (57):
#' c.im |>
#'  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'   binarize_fisheye() |>
#'   gapfrac_fisheye(lens='FC-E8',nrings=1,nseg=8,startVZA=55,endVZA=60,display=TRUE)
#' }
#'
#'
#' @export

gapfrac_fisheye<-function(img.bw,maxVZA=90,lens='equidistant',startVZA=0,endVZA=70,nrings=7,nseg=8,message=FALSE,display=FALSE){

  if (length(setdiff(unique(raster::getValues(img.bw)),
                     c(NA,0, 1))) > 0)
    stop("Error: please select a binary (0,1) fisheye image, which can be classified using the binarize_fisheye() function")

  if (length(setdiff(c(0,1),unique(raster::getValues(img.bw))))>0)
    stop("Error: only 0 or 1 is contained in the binary image, calculation of gap fraction is not possible")



  metadata<-dplyr::bind_rows(unlist(img.bw@data@attributes))
  imgdf<-data.frame(raster::rasterToPoints(img.bw))
  base::names(imgdf)<-c('x','y','value')

  #calculate back xc,yc,rc:
  xc=round(mean(imgdf$x))
  yc=round(mean(imgdf$y))

  if(length(unique(raster::getValues(img.bw)))==2){
    rc=round(sqrt(xc^2+yc^2))-2
    if(message==TRUE){
      message(paste('Used parameters xc, yc and rc are',xc,yc,rc,sep=", "))
    }}

  if(length(unique(raster::getValues(img.bw)))==3){
    rc=round((max(imgdf$x)-min(imgdf$x))/2)
    if(message==TRUE){
      message(paste('Used parameters xc, yc and rc are',xc,yc,rc,sep=", "))
    }}

  imgdf$dx<-imgdf$x-mean(imgdf$x)
  imgdf$dy<-imgdf$y-mean(imgdf$y)
  imgdf$r<-round(sqrt(imgdf$dx^2+imgdf$dy^2))

  imgdf$theta=NA
  imgdf$theta[imgdf$dx>0 & imgdf$dy>=0] <- atan(imgdf$dy[imgdf$dx>0 & imgdf$dy>=0]/imgdf$dx[imgdf$dx>0 & imgdf$dy>=0])
  imgdf$theta[imgdf$dx>0 & imgdf$dy<0] <- atan(imgdf$dy[imgdf$dx>0 & imgdf$dy<0]/imgdf$dx[imgdf$dx>0 & imgdf$dy<0])+2*pi
  imgdf$theta[imgdf$dx<0] <- atan(imgdf$dy[imgdf$dx<0]/imgdf$dx[imgdf$dx<0])+pi
  imgdf$theta[imgdf$dx==0 & imgdf$dy>0] <- pi/2
  imgdf$theta[imgdf$dx==0 & imgdf$dy<0] <- pi*3/2
  imgdf$theta<-imgdf$theta*180/pi

  startVZA=startVZA
  endVZA=endVZA
  maxVZA=maxVZA

  # 9 zenith rings
  nrings=nrings

  list.lenses<-c("equidistant","orthographic","stereographic","equisolid", sort(c("FC-E8","FC-E9","Sigma-4.5","Nikkor-OP10","Nikkor-10.5","Sigma-8","Nikkor-8","Soligor","Raynox-CF185","CanonEF-8-15","Kodak-SP360","Entaniya-M12-280","Canon-RF-5.2","Entaniya-HAL-200","Entaniya-HAL-250","Entaniya-M12-250","Entaniya-M12-220","Meike-3.5","Meike-6.5","Laowa-4","iZugar-MKX200","iZugar-MKX22","iZugar-MKX13","iZugar-MKX19","DZO-VRCA","Sunex-DSL315","Sunex-DSL239","Sunex-DSL415","Sunex-DSLR01","Lensagon-BF10M14522S118","Lensagon-BF16M220D","Omnitech-ORIFL190-3","Aico-ACHIR01028B10M","Aico-ACHIR01420B9M","SMTEC-SL-190","Evetar-E3307","Evetar-E3267A","Evetar-E3279","ArduCam-M25156H18","Bosch-Flexidome-7000")))

  if (length(setdiff(lens, list.lenses)) > 0 | is.null(lens)){
    stop(cat('Unknown lens type. Allowed lenses are:\n"',sep='',paste0(list.lenses,collapse='",\n"'),'".'))
  }
  if(lens=="equidistant"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*VZAbins/maxVZA)
  }

  #From Pekin & Macfarlane 2009 (doi:10.3390/rs1041298). Note: it gives relative distance y based on relative distance theta:
  if (lens=="FC-E8") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(1.06*VZAbins/maxVZA+0.00498*(VZAbins/maxVZA)^2-0.0639*(VZAbins/maxVZA)^3))
  }

  #From Pekin & Macfarlane 2009. Note: it gives relative distance y based on relative distance theta:
  if (lens=="Nikkor-10.5") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(1.13*VZAbins/maxVZA+0.00798*(VZAbins/maxVZA)^2-0.138*(VZAbins/maxVZA)^3))
  }

  #From Pekin & Macfarlane 2009. Note: it gives relative distance y based on relative distance theta:
  if (lens=="Sigma-4.5") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(1.12*VZAbins/maxVZA+0.00598*(VZAbins/maxVZA)^2-0.178*(VZAbins/maxVZA)^3))
  }

  #From Hemisfer Lens file. Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="FC-E9") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6427*(VZAbins*pi/180)+0.0346*(VZAbins*pi/180)^2-0.024491*(VZAbins*pi/180)^3))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Sigma-8") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.75276*(VZAbins*pi/180)-0.073937*(VZAbins*pi/180)^2))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Nikkor-8") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.9192*(VZAbins*pi/180)-0.1792*(VZAbins*pi/180)^2-0.000443*(VZAbins*pi/180)^3))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Nikkor-OP10") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(1.0168*(VZAbins*pi/180)-0.0573*(VZAbins*pi/180)^2-0.1176*(VZAbins*pi/180)^3))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Soligor") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.677923*(VZAbins*pi/180)-0.029481*(VZAbins*pi/180)^2-0.022084*(VZAbins*pi/180)^3+0.041495*(VZAbins*pi/180)^4-0.016644*(VZAbins*pi/180)^5))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Raynox-CF185") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5982*(VZAbins*pi/180)+0.024459*(VZAbins*pi/180)^2))
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="CanonEF-8-15") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.88554*(VZAbins*pi/180)-0.29909*(VZAbins*pi/180)^2)+0.089523*(VZAbins*pi/180)^3)
  }

  #From Hemisfer - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Kodak-SP360") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.42444*(VZAbins*pi/180)+0.23456*(VZAbins*pi/180)^2)-0.063332*(VZAbins*pi/180)^3)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Laowa-4") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6241*(VZAbins*pi/180)-0.0252*(VZAbins*pi/180)^2)+0.024*(VZAbins*pi/180)^3-0.0183*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Meike-6.5") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5165*(VZAbins*pi/180)+0.1106*(VZAbins*pi/180)^2)+0.0617*(VZAbins*pi/180)^3-0.0601*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Meike-3.5") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6475*(VZAbins*pi/180)-0.002*(VZAbins*pi/180)^2)-0.0331*(VZAbins*pi/180)^3-0.00010171*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Entaniya-M12-220") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5449*(VZAbins*pi/180)-0.0491*(VZAbins*pi/180)^2)+0.0513*(VZAbins*pi/180)^3-0.0166*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Entaniya-M12-250") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5146*(VZAbins*pi/180)-0.0453*(VZAbins*pi/180)^2)+0.0374*(VZAbins*pi/180)^3-0.013*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Entaniya-HAL-250") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5187*(VZAbins*pi/180)-0.0351*(VZAbins*pi/180)^2)+0.0333*(VZAbins*pi/180)^3-0.0137*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Entaniya-HAL-200") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6093*(VZAbins*pi/180)-0.0187*(VZAbins*pi/180)^2)+0.0235*(VZAbins*pi/180)^3-0.0142*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Entaniya-M12-280") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5229*(VZAbins*pi/180)-0.043*(VZAbins*pi/180)^2)+0.0253*(VZAbins*pi/180)^3-0.0109*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Canon-RF-5.2") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*0.6031*(VZAbins*pi/180))
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="iZugar-MKX22") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6622*(VZAbins*pi/180)-0.0163*(VZAbins*pi/180)^2)+0.0029*(VZAbins*pi/180)^3-0.0169*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="iZugar-MKX13") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6208*(VZAbins*pi/180)-0.0101*(VZAbins*pi/180)^2)+0.0134*(VZAbins*pi/180)^3-0.0047*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="iZugar-MKX200") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.5898*(VZAbins*pi/180)-0.0133*(VZAbins*pi/180)^2)+0.0193*(VZAbins*pi/180)^3-0.0099*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="iZugar-MKX19") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6355*(VZAbins*pi/180)-0.0259*(VZAbins*pi/180)^2)+0.0292*(VZAbins*pi/180)^3-0.0152*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="DZO-VRCA") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6984*(VZAbins*pi/180)+0.0173*(VZAbins*pi/180)^2)-0.0421*(VZAbins*pi/180)^3-0.008*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Sunex-DSL315") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.748*(VZAbins*pi/180)-0.0272*(VZAbins*pi/180)^2)-0.0032*(VZAbins*pi/180)^3-0.0198*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Sunex-DSL239") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.7129*(VZAbins*pi/180)-0.0239*(VZAbins*pi/180)^2)+0.0069*(VZAbins*pi/180)^3-0.0172*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Sunex-DSL415") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.587*(VZAbins*pi/180)-0.0231*(VZAbins*pi/180)^2)+0.0351*(VZAbins*pi/180)^3-0.0124*(VZAbins*pi/180)^4)
  }


  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Sunex-DSLR01") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.7811*(VZAbins*pi/180)-0.0363*(VZAbins*pi/180)^2)-0.0144*(VZAbins*pi/180)^3-0.0154*(VZAbins*pi/180)^4)
  }


  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Lensagon-BF10M14522S118"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6391*(VZAbins*pi/180)-0.0252*(VZAbins*pi/180)^2)+0.0381*(VZAbins*pi/180)^3-0.0214*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Lensagon-BF16M220D"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(.5574*(VZAbins*pi/180)-0.0371*(VZAbins*pi/180)^2)+0.0412*(VZAbins*pi/180)^3-0.0164*(VZAbins*pi/180)^4)
  }


  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Omnitech-ORIFL190-3"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.7418*(VZAbins*pi/180)-0.0309*(VZAbins*pi/180)^2)-0.0022*(VZAbins*pi/180)^3-0.0176*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Aico-ACHIR01028B10M"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.4896*(VZAbins*pi/180)-0.054*(VZAbins*pi/180)^2)+0.0651*(VZAbins*pi/180)^3-0.0208*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Aico-ACHIR01420B9M"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6194*(VZAbins*pi/180)-0.0094*(VZAbins*pi/180)^2)+0.0126*(VZAbins*pi/180)^3-0.0041*(VZAbins*pi/180)^4)
  }

  # From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  # if (lens=="Raynox-DCR-CF187"){
  #   VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
  #   rset<-round(rc*(0.9442*(VZAbins*pi/180)+0.0138*(VZAbins*pi/180)^2)-0.167*(VZAbins*pi/180)^3+0.0213*(VZAbins*pi/180)^4)
  # }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="SMTEC-SL-190"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.7091*(VZAbins*pi/180)-0.0123*(VZAbins*pi/180)^2)-0.0013*(VZAbins*pi/180)^3-0.0126*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Evetar-E3307"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6929*(VZAbins*pi/180)+0.003*(VZAbins*pi/180)^2)+0.0541*(VZAbins*pi/180)^3-0.0548*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Evetar-E3267A"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(.5171*(VZAbins*pi/180)-0.055*(VZAbins*pi/180)^2)+0.054*(VZAbins*pi/180)^3-0.019*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Evetar-E3279"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.4914*(VZAbins*pi/180)-0.0616*(VZAbins*pi/180)^2)+0.1315*(VZAbins*pi/180)^3-0.0323*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="ArduCam-M25156H18"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(0.6826*(VZAbins*pi/180)-0.0284*(VZAbins*pi/180)^2)+0.0292*(VZAbins*pi/180)^3-0.0185*(VZAbins*pi/180)^4)
  }

  #From Paul Bourke (http://www.paulbourke.net/dome/fisheyecorrect/) - Note: it give relative distance (y) based on theta in radians (x)
  if (lens=="Bosch-Flexidome-7000"){
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    rset<-round(rc*(.7823*(VZAbins*pi/180)-0.0102*(VZAbins*pi/180)^2)-0.0325*(VZAbins*pi/180)^3-0.0127*(VZAbins*pi/180)^4)
  }

  if (lens=="equisolid") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    maxVZAmm<-2*sin(maxVZA/2*pi/180)
    rset<-round(rc*2*sin(VZAbins/2*pi/180)/maxVZAmm)
  }

  if (lens=="stereographic") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    maxVZAmm<-2*tan(maxVZA/2*pi/180)
    rset<-round(rc*2*tan(VZAbins/2*pi/180)/maxVZAmm)
  }

  if (lens=="orthographic") {
    VZAbins<-seq(startVZA,endVZA,(endVZA-startVZA)/nrings)
    maxVZAmm<-2*sin(maxVZA*pi/180)
    rset<-round(rc*2*sin(VZAbins*pi/180)/maxVZAmm)
  }

  #8 azimuth segments
  nseg=nseg

  #define the sector of the image based on the rdf:
  options(dplyr.summarise.inform=FALSE)

  rdf<-imgdf |>
    dplyr::mutate(ring=cut(.data$r,rset,include.lowest=TRUE,labels=seq(startVZA+((endVZA-startVZA)/2/nrings),endVZA-((endVZA-startVZA)/2/nrings),(endVZA-startVZA)/nrings)))|>
    # dplyr::mutate(ring=cut(.data$r,rset,include.lowest=TRUE,labels=seq(setVZA/nrings,setVZA,setVZA/nrings)))|>
    tidyr::drop_na(.data$ring) |>
    dplyr::mutate(ring=as.numeric(as.character(.data$ring))) |>
    dplyr::mutate(alpha.to=cut(.data$theta,seq(0,2*pi,2*pi/nseg)*180/pi,include.lowest=TRUE,labels=rep(seq(2*pi/nseg,2*pi,2*pi/nseg))*180/pi)) |>
    dplyr::mutate(alpha.to=as.numeric(as.character(.data$alpha.to))) |>
    dplyr::group_by(.data$ring,.data$alpha.to) |>
    dplyr::summarise(GF=mean(.data$value,na.rm=TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(alpha.from=rep(seq(0,2*pi-pi/nseg,2*pi/nseg)*180/pi,nrings))|>
    # dplyr::mutate(GF=dplyr::case_when(
    #   .data$GF==0~dplyr::nth(sort(unique(.data$GF)),2),
    #   # .data$GF==0~0.00004530,
    #   TRUE~.data$GF
    # )) |>
    dplyr::relocate(.data$ring,.data$alpha.from)

  rdfw<-rdf |>
    tidyr::pivot_wider(id_cols=.data$ring,names_from=c('alpha.from','alpha.to'),values_from='GF',names_prefix='GF') |>
    dplyr::mutate(id=base::names(img.bw),lens=lens) |>
    dplyr::mutate(circ=paste(xc,yc,rc, sep='_')) |>
    dplyr::relocate(.data$id) |>
    dplyr::ungroup()

  if (length(setdiff(c('channel','stretch','gamma','zonal','thd','method'),names(metadata)))>0){
    metadata=data.frame(channel=NA,stretch=NA,gamma=NA,zonal=NA,thd=NA,method=NA)
  }

  rdfw<-cbind(rdfw,metadata)

  if (display==TRUE){
    raster::plot(img.bw,col=c('black','white'), main='applied rings & segments',legend=FALSE)
    plotrix::draw.circle(xc,yc,rset,border='yellow',lwd=3)
    slices<-seq(pi/nseg,2*pi,2*pi/nseg)
    for (i in 1:length(slices)){
      graphics::segments(xc,yc,xc+max(rset)*sin(slices[i]),yc+max(rset)*cos(slices[i]),col='yellow',lwd=3)
    }
  }
  return(rdfw)
}
