#' @name canopy_fisheye
#' @aliases canopy_fisheye
#' @title Calculate canopy attributes from angular gap fraction data derived from fisheye images
#' @param rdfw Dataframe. The input dataframe generated from [gapfrac_fisheye()], which contains gap fraction for zenith and azimuth bins.
#'
#' @importFrom dplyr mutate group_by summarise select relocate contains across
#'
#' @return A dataframe of canopy attributes from classified fisheye images.
#'
#' @description The function calculate canopy attributes from angular distribution of gap fraction.
#' It returns both the effective (Le) and actual (L) leaf area index following the [Miller theorem (1967)](https://www.publish.csiro.au/BT/BT9670141).
#' The [Lang and Xiang (1986)](https://www.sciencedirect.com/science/article/pii/016819238690033X) clumping index LX is calculated as the ratio of Le to L;  two additional clumping indices (LXG1, LXG2) are derived from ordered weighted average gap fraction as in [Chianucci et al. 2019](https://cdnsciencepub.com/doi/10.1139/cjfr-2018-0213).
#' The mean leaf angle (MTA) and the ellipsoidal x are derived from [Norman and Campbell (1989)](https://link.springer.com/chapter/10.1007/978-94-009-2221-1_14).
#' Canopy openness is also provided as weighted diffuse non-interceptance (DIFN), following the LAI-2200 manual (Li-Cor Inc., Nebraska US).
#'
#' @examples
#' \donttest{
#' c.im<-system.file('extdata/circular_coolpix4500+FC-E8_chestnut.jpg',package='hemispheR')
#' c.im |>
#'   import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'   binarize_fisheye() |>
#'   gapfrac_fisheye(lens='FC-E8',nrings=7,nseg=8,endVZA=70) |>
#'   canopy_fisheye()
#'
#' #Zenith rings similar to LAI-2000/2200:
#' c.im |>
#'  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'   binarize_fisheye() |>
#'   gapfrac_fisheye(lens='FC-E8',nrings=5,nseg=8,endVZA=75) |>
#'   canopy_fisheye()
#'
#' #The hinge angle method close to 1 radian (57 degree):
#' c.im |>
#'  import_fisheye(circ.mask=camera_fisheye('Coolpix4500+FC-E8')) |>
#'   binarize_fisheye() |>
#'   gapfrac_fisheye(lens='FC-E8',nrings=1,nseg=8,startVZA=55,endVZA=60) |>
#'   canopy_fisheye()
#' }
#' @seealso
#' Chianucci F., Zou J., Leng P., Zhuang Y., Ferrara C. 2019. A new method to estimate clumping index integrating gap fraction averaging with the analysis of gap size distribution. Canadian Journal of Forest Research 49 \doi{10.1139/cjfr-2018-0213} \cr
#' \cr
#' LAI-2200C Plant Canopy Analyzer - Instruction Manuals. Licor.\cr
#' \cr
#' Lang A.R.G., Xiang Y. 1986. Estimation of leaf area index from transmission of direct sunlight in discontinuous canopies. Agricultural and Forest Meteorology 37, 228-243. <https://www.sciencedirect.com/science/article/pii/016819238690033X> \cr
#' \cr
#' Miller J.B. 1967. A formula for average foliage density. Australian Journal of Botany 15(1) 141 - 144. \doi{10.1071/BT9670141} .\cr
#' \cr
#' Norman J.M., Campbell G.S. 1986. Canopy structure. In: Plant Physiological Ecology, pp. 301-325 \doi{10.1007/978-94-009-2221-1_14}.
#'
#' @export

# utils::globalVariables('.')

canopy_fisheye<-function(rdfw){

ring <- sinth <- costh <- name <- value <- wa2.3 <- wr2.3 <- wa3.4 <- wr3.4 <- NULL
valuea2.3 <- valuer2.3 <- valuea3.4 <- valuer3.4 <- LXG1 <- LXG2 <- NULL
w <- GapFr <- W <- id <- Le <- L <- DIFN <- LX <- NULL


  setVZA=paste0(unique(rdfw$ring),collapse= '_')
  nrings=length(unique(rdfw$ring))
  nseg=rdfw |>
    dplyr::select(contains('GF')) |>
    ncol()

  if (length(setdiff(c('circ','lens','channel','stretch','gamma','zonal','thd','method'),names(rdfw)))>0){
    circ=lens=channel=stretch=gamma=zonal=thd=method=NA
    metadata=data.frame(circ=NA,lens=NA,channel=NA,stretch=NA,gamma=NA,zonal=NA,thd=NA,method=NA)
    rdfw |>
      dplyr::bind_cols(metadata)
  } else{

    circ=unique(rdfw$circ)
    lens=unique(rdfw$lens)
    channel=unique(rdfw$channel)
    stretch=unique(rdfw$stretch)
    gamma=unique(rdfw$gamma)
    zonal=unique(rdfw$zonal)
    thd=unique(rdfw$thd)
    method=unique(rdfw$method)
  }


  Gaps<-rdfw |>
    dplyr::mutate(dplyr::across(dplyr::starts_with('GF'), ~ifelse(.==0,0.00004530,.))) |>
    # dplyr::mutate(CNTCT=rowMeans(-log(dplyr::select(.,contains('GF'))))*cos(ring*pi/180)) |> # working after magrittr's %>%
    dplyr::mutate(GapFr=rowMeans(dplyr::across(dplyr::contains('GF'))))   |>
    dplyr::mutate(costh=cos(ring*pi/180),sinth=sin(ring*pi/180)) |>
    dplyr::mutate(w=sinth/sum(sin(unique(ring)*pi/180))) |>
    dplyr::mutate(W=sinth*costh/sum(sinth*costh)/2)


  # Not included as output:
  #Lang's L:
  # CNTCT<-Gaps$CNTCT
  # angles<-(Gaps$ring)*pi/180
  # mod.log<-coef(lm(CNTCT~angles))
  # Lang_L<-2*(mod.log[1]+mod.log[2])
  # #
  # #Lang's MTA:
  # G<-CNTCT/Lang_L
  # m<-coef(lm(G~angles))[2]
  # MTA<-round(as.numeric(56.81964+(46.84833+(-64.62133+(-158.69141+(522.0626+1008.14931*m)*m)*m)*m)*m))
  # MTA[MTA<0]<-0
  # MTA[MTA>90]<-90

  #Ellipsoidal
  ellip<-function(Z,x){
    sqrt(x*x+tan(Z)*tan(Z))/(1.47+(((0.000509*x-0.013)*x+0.1223)*x+0.45)*x)
  }

  dx=.01

  Z<-Gaps$ring
  rad=pi/180
  T1<-t(as.matrix(Gaps$GapFr))
  # T2<-t(as.matrix(Gaps$lnGapFr))

  x=1
  xmax=10
  xmin=.1


  F=NULL
  KB=ellip(Z*rad,x)
  DK=(ellip(Z*rad,x+dx)-KB)
  S1=sum(log(T1)*KB)
  S2=sum(KB^2)
  S3=sum(KB*DK)
  S4=sum(DK*log(T1))
  F=c(F,S2*S4-S1*S3)  #ok


  S1=S2=S3=S4=NULL
  while(xmax-xmin>dx){

    if (F<0){
      xmin=x
    }else{
      xmax=x
    }
    x=(xmax+xmin)/2
    KB=ellip(Z*rad,x)
    DK=(ellip(Z*rad,x+dx)-KB)
    S1=sum(log(T1)*KB)
    S2=sum(KB^2)
    S3=sum(KB*DK)
    S4=sum(DK*log(T1))
    F=S2*S4-S1*S3
  }

  # Le.ell<--S1/S2
  MTA.ell<-round(90*(.1+.9*exp(-.5*x)))

  #LXG methods by Chianucci et al. 2019 <https://cdnsciencepub.com/doi/abs/10.1139/cjfr-2018-0213>
  n<-nseg
  w2.3<-(2*(n:1)/(n*(n+1)))
  rcumsum <- function(x) rev(cumsum(rev(x)))
  w3.4<-rcumsum((1/1:n)/n)

  LXG<-Gaps |>
    dplyr::select(ring,contains('GF')) |>
    tidyr::pivot_longer(-ring) |>
    dplyr::select(-name) |>
    dplyr::arrange(ring,dplyr::desc(value)) |>
    dplyr::mutate(wa2.3=rep(w2.3,nrings),wr2.3=rep(rev(w2.3),nrings)) |>
    dplyr::mutate(wa3.4=rep(w3.4,nrings),wr3.4=rep(rev(w3.4),nrings)) |>
    dplyr::mutate(valuea2.3=value*wa2.3,valuer2.3=value*wr2.3) |>
    dplyr::mutate(valuea3.4=value*wa3.4,valuer3.4=value*wr3.4) |>
    dplyr::group_by(ring) |>
    dplyr::summarise(LXG1=log(sum(valuea2.3,na.rm = T))/log(sum(valuer2.3,na.rm = T)),
                     LXG2=log(sum(valuea3.4,na.rm = T))/log(sum(valuer3.4,na.rm = T))) |>
    dplyr::mutate(w=sin(ring*pi/180)/sum(sin(unique(ring)*pi/180))) |>
    dplyr::summarise(LXG1=sum(LXG1*w,na.rm=T),LXG2=sum(LXG2*w,na.rm=T))

  Canopy<-Gaps |>
    dplyr::mutate(Le=-log(GapFr)*w*costh) |>
    dplyr::mutate(L=rowMeans(-log(dplyr::across(contains('GF'))))*w*costh) |>
    # dplyr::mutate(L=rowMeans(-log(dplyr::select(.,contains('GF'))))*$w*costh) |> # working after magrittr's %>%
    dplyr::mutate(DIFN=rowMeans(dplyr::across(contains('GF')))*2*W) |>
    dplyr::group_by(id) |>
    dplyr::summarise(Le=round(2*sum(Le,na.rm=T),2),L=round(2*sum(L,na.rm=T),2),LX=round(Le/L,2),DIFN=round(sum(DIFN,na.rm=T)*100,3)) |>
    dplyr::mutate(VZA=setVZA,rings=nrings,azimuths=nseg,x=round(x,2),MTA.ell=MTA.ell,LXG1=round(LXG$LXG1,2),LXG2=round(LXG$LXG2,2),
                  mask=circ,lens=lens,channel=channel,stretch=stretch,gamma=gamma,zonal=zonal,method=method,thd=thd) |>
    dplyr::relocate(id,Le:LX,LXG1,LXG2,DIFN,MTA.ell,x)

  return(Canopy)
}
