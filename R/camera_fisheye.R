#' @name camera_fisheye
#' @aliases camera_fisheye
#' @title Provide circular mask parameters from known camera+fisheye lens models
#' @param model Character. An input camera+lens model
#' @returns A list of three parameters (xc, yc, rc) of the circular mask
#' @examples
#' #available camera+lenses:
#' list.cameras
#'
#' camera_fisheye(model='Coolpix4500+FC-E8')
#' @export

camera_fisheye<-function(model=NULL){

  if(length(setdiff(model,c('Coolpix950+FC-E8',
                            'Coolpix990+FC-E8','Coolpix995+FC-E8',
                            'Coolpix4500+FC-E8','Coolpix5000+FC-E8',
                            'Coolpix5400+FC-E9','Coolpix8700+FC-E9',
                            'Coolpix8400+FC-E9','Coolpix8800+FC-E8',
                            'D60+Sigma-4.5','D80+Sigma-4.5',
                            'D90+Sigma-4.5','D300+Sigma-4.5',
                            'EOS300+Regent-4.9','EOS20D+Sigma-4.5',
                            'EOS30D+Sigma-4.5','EOSXSi+Sigma-4.5',
                            'EOST1i+Sigma-4.5','EOST2i+Sigma-4.5',
                            'EOST3i+Sigma-4.5')))>0){
    stop("Error: Unknown camera type. Type 'list.cameras' to see the list of options available.")}


  if(is.null(model)){
    stop("Error: missing values. Set a model or the xc,yc,rc parameters or type 'list.cameras' to see the list of options available.")
  }

  if(model=='Coolpix950+FC-E8'){
    xc=800
    yc=660
    rc=562
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix990+FC-E8'){
    xc=1024
    yc=768
    rc=686
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix995+FC-E8'){
    xc=1024
    yc=768
    rc=731
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix4500+FC-E8'){
    xc=1136
    yc=852
    rc=754
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix5000+FC-E8'){
    xc=1280
    yc=960
    rc=735
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix5400+FC-E9'){
    xc=1296
    yc=972
    rc=622
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix8700+FC-E9'){
    xc=1632
    yc=1224
    rc=954
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix8400+FC-E9'){
    xc=1632
    yc=1224
    rc=1001
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='Coolpix8800+FC-E8'){
    xc=1632
    yc=1224
    rc=954
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='D60+Sigma-4.5'){
    xc=1936
    yc=1296
    rc=1019
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='D80+Sigma-4.5'){
    xc=1936
    yc=1296
    rc=1019
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='D90+Sigma-4.5'){
    xc=2144
    yc=1424
    rc=1050
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='D300+Sigma-4.5'){
    xc=2144
    yc=1424
    rc=1050
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOS300+Regent-4.9'){
    xc=1536
    yc=1024
    rc=999
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOS20D+Sigma-4.5'){
    xc=1752
    yc=1168
    rc=980
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOS30D+Sigma-4.5'){
    xc=1752
    yc=1168
    rc=980
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOSXSi+Sigma-4.5'){
    xc=2136
    yc=1424
    rc=1186
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOST1i+Sigma-4.5'){
    xc=2376
    yc=1584
    rc=1119
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOST2i+Sigma-4.5'){
    xc=2592
    yc=1728
    rc=1438
    cm=list(xc=xc,yc=yc,rc=rc)
  }

  if(model=='EOST3i+Sigma-4.5'){
    xc=2592
    yc=1728
    rc=1438
    cm=list(xc=xc,yc=yc,rc=rc)
  }
  return(cm)
}
