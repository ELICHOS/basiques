#' @export splitvar
splitvar<-function(datai= dataRPV, vari="QF.Renonce", idvar=NULL, new.complete.df=FALSE, ...){
  library(questionr)
  questionr::multi.split(var = datai[  , vari], split.char = " ; ")->split.dat
####
  namos<-gsub(pattern = "datai[,_vari].", replacement = "", x = names(split.dat), fixed = TRUE)
  split.dat<-data.frame(apply(X = split.dat, MARGIN = 2, FUN = function(xi){
    gsub(pattern = 1, replacement = TRUE, x = xi)
  }))
  split.dat<-data.frame(apply(X = split.dat, MARGIN = 2, FUN = function(xi){
    gsub(pattern =0, replacement = FALSE, x = xi)
  })
  )
  names(split.dat)<-namos

    if(new.complete.df==TRUE){
    cbind(split.dat, datai)->split.dat
    } else {
      if(!is.null(idvar)){
        cbind(split.dat, datai[ , idvar])->split.dat
    }
  }
  return(split.dat)
}
