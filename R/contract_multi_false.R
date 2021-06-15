#' @export
contract.multi.false<-function(vars_libs=c("Oui"="QP2.MOUI.","Non"="QP2.MNON."), 
                               valids=list(1, "Oui", "OUI"), 
                               nas.pat=c(NA, "N/A") ){
  data[ , unname(vars_libs)]->tempdf
  tempdf[]<-lapply(tempdf, as.character)
  print(names(tempdf))
  tempdf[]<-lapply(names(tempdf), function(xna){
    tempdf[ , xna]->myvec
    myvec[myvec%in%valids]<-names(vars_libs)[vars_libs==xna]
    return(myvec)
  })
  res<-sapply(1:nrow(tempdf), function(i){
    res<-unlist(tempdf[i , ], use.names = FALSE)
    unique(res)->unires
    if(length(unires)==1){
      if(unires%in%nas.pat){
        res<-NA
      } else {
        res<-unires
      }
    } else {
      res<-unires[!unires%in%nas.pat]
    }
    return(res)
  })
  return(res)
}