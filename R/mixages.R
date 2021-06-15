#' @export
mixages<-function(vecs=list("nom1"=vec1, "nom2"=vec2)){
  vecs[[1]]<-as.character(vecs[[1]])
  vecs[[2]]<-as.character(vecs[[2]])
  
  common<-intersect(vecs[[1]], vecs[[2]])
  only.1<-vecs[[1]][!vecs[[1]]%in%common]
  only.2<-vecs[[2]][!vecs[[2]]%in%common]
  res<-list("communs"=common, "only.1"=only.1, "only.2"=only.2)
  if(!is.null(names(vecs))){
  names(res)[names(res)=="only.1"]<-paste0("only.",names(vecs)[1])
  names(res)[names(res)=="only.2"]<-paste0("only.",names(vecs)[2])
  }
  return(res)
}
