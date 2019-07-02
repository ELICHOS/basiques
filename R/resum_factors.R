#'  resum.factors retourne un résumé d'une variable qualitative (effectifs et fréquences) dans deux tableaux
#'
#' @param dim variable factor (quali)
#' @param decim nombre de décimales pour l'output
#' @return une matrice res
#' @export
resum.factors<-function(dim=data_log$contrepartie, decim=0) {
  if(class(dim)=="character"){
    dim<-as.factor(dim)
  }
  table1<-rbind(
    round(summary(dim), decim),
    round(summary(dim)/sum(summary(dim))*100, decim)
  )
  table2<-cbind(table1, "TOTAL"=rowSums(table1))
  row.names(table2)<-c("effectifs", "frequence (en %)")
  res<-table2
  return(res)
}
