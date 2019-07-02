#'  table.con retourne des tables de contingences pour deux variables d'un même data.frame. Effectifs, fréquence totale, en ligne et en colonne
#'
#' @param data le data.frame
#' @param var vecteur des 2 variables d'intéret. Chaines de caracteres.
#' @param decim nombre de décimales
#' @return res, un data.frame
#' @export

table.con<-function(data=df, var=c(), decim=0) {
  var1<-var[1]
  var2<-var[2]
  #
  tab1<-table(data[, var1], data[, var2])
  tab1.1<-cbind(tab1, rowSums(tab1))
  tab1.1<-rbind(tab1.1, colSums(tab1.1))
  #
  tab2<-prop.table(tab1)
  tab2.1<-cbind(tab2, rowSums(tab2))
  tab2.1<-rbind(tab2.1, colSums(tab2.1))
  tab2.1<-round(tab2.1*100, decim)
  #
  tab3<-prop.table(tab1, margin=1)
  tab3.1<-cbind(tab3, rowSums(tab3))
  tab3.1<-round(tab3.1*100, decim)
  #
  tab4<-prop.table(tab1, margin=2)
  tab4.1<-rbind(tab4, colSums(tab4))
  tab4.1<-round(tab4.1*100, decim)
  #
  res<-list("Effectifs"=tab1.1, "Frequence"=tab2.1, "Freq_ligne"=tab3.1, "Freq_col"=tab4.1)
  return(res)
}
