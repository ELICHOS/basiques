#' @author Elie
#' @description cré des tables de contingences (effectifs et fréquences*3) sur un data.frame entier pour une variable de base
#' @param  dataframe  le data.frame
#' @param  var.base  nom (char) ou index de la variable à tester
#' @param  var.exclude  NULL optional: nom ou index des variables à exclure
#' @param  decim  1 nombre de décimales dans le résultat (round())
#' @param  type  1 what is it?
#' @return list("Effectifs"=tab1.1, "Frequence"=tab2.1, "Freq_ligne"=tab3.1, "Freq_col"=tab4.1)
#' @references table() ; prop.table() ; round()
#' @title La fonction "app.tabcon"
# @examples app.tabcon<-function(dataframe=databool.N1[, names(databool.N1)%w/o%paste("clust.c1.", 2:5, sep="")]
# var.base="clust.c1.1",
#var.exclude=NULL,
#decim=1)
#' @export
app.tabcon<-function(dataframe=databool.N1[, names(databool.N1)%w/o%paste("clust.c1.", 2:5, sep="")],
                     var.base="clust.c1.1",
                  var.exclude=NULL,
                  decim=1){
    var.exclude=c(var.base, var.exclude)
  if(is.null(var.base)==FALSE){
    vartest<-names(dataframe[, names(dataframe)%w/o%var.exclude])
    df<-data.frame(dataframe[, names(dataframe)%in%vartest])
    names(df)<-vartest
  }else{
    if(is.null(var.base)==TRUE){
stop("Absence de var.base")
    }
  }
  #
    lapply(X = df, FUN = function(x){
        table(x = dataframe[, var.base], y = x)->tab1
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
    })->res.tabs
    return(res.tabs)
}
