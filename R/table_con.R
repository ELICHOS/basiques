#'  table.con retourne des tables de contingences pour deux variables d'un même data.frame. Effectifs, fréquence totale, en ligne et en colonne
#'
#' @param data le data.frame
#' @param var vecteur des 2 variables d'intéret. Chaines de caracteres.
#' @param decim nombre de décimales
#' @return res, un data.frame
#' @export
table.con<-function(data=df, var=c(), decim=0,
                    na.pattern=c("", " ", NA),
                    na.recode="non-réponse", ... ) {
  var1<-var[1]
  var2<-var[2]
  data[, var1]->datvar1
  data[, var2]->datvar2
  if(is.character(datvar1)){
    datvar1[datvar1%in%na.pattern]<-na.recode
  }
  if(is.factor(datvar1)){
    levels(datvar1)->storlev
    datvar1<-as.character(datvar1)
    datvar1[datvar1%in%na.pattern]<-na.recode
    datvar1<-factor(x = datvar1, levels = c(storlev, na.recode), ordered = TRUE)
  }
  if(is.character(datvar2)){
    datvar2[datvar2%in%na.pattern]<-na.recode
  }
  if(is.factor(datvar2)){
    levels(datvar2)->storlev
    datvar2<-as.character(datvar2)
    datvar2[datvar2%in%na.pattern]<-na.recode
    datvar2<-factor(x = datvar2, levels = c(storlev, na.recode), ordered = TRUE)
  }
  #
  tab1<-table(datvar1,datvar2, ... )
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
  tab5.1<-as.table(matrix(data=paste(tab1.1[1:nrow(tab1.1)-1 , ], " (",tab3.1,"%)", sep=""), ncol = ncol(tab1.1)))
  colnames(tab5.1)<-colnames(tab1.1)
  colnames(tab5.1)[length(  colnames(tab5.1) )]<-"Ensemble"
  rownames(tab5.1)<-rownames(tab1.1)[1:length(rownames(tab1.1))-1]
  tab6.1<-as.table(matrix(data=paste(tab1.1[ , 1:ncol(tab1.1)-1], " (",tab4.1,"%)", sep=""), ncol = ncol(tab4.1)))
  colnames(tab6.1)<-colnames(tab1.1)[1:length(colnames(tab1.1))-1]
  rownames(tab6.1)<-rownames(tab1.1)
  rownames(tab6.1)[length(  rownames(tab6.1) )]<-"Ensemble"
  
  res<-list("Effectifs"=tab1.1, "Frequence"=tab2.1, "Freq_ligne"=tab3.1, "Freq_col"=tab4.1, "print_row"=tab5.1, "print_col"=tab6.1)
  return(res)
}
