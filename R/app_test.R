#' @author Elie
#' @description Applique un test d'indépendance (Khi2 ou fisher) entre une variable d'un data.frame et toutes les autres (ou certaines) variables
#' @param  dataframe  le data.frame
#' @param  var.base  nom (char) ou index de la variable à tester
#' @param  var.test  NULL optionnal: nom ou index des variables à tester contre var.base
#' @param  var.exclude  NULL optional: nom ou index des variables à exclure du test
#' @return list("res.test"=res.test, "res.df"=res.df)
#' @references voir fisher.test() et chisq.test()
#' @title La fonction "app.test"
#' @examples app.test<-function(dataframe=databool.N1[, names(databool.N1)%w/o%paste("clust.c1.", 2:5, sep="")],
#'var.base="clust.c1.1",
#'var.test=NULL,
#'var.exclude=NULL,
#'test.t="fisher"
#')
#' @export


app.test<-function(dataframe=databool.N1[, names(databool.N1)%w/o%paste("clust.c1.", 2:5, sep="")],
                   var.base="clust.c1.1",
                   var.test=NULL,
                   var.exclude=NULL,
                   test.t="fisher"
                   ){
  var.exclude=c(var.base, var.exclude)
  if(is.null(var.test)==TRUE){
    vartest<-names(dataframe[, names(dataframe)%w/o%var.exclude])
    df<-data.frame(dataframe[, names(dataframe)%in%vartest])
    names(df)<-vartest
  }else{
      if(is.null(var.test)==FALSE){
        vartest<-var.test
        df<-data.frame(dataframe[, names(dataframe)%in%vartest])
        names(df)<-vartest
      }
  }
   lapply(X = df, FUN = function(x){
     if (eff_sup_5(dataframe[, var.base],x)==TRUE){
       test.t="chisq"
     }
     else{test.t="fisher"}
     if(test.t=="fisher"){
      #print(x)
      table(x = dataframe[, var.base], y = x)->tabs
      colnames(tabs)->colnametabs
      #colnametabs
      #tabs
      dftab<-data.frame(tabs)
      if(dim(tabs)[2]<2){tabso<-as.table(cbind(tabs, Othert<-c(0, 0)))
      colnames(tabso)<-c(colnametabs, "other")} else {tabso<-tabs}
      #if(ncol(dftab)<3){tabso<-cbind(tabs, table("other"=c(0, 0)))}else{tabso<-tabs}
#message("Hello")
#print(tabs)
      if(dim(tabso)[2]<2|dim(tabso)[1]<2){
        #message(dim(tabso))
        list("p.value", "estimate")} else {
          if(dim(tabso)[2]>=2|dim(tabso)[1]>=2){
      #message(dim(tabso))
      fisher.test(tabso, simulate.p.value = TRUE)
          }
      }
      #}
    } else {if(test.t=="chisq"){
      table(dataframe[, var.base], x)->tab.chisq
      if(1%in%dim(tab.chisq)|0%in%dim(tab.chisq)){
        list("p.value", "estimate")
      } else {
      chisq.test(x = dataframe[, var.base], y = x)
      }
    }
    }
  }
  )->res.test
   #
   #message("jusqu'ici tout va bien")
   if(test.t=="fisher"){

   pvdf<-lapply(X = res.test, FUN = function(x){
          list(as.character(x[c("p.value", "estimate")]))
   }
   )
   df <- data.frame(matrix(unlist(pvdf), nrow=length(pvdf), byrow=T),stringsAsFactors=FALSE)
   row.names(df)<-names(pvdf)
   names(df)<-c("p.value", "odd.ratio")
   df$p.value<-as.numeric(as.character(df$p.value))
   df$odd.ratio<-as.numeric(as.character(df$odd.ratio))
   df->res.df
   res<-list("res.test"=res.test, "res.df"=res.df)
  return(res)
   }
}
