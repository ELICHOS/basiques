#' @description Cré un data.frame avec autant de variables booléennes que de modalités présentes dans la variable factor.vars
#' @author Elie
#' @param  dataframe un data.frame
#' @param  factor.vars le nom (char) ou l'indexe de la variable facteur d'intérêt
#' @return un data.frame
#' @references no
#' @title La fonction "create.bool"
#' @examples create.bool(dataframe=dataclust, factor.vars="Q8.Ress1")
#' @export

create.bool<-function(dataframe=dataclust,
                      factor.vars="Q8.Ress1"){
  factor.var<-dataframe[, factor.vars]
  if(length(factor.vars)==1){
    levels(factor.var)->levs
    list()->empli
    for(i in 1:length(levs)){
      empli[[levs[i]]]<-factor.var==levs[i]
    }
    do.call(cbind.data.frame, empli)->res
  }
  if(length(factor.vars)>1){
    trest<-lapply(X = factor.var, FUN = function(x){
      levels(x)->levs
      list()->empli
      for(i in 1:length(levs)){
        empli[[levs[i]]]<-x==levs[i]
      }
      do.call(cbind.data.frame, empli)->res0
    }
    )
    do.call(cbind.data.frame, trest)->res
  }
  return(res)
}


