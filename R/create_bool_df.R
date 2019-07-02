#' @description  Cré un data.frame avec autant de variables booléennes que de modalités présentes dans toutes les variables de type factor dans le data.frame d'origine
#' @author Elie
#' @param  dataframe un data.frame
#' @return un data.frame
#' @references no
#' @title La fonction "create.bool.df"
#' @examples create.bool.df(dataframe= dataclust)
#' @export

create.bool.df<-function(dataframe= dataclust){
  tot<-lapply(X = dataframe, FUN = function(x){
    levels(x)->levs.2
    if(length(levs.2)>2){
      list()->empli
      for(i in 1:length(levs.2)){
        empli[[levs.2[i]]]<-x==levs.2[i]
      }
      do.call(cbind.data.frame, empli)->res0
      do.call(cbind.data.frame, res0)->res
    } else {x}
  }
  )
  do.call(cbind.data.frame, tot)->tot2
  return(tot2)
}

