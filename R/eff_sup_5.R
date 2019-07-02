#' @author Romane
#' @description Verifie que l'effectif est bien supérieur à 5 dans chaque case du tableau de contingence entre deux variables
#' Permet de savoir si peut appliquer un test du Chi2.
#' @param  var1  la premiere variable : typa factor
#' @param  var2 la deuxième variable : type factor
#' @return booleen: true si effectif supérieur à 5 partout
#' @references voir table()
#' @title La fonction "eff_sup_5"
#' @examples eff_sup_5<-function(var1=as.factor(infert$spontaneous),
#' var2=as.factor(infert$education))
#' @export


eff_sup_5 <- function(var1,var2){
  eff<-table(var1,var2)
  r<-TRUE
  for (i in 1:length(eff[1,])){
    for (j in length(eff[,1])){
      if (eff[i,j]<5) {r=FALSE}
    }
  }
  return(r)
}
