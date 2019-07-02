#'  asindice crée un indice à paretir d'une variable numérique. Précise la base par le numéro de ligne et indice à choisir (100, 1000...)
#'
#' @param type 'VALEUR' ou 'TAUX': renseigne le type de variable entrante
#' @param timeserie la série temporelle
#' @param rowbase le rang de la base dans la série temporelle
#' @param indice la base (numérique)
#' @return un vecteur res01
#' @export
asindice<-function(type="VALEUR", timeserie=emploiEQTP$TOTAL , rowbase=1 , indice=100){
  if(type=="VALEUR") {
    res1<-timeserie/timeserie[rowbase]*indice
    return(res1)
  } else {if(type=="TAUX") {
    mult<-timeserie+1
    res0<-with(data.frame(timeserie),
               sapply(seq(mult),function(i)
                 100*prod(mult[1:i])
               )
    )
    res01<-res0/res0[rowbase]*indice
    return(res01)
  }
  }
}
