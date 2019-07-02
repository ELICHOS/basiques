#' multsheet permet de lire plusieurs feuille d'une meme table xls avec la fonction read_xls
#'
#' @param tablepath "chemin/du/fichiers.xls"
#' @param sheet.vec  vecteur numérique des feuilles à impotrer (exemple: 1:20)
#' @param sheet.name  nom affecté à chaque data.frame créée (suivi de son numéro)
#' @param SKIP  un seul numérique ou un vecteur de numérique de meme longueur que sheet.vec
#' @param aslist 1=return a list of data.frame ; 2= enregistre les dataframe dans l'env ; 3= combo!
#' @return rien
#' @export
multsheet<-function(tablepath= "secteur_dep.xls" , sheet.vec=1:20, sheet.name=NULL,  SKIP=4, aslist=1){
  library(readxl)
  if(length(SKIP)==1){rep(SKIP, times=length(sheet.vec))->SKIP}
  excel_sheets(path = tablepath)->namo
  namo[sheet.vec]->namo
  if(is.null(sheet.name)){sheet.name<-namo}
  if(!is.null(sheet.name)){sheet.name<-paste(sheet.name, sheet.vec, sep="")}
  if(aslist==1){
    vec<-c()
    for(i in sheet.vec){
      vec[i]<-sheet.name[i]
    }
    as.list(vec)->listo
    names(listo)<-vec
    #as.list(sheet.vec)->obj
    #lapply(X = sheet.vec, FUN = function(x){
    for( i in sheet.vec){
      listo[[i]]<-data.frame(read_xls(path = tablepath , sheet = i, skip = SKIP[i]))
    }
    return(listo)
  }
  if(aslist==2){
    vec<-c()
    for(i in sheet.vec){
      vec[i]<-sheet.name[i]
    }
    as.list(vec)->listo
    names(listo)<-vec
    #as.list(sheet.vec)->obj
    #lapply(X = sheet.vec, FUN = function(x){
    for( i in sheet.vec){
      listo[[i]]<-data.frame(read_xls(path = tablepath , sheet = i, skip = SKIP[i]))
    }
    list2env(listo ,.GlobalEnv)
  }
  if(aslist==3){
    vec<-c()
    for(i in sheet.vec){
      vec[i]<-sheet.name[i]
    }
    as.list(vec)->listo
    names(listo)<-vec
    #as.list(sheet.vec)->obj
    #lapply(X = sheet.vec, FUN = function(x){
    for( i in sheet.vec){
      listo[[sheet.name[i] ]]<-data.frame(read_xls(path = tablepath , sheet = i, skip = SKIP[i]))
    }
    return(listo)
    list2env(listo ,.GlobalEnv)
  }
}
