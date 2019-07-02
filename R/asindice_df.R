#'  asindice.df applique asindice sur une data.frame ou une matrice
#'
#' @param DF une data.frame ou matrice
#' @param Margin le temps en ligne ou en colonne
#' @param type 'VALEUR' ou 'TAUX': renseigne le type de variable entrante
#' @param rowbase le rang de la base dans la série temporelle
#' @param indice la base (numérique)
#' @param alter.rownames characters vector
#' @param change.nom applique ou non le chaine de caracteres change.nom aux noms de variables pré-existants
#' @return un vecteur res01
#' @export
asindice.df<-function(DF=NAF17ens, Margin=2, type="VALEUR", rowbase=1 , indice=100, alter.rownames=NULL , alter.colnames=NULL, change.nom=NULL) {
  if(is.null(alter.colnames)){nama<-names(DF)}
  if(!is.null(alter.colnames)){nama<-alter.colnames}
  if(is.null(alter.rownames)){namo<-row.names(DF)}
  if(!is.null(alter.rownames)){namo<-alter.rownames}
  DF0<-data.frame()
  if(Margin==1){
    for( i in 1:dim(DF)[1]){
      for(j in 1:dim(DF)[2]){
        DF0[i, j]<-DF[i, j]/DF[i, rowbase]*indice
      }
    }
    names(DF0)<-names(DF)
    row.names(DF0)<-namo
  }
  if(Margin==2){
    for( i in 1:dim(DF)[1]){
      for(j in 1:dim(DF)[2]){
        DF0[i, j]<-DF[i, j]/DF[rowbase, j]*indice
      }
    }
    names(DF0)<-names(DF)
    row.names(DF0)<-namo
  }
  return(DF0)
}



# asindice.df<-function(DF=NAF17ens, Margin=2, type="VALEUR", rowbase=1 , indice=100, change.nom=NULL) {
#   mat1<-apply(X = DF, MARGIN = Margin, FUN = function(x){asindice(type =type, timeserie = x, rowbase=1, indice=100)})
#   mat1.df<-data.frame(mat1)
#   if(is.null(change.nom)){
#     if(is.data.frame(DF)==TRUE){
#       if(Margin==1){names(mat1.df)<-names(DF)}
#       if(Margin==2){names(mat1.df)<-row.names(DF)}
#     }
#     if(is.matrix(DF)==TRUE){
#       if(Margin==1){names(mat1.df)<-colnames(DF)}
#       if(Margin==2){names(mat1.df)<-rownames(DF)}
#     }
#   } else {
#     if(is.data.frame(DF)==TRUE){
#       if(Margin==1){
#         names2<-paste(names(DF), change.nom, sep = "", collapse = NULL)
#         names(mat1.df)<-names2
#       }
#       if(Margin==2){
#       names2<-paste(row.names(DF), change.nom, sep = "", collapse = NULL)
#       names(mat1.df)<-names2
#       }
#     }
#     if(is.matrix(DF)==TRUE){
#       if(Margin==1){
#         names2<-paste(colnames(DF), change.nom, sep = "", collapse = NULL)
#         names(mat1.df)<-names2
#       }
#       if(Margin==2){
#       names2<-paste(rownames(DF), change.nom, sep = "", collapse = NULL)
#       names(mat1.df)<-names2
#     }
#     }
#   }
#   return(mat1.df)
# }
