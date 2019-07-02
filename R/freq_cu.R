#' @title La fonction "freq.cu"
#' @description  freq.cu calcul les fréquences cumulées pour une variable d'un df, de bas en haut ('bas') ou de haut en bas('haut')
#' @param df le dataframe. Si df est une table ou une matrice, sera transformé en data.frame
#' @param var la variable d'intéret dans le data.frame en chaine de caracteres ou en index
#' @param debut  de bas en haut ('bas') ou de haut en bas('haut')
#' @return un vecteur res
#' @export
freq.cu<-function(df=propomat, var="X2", debut="haut"){
  if(class(df)!="data.frame"){
    df<-data.frame(df)
  } else {df<-df}
  #
  if(debut=="haut"){
    frecu1<-with(df,
                 sapply(2:nrow(df), function(i)
                   sum(df[i, var], df[1:(i-1), var])
                 )
    )
    res<-c(df[1, var], frecu1)
  } else {if(debut=="bas"){
    frecu1<-with(df,
                 sapply(1:(nrow(df)-1), function(i)
                   sum(df[i, var], df[(i+1):length(df[, var]), var])
                 )
    )
    res<-c(frecu1, df[length(df[, var]), var])
  }
  }
  return(res)
}
