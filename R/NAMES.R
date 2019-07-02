#'  NAMES: data.frame de names d'un data.frame
#'
#' @param dataframe le data.frame
#' @return res, un data.frame
#' @export
NAMES<-function(dataframe=data.o){
  res<-data.frame(names(dataframe))
  res$rank<-row.names(res)
  return(res)
}
