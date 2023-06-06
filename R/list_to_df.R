#' list_to_df
#' 
#' converti list("lettres"=c("a","b","c"), "numeros"=c(1, 2, 3)) en data.frame
#' 
#' @param x a named list of vectors
#' 
#' @export
list_to_df<-function(x){
  res<-dplyr::bind_rows(
    lapply(names(x), function(ni){
    data.frame("name"=ni, "value"=x[[ni]])
  })
  )
  return(res)
}