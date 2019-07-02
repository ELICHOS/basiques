#' @author Elie
#' @description  ...
#' @title La fonction "tranfofacto"
#' @description  tranfofacto: transforme les levels d'une variable factor
#' @param var de la forme data.frame$var
#' @param codage  codage doit être une liste: list("new code"=c("old 1", "old 2"))
#' @return un vecteur de facteurs res
#' @export
tranfofacto<-function(var, codage) {
  codes <- codage
  res <- as.character(var)
  for(code in names(codes)) {
    res[res %in% codes[[code]]] <- code
  }
  factor(res)
}
