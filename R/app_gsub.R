#' @export
app_gsub<-function(vector, equiv.list){
  res<-sapply(1:length(vector), function(i){
    for(labna in names(equiv.list)){
      equiv.list[[labna]]->vecs
      for(lab in vecs){
        vector[i]<-gsub(x = vector[i], pattern = lab, replacement = labna) 
      }
    }
    return(vector[i])
  })
  return(res)
}
