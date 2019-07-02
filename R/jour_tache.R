jour.tache<-function(heure.jour=8, heure.tache=0.5, nb.tache=30, verbeux.table=FALSE){
  tache.jour=heure.jour/heure.tache
  nb.jour=nb.tache/tache.jour
  res1<-list(
    paste("Nb de tâches (param) = ", nb.tache),
    paste("Nb de tâches par jour = ", tache.jour),
    paste("Nb de jours nécéssaires = ", nb.jour)
    )
  veco<-1:(nb.tache*2)
  res2<-data.frame(
    "Nb.tache"=veco,
    "Nb.jour"=sapply(veco, FUN = function(i){
      i/tache.jour
      }
      )
  )
  print(
    res1
  )
  if(verbeux.table==TRUE){
  print(res2)
  }
  return(res2)
}
