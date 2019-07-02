#' @export apparis
apparis<-function(dataf.depart=datas_e2, dataf.arrivee=datas_e3, VARS="Q1NB", MAILS="email"){
  name.depart<-names(dataf.depart)[grepl(pattern = VARS,  gsub(".", "", x=names(dataf.depart), fixed = TRUE), ignore.case = TRUE)==TRUE]
  #
  name.arrive<-names(dataf.arrivee)[grepl(pattern = VARS,  gsub(".", "", x=names(dataf.arrivee), fixed = TRUE), ignore.case = TRUE)==TRUE]
  #
  mail.depart<-names(dataf.depart)[grepl(pattern = MAILS,  gsub(".", "", x=names(dataf.depart), fixed = TRUE), ignore.case = TRUE)==TRUE]
  #
  mail.arrivee<-names(dataf.arrivee)[grepl(pattern = MAILS,  gsub(".", "", x=names(dataf.arrivee), fixed = TRUE), ignore.case = TRUE)==TRUE]
  #
  levels(as.factor(dataf.depart[ , name.depart ]))->levsdep
  #
  lo<-lapply(X = levsdep, FUN = function(li){
    subset(dataf.depart, dataf.depart[ , name.depart]==li)->subsli
    nrow(subsli)->totli
    subset(dataf.arrivee, dataf.arrivee[ , mail.arrivee]%in%subsli[ , mail.depart])->sub.arrili
    table(sub.arrili[ , name.arrive])->tab.eff
    round(prop.table(tab.eff)*100, 2)->tab.prop
    resli<-list("tab.eff"=tab.eff, "tab.prop"=tab.prop)
    return(resli)
  })
  #
  names(lo)<-levsdep
  #
  do.call( "rbind", lapply(X = lo, FUN = function(x){as.data.frame(x)}))->la
  la$rep.dep<-gsub(".", "", x=row.names(la), fixed = TRUE)
  la$rep.dep<-gsub('[[:digit:]]+', "", x=la$rep.dep)
  vecverif<-sapply(X = 1:nrow(la), FUN = function(j){
    la$tab.eff.Var1[j]==la$tab.prop.Var1[j]
  })
  if(FALSE%in%vecverif){
    message("ERROR")
  } else {
    la[ , c("rep.dep", "tab.eff.Var1",  "tab.eff.Freq", "tab.prop.Freq")]->la
    names(la)<-c("Reponse.Depart","Reponse.arrivee" , "Effectifs.arrivee", "Prop.arrivee")
    return(la)
  }
}
