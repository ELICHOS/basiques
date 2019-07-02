codage<-function(var.de.ref=31:35, data.frame.de.ref= datas_e1,
                 var.de.travail=datas_e2$Q2b.ProfilLq.e2, var.id.de.detravail=datas_e2$email.e2, char.split=";"){
  #library(questionr)
  #questionr::multi.split(var = var.de.ref, split.char = char.split)->df0
  lapply(datas_e1[, var.de.ref], FUN = levels)->levelo
  unlist(levelo)->levelo
  unique(levelo)->levelo
  #df1<-df0[-c(1:nrow(df0)) , ]
  #names(df1)<-names(df0)
  df1<-data.frame(matrix(nrow = length(var.de.travail) ,ncol = length(levelo)))

  names(df1)<-levelo

  cbind(var.id.de.detravail, var.de.travail, df1)->df2
  write.csv(x = df2, file = "codage.csv")

  for(j in 1:length(var.de.travail)){
    for(i in levelo){
      if(is.na(var.de.travail[j])|var.de.travail[j]==""|var.de.travail[j]==" "){
        #vardroite <- readline(paste(var.de.travail[j], "\n", "Question :",  i, "\n", "ind: ", j, sep=""))
        df1[j, i]<-NA
      } else {
        vardroite <- readline(paste(var.de.travail[j], "\n", "Question :",  i, "\n", "ind: ", j, sep=""))
        df1[j, i]<-vardroite}
    }

      nouvelle.cat<-readline("Nouvelle?")
      if(nouvelle.cat!=0){
        nouvelle<-readline(paste(var.de.travail[j], "\n", "Nouvelle :",  "\n", "ind: ", j, sep=""))
        df1[j, nouvelle.cat]<-nouvelle
        levelo<-c(levelo, nouvelle)
    }
    save(df1, file = "tempdf1.Rdata")
  }
return(df1)
}
