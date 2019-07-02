#' @export tabacp
tabacp<-function(model=catdes.global){
  library(tidyverse)
  names(model$quanti)->grups
  names(model$category)->grups.quali
  model$quanti.var -> quantivars
  qualivars<-unique(
    unlist(
      lapply(seq_along(model$category), FUN = function(i){
        rownames(model$category[[i]])->names.i
        names.i
      })
    )
  )
  #### QUANTI ####
  if(!is.null(quantivars)){
  data.frame(matrix(data = 0, nrow = nrow(quantivars), ncol = length(grups)))->df
  names(df)<-grups
  row.names(df)<-row.names(quantivars)
  df2<-sapply(X = 1:dim(df)[2], FUN = function(j){
    model$quanti[[names(df)[j]]]->dfj
    #return(dfj)
    sapply(X = 1:dim(df)[1], FUN = function(i){
      row.names(df)[i]->vari
      #print(vari)
      dfj[row.names(dfj)==vari, "v.test"]->vtest.ij
      if(length(vtest.ij)==0){vtest.ij<-NA}
      dfj[row.names(dfj)==vari, "p.value"]->pvalue.ij
      if(length(pvalue.ij)==0){pvalue.ij<-NA}
      #print(vtest.ij==0)
      #print(vtest.ij)
      #if(pvalue.ij<0.1&pvalue.ij>0.05){}
      #df[i , j]<-vtest.ij
      vtest.ij
    }
    )
  }
  )
  data.frame(df2)->df2

  if(nrow(quantivars)==1&FALSE%in%unique(dim(df2)==dim(df))){
    names(df2)<-row.names(df)
    row.names(df2)<-names(df)
  } else {
  names(df2)<-names(df)
  row.names(df2)<-row.names(df)
  }
  ####
  df2$VARIABLE<-row.names(df2)
  df2<-df2 %>% gather(-VARIABLE, key = CLASS, value = VALUE)
  df.quanti<-df2
  } else { df.quanti<-NULL }

  #### QUALI ####
  if(!is.null(qualivars)){
  data.frame(matrix(data = 0, nrow = length(qualivars), ncol = length(grups.quali)))->df
  names(df)<-grups.quali
  row.names(df)<-qualivars
  #
  df2<-sapply(X = 1:ncol(df), FUN = function(j){
    model$category[[names(df)[j]]]->dfj
    #return(dfj)
    sapply(X = 1:nrow(df), FUN = function(i){
      row.names(df)[i]->vari
      #print(vari)
      dfj[row.names(dfj)==vari, "v.test"]->vtest.ij
      if(length(vtest.ij)==0){vtest.ij<-NA}
      dfj[row.names(dfj)==vari, "p.value"]->pvalue.ij
      if(length(pvalue.ij)==0){pvalue.ij<-NA}
      #print(vtest.ij==0)
      #print(vtest.ij)
      #if(pvalue.ij<0.1&pvalue.ij>0.05){}
      #df[i , j]<-vtest.ij
      vtest.ij
    }
    )
  }
  )
  #
  data.frame(df2)->df2
  names(df2)<-names(df)
  row.names(df2)<-row.names(df)
  ####
  df2$VARIABLE<-row.names(df2)
  df2<-df2 %>% gather(-VARIABLE, key = CLASS, value = VALUE)
  df.quali<-df2
  } else {df.quali<-NULL}

  #### RES ####
  res<-list("QUANTI"=df.quanti, "QUALI"=df.quali)
  return(res)
  ####
  #seq.cut<-c(0, 0.01, 0.05, 0.1, 1)

  ####
}
