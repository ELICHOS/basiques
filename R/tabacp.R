#' @export tabacp
tabacp<-function(model=catdes.global, origin.data=data, origin.var){
  store.na<-names(origin.data)
  origin.data<-as.data.frame(origin.data)
  names(origin.data)<-store.na
  library(tidyverse)
  names(model$category)[names(model$category)==""]<-"empty"
  if(!is.null(model$quanti)){
  names(model$quanti)[names(model$quanti)==""]<-"empty"
  names(model$quanti)->grups
  }
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
    df2<-t(df2)%>%data.frame(check.names = FALSE)
  
  } else {
  names(df2)<-names(df)
  row.names(df2)<-row.names(df)
  }
  
  ####
  df2$VARIABLE<-row.names(df2)
  print(df2)
  df2<-df2 %>% gather(-VARIABLE, key = CLASS, value = VALUE)
  print(df2)
  df.quanti<-df2
  df.quanti<-df.quanti%>%#rename("VARIABLE"="CLASS",  "CLASS"="VARIABLE","vtest.ij"="VALUE")%>%
    mutate(TYPE="quanti")
  } else { df.quanti<-NULL }

  #### QUALI ####
  if(!is.null(qualivars)){
  data.frame(matrix(data = 0, nrow = length(qualivars), ncol = length(grups.quali)))->df
  names(df)<-grups.quali
  row.names(df)<-qualivars
  #
  df2<-lapply(X = 1:ncol(df), FUN = function(j){
    model$category[[names(df)[j]]]->dfj
    #return(dfj)
    lapply(X = 1:nrow(df), FUN = function(i){
      row.names(df)[i]->vari
      #print(vari)
      dfj[row.names(dfj)==vari, "v.test"]->vtest.ij
      if(length(vtest.ij)==0){vtest.ij<-NA}
      dfj[row.names(dfj)==vari, "p.value"]->pvalue.ij
      if(length(pvalue.ij)==0){pvalue.ij<-NA}
      dfj[row.names(dfj)==vari, "Mod/Cla"]->mc.test.ij
      if(length(mc.test.ij)==0){mc.test.ij<-NA;mc.test.ij.nb<-NA} else {
        if(!is.null(model$call$row.w)){
        if(length(model$call$row.w)==nrow(origin.data)){
          library(questionr)
          temptab<-questionr::wtd.table(x = origin.data[ , origin.var], 
                               weights = model$call$row.w)
        }
        } else {
          temptab<- table(origin.data[ , origin.var])
        }
        temptab[names(df)[j]]*(mc.test.ij/100)->mc.test.ij.nb
      }
      #print(vtest.ij==0)
      #print(vtest.ij)
      #if(pvalue.ij<0.1&pvalue.ij>0.05){}
      #df[i , j]<-vtest.ij
      c("vtest.ij"=vtest.ij, "mc.test.ij"=mc.test.ij, "mc.test.ij.nb"=mc.test.ij.nb)->res
      return(res)
    }
    )->reslap
    data.frame(do.call("rbind", reslap), stringsAsFactors = FALSE)->resldf
    resldf$CLASS<-names(df)[j]
    resldf$VARIABLE<-row.names(df)
    names(resldf)<-c("vtest.ij", "mc.test.ij", "mc.test.ij.nb", "CLASS", "VARIABLE")
    return(resldf)
  }
  )
  names(df2)<-names(df)
  do.call("rbind", df2)->df2
  ####
  names(df2)[names(df2)==""]<-"vide"
  #df2<-df2 %>% gather(-VARIABLE, key = CLASS, value = VALUE)
  df.quali<-df2
  df.quali<-df.quali%>%mutate(TYPE="quali")
  } else {df.quali<-NULL}

  #### RES ####
  res<-list("QUANTI"=df.quanti, "QUALI"=df.quali)
  return(res)
  ####
  #seq.cut<-c(0, 0.01, 0.05, 0.1, 1)

  ####
}
