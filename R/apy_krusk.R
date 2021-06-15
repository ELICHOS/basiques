#' @export
apy.krusk<-function(data, name.num.var){
  data[ ,
    sapply(names(data), function(li){
      data[ , li]->var
      inherits(var, c("character", "factor"))&length(unique(var[!is.na(var)]))>=2
    })
    ]->silona
  cbind(silona, "name.num.var"=data[ , name.num.var])->data.test

  resli<-lapply(names(data.test)[names(data.test)!="name.num.var"], function(ni){
    res<-kruskal.test(data.test$name.num.var~data.test[ , ni])
    data.frame("exogenous"=ni, "test.var"=name.num.var, "p.value"=res$p.value, "statistic"=res$statistic, "parameter"=res$parameter, "method"=res$method)->ress

    return(ress)
  })
  names(resli)<-names(data.test)[names(data.test)!="name.num.var"]
  do.call("rbind", resli)->resli

  describ.dat<-lapply(unique(resli$exogenous), function(xi){
    xi<-as.character(xi)
    if(resli[resli$exogenous==xi , ]$p.value<0.1){
    data %>% group_by(.data[[xi]]) %>% summarise(moyenne=mean(.data[[name.num.var]], na.rm=TRUE),
                                            médiane=median(.data[[name.num.var]], na.rm=TRUE),
                                            nb=n())
    }
  })
  names(describ.dat)<-unique(resli$exogenous)
  describ.dat<-describ.dat[lengths(describ.dat)>0]
  resss<-list("synthese"=resli, "describ"=describ.dat)
  return(resss)
}
