#' @export
create_struct<-function(FILE="V1_structure.csv"){
  struct<-read.csv(file=FILE, header=TRUE, stringsAsFactors = FALSE)%>%
    select(related_id, class, type.scale, name, text, relevance, help)%>%
    mutate(text=gsub('"', '', text))
  res<-lapply(1:nrow(struct), function(i){
    tedf<-struct[1:i , c("class", "type.scale", "name")]%>%
      filter(class=="G")
    print(dim(tedf))
    if(nrow(tedf)>0){
      tedf<-tedf[nrow(tedf) , c("type.scale", "name")]
    } else {tedf<-c(NA, NA)}
    return(tedf)
  })
  partie<-do.call("rbind", res)
  names(partie)<-c("num.part", "nom.part")
  #partie<-unique(partie)%>%filter(!is.na(nom.part))
  struct<-cbind(struct, partie)
  struct[]<-lapply(struct, function(x){
    if(class(x)=="character"){
      trimws(x, which = "both", whitespace = "[\\h\v]")->x
    }
    return(x)
  })
  
  subset(struct, struct$class=="G")->titles
  subset(struct, struct$class%in%c("Q", "SQ", "A"))->struct
  struct$tableau<-sapply(1:nrow(struct), function(i){
    subset(struct[1:i , ], struct[1:i , ]$class=="Q")->df.prec
    if(df.prec[nrow(df.prec) , "type.scale"]==":"){
      "tab"
    } else {"no"}
  })
  
  struct$DATA.NAMES<-sapply(1:nrow(struct), function(i){
    if(struct$class[i]%in%c("SQ")){
      subset(struct[1:i , ], struct[1:i , ]$class=="Q")->df.prec
      paste(df.prec[nrow(df.prec), "name"], ".", struct$name[i], ".", sep="")->res
    } else {
      res<-struct$name[i]
    }
    return(res)
  })
  
  if("tab"%in%struct$tableau){
    struct.tab<-filter(struct, tableau=="tab")
    struct.tab$DATA.NAMES<-sapply(1:nrow(struct.tab), function(i){
      if(struct.tab$class[i]%in%c("SQ")){
        subset(struct.tab[1:i , ], struct.tab[1:i , ]$class=="Q")->df.prec
        res<-df.prec[nrow(df.prec), "name"]
      } else {
        res<-struct.tab$name[i]
      }
      return(res)
    })
    tab.q<-lapply(unique(struct.tab$DATA.NAMES), function(x){
      unlist(filter(struct.tab, DATA.NAMES==x&class=="SQ"&type.scale==1)[ , "name"])
    })
    names(tab.q)<-unique(struct.tab$DATA.NAMES)
    struct.tab.list<-lapply(unique(struct.tab$DATA.NAMES), function(Q){
      subset(struct.tab, struct.tab$DATA.NAMES==Q)->tempQ
      res<-lapply(tab.q[[Q]], function(xQ){
        tempdf<-tempQ
        tempdf$DATA.NAMES<-sapply(1:nrow(tempdf), function(i){
          if(tempdf$type.scale[i]==""){
            paste(tempdf$DATA.NAMES[i], ".", tempdf$name[i], "_", xQ, ".", sep="")
          } else {tempdf$name[i]}
        })
        tempdf$tableau<-subset(tempdf, tempdf$type.scale==1&tempdf$name==xQ)$text
        tempdf<-filter(tempdf, type.scale!=1)
        tempdf$type.scale<-"M"
        return(tempdf)
      })
      print(res)
      res<-do.call("rbind", res)
      print(res)
      return(res)
    })
    struct.tab<-do.call("rbind", struct.tab.list)
    struct<-rbind(
      filter(struct, tableau!="tab"),
      struct.tab
    )
  }
  return(struct)
}