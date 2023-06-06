#' @export 
text_td_fr<-function(var, stopwords=NULL, ud_model=NULL, ... ){
   if(is.null(stopwords)){
   stop("generate stopwords")
   stopwords<-tidystopwords::generate_stoplist(language = "French")
   stopwords <- data.frame(word = stopwords, stringsAsFactors = FALSE)
   }
  
   if(is.null(ud_model)){
   stop("generate ud_model")
   ud_model.0 <- udpipe::udpipe_download_model(language = "french")
   ud_model <- udpipe::udpipe_load_model(ud_model.0)
   }

   ud_twistFR <- udpipe::udpipe_annotate(ud_model, var, parallel.cores = 3, ... )
   ud_twistFR_df <- data.frame( ud_twistFR, stringsAsFactors = FALSE )
   ud_twistFR_df <- ud_twistFR_df %>%
     mutate( token = tolower(token) ) %>%
     anti_join( stopwords, by = c("token" = "word") )%>%
     filter(upos != "PUNCT")  
   res<-ud_twistFR_df%>%as_tibble()
   return(res)
}