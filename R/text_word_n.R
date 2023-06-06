#' @export 
text_word_n<-function(text_df, var_gr="lemma", ... ){
   res<-text_df%>%
     group_by(across(one_of(var_gr)), ... )%>%
     summarise(nb = n())%>%arrange(desc(nb))%>%
     left_join(text_df%>%
                 group_by(across(one_of("doc_id", var_gr)), ... )%>%
                 summarise(TEST = n()>0)%>%
                 group_by(across(one_of(var_gr)), ... )%>%
                 ungroup(doc_id)%>%
                 summarise(nb_ind = sum(TEST)))
   return(res)
}