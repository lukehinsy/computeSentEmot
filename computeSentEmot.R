DATASET<-OSUdiss.raw

computeSentEmot <- function(DATASET, ID, textcol) {
  require(dplyr)
  require(tidyr)
  require(sentimentr)
  
  DATASET <- DATASET %>% 
    dplyr::mutate(element_id = row_number())
  
  DATASET_spl <- DATASET %>% 
    dplyr::select(element_id, {{textcol}}) %>% 
    dplyr::mutate(Exp_split = sentimentr::get_sentences({{textcol}}))
  
  DATASET_sent <- DATASET_spl %$% 
    sentimentr::sentiment_by(Exp_split) %>% 
    dplyr::right_join(DATASET, by=c('element_id'))
  
  DATASET_sent <- DATASET_sent[,c(1,5,6,7,8,2,4,3)]
  
  DATASET_emot <- DATASET_spl %$% 
    sentimentr::emotion_by(Exp_split) %>% 
    dplyr::filter(grepl("_neg", emotion_type, fixed = TRUE) == FALSE & !is.na(emotion_type)) %>% 
    dplyr::right_join(DATASET, by = c('element_id')) %>% 
    tidyr::pivot_wider(names_from = emotion_type, values_from = c(ave_emotion, emotion_count, sd))
  
  DATASET_return <- DATASET %>% 
    dplyr::left_join(DATASET_sent %>% 
                dplyr::select(element_id,
                              word_count,
                              ave_sentiment,
                              sd_sentiment = sd)) %>%
    dplyr::left_join(DATASET_emot %>% 
                       dplyr::select(element_id, 
                                     dplyr::starts_with('ave_e'),
                                     dplyr::starts_with('emotion_co'),
                                     dplyr::starts_with('sd_'))) %>%
    dplyr::select(-element_id)
  
  return(DATASET_return)
}
