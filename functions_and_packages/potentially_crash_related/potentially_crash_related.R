# Classify potentially crash related tweets

fuzzy_match <- function(tweet, words, maxDist){
  if(nchar(tweet) >= 2){
    tweet_words <- ngram::ngram_asweka(tweet, min=1, max=1) 
    contains_accident_word <- TRUE %in% ain(tweet_words, words, maxDist=maxDist)
  } else{
    contains_accident_word <- FALSE
  }
  return(contains_accident_word)
}

class_potnt_crash <- function(text){
  # Determine if a tweet is potentially accident related
  
  text_df <- text %>%
    as.data.frame() %>%
    dplyr::rename(text = ".")
  text_df$text <- text_df$text %>% as.character()
  
  # Search for words (space before/after)
  accident_words <- c("accident",
                      "accidents",
                      "wreckage",
                      "wreck",
                      "collision",
                      "crash",
                      "crashs",
                      "crashes",
                      "ajali",
                      "magari zmegongana",
                      "zimecrash",
                      "zilicrash",
                      "disaster",
                      "hazard",
                      "pileup",
                      "fender-bender",
                      "fender bender",
                      "smash",
                      "smashed",
                      "axident",
                      "fatality",
                      "mishap",
                      "injuries",
                      "injury",
                      "incident",
                      "incidents",
                      "emergency",
                      "hit",
                      "fatal",
                      "damage",
                      "hit-and-run",
                      "overturn",
                      "overturned",
                      "ovrturn",
                      "ovrturned",
                      "rolled",
                      "roll",
                      "read end",
                      "rear ended",
                      "rammed",
                      "crush",
                      "crushed")
  accident_words <- paste0("\\b", accident_words, "\\b") %>% paste(collapse = "|") 
  
  text_df$potentially_accident_related_exact <- str_detect(text_df$text, accident_words)
  text_df$potentially_accident_related_char_exact <- grepl("accident|acident|incident|axident|crash|collision|pileup|wreck|smash|\\bhit\\b|\\bhits\\b|knock|collide|injury|fatality|injuries|fatalities|overturn|damage|hit-and-run|fender bender|fender-bender", text_df$text)
  
  text_df$potentially_accident_related_fuzzy_1 <- lapply(text_df$text,fuzzy_match, c("accident","incident"), 2) %>% unlist
  text_df$potentially_accident_related_fuzzy_2 <- lapply(text_df$text,fuzzy_match, c("crash"), 1) %>% unlist
  
  text_df$potentially_accident_related <- text_df$potentially_accident_related_exact | text_df$potentially_accident_related_char_exact | text_df$potentially_accident_related_fuzzy_1 | text_df$potentially_accident_related_fuzzy_2
  
  return(text_df$potentially_accident_related)
}


