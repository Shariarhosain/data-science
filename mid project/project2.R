library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(textstem)
library(tidytext)
library(hunspell)

setwd("D:/vercity/Class12/INTRODUCTION TO DATA SCIENCE [A]/MID/Project")

url <- "https://www.kippy.cloud/post/ways-technology-supercharges-strategic-decision-making"
webpage <- read_html(url)

text_data <- webpage %>%
  html_nodes("p") %>%
  html_text()

text_data <- paste(text_data, collapse = " ")

clean_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "[^[:alnum:][:space:]]", "")
  text <- str_squish(text)
  return(text)
}

cleaned_text <- clean_text(text_data)

tokens <- unlist(str_split(cleaned_text, "\\s+"))

data("stop_words")
tokens_no_stop <- tokens[!tokens %in% stop_words$word]

stemmed_tokens <- wordStem(tokens_no_stop)

lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)

handle_contractions <- function(text) {
  text <- str_replace_all(text, "can't", "cannot")
  text <- str_replace_all(text, "won't", "will not")
  text <- str_replace_all(text, "it's", "it is")
  text <- str_replace_all(text, "I'm", "I am")
  return(text)
}

cleaned_text_with_contractions <- handle_contractions(cleaned_text)

cleaned_text_no_emojis <- str_replace_all(cleaned_text_with_contractions, "[\U0001F600-\U0001F64F]", "")

corrected_tokens <- hunspell::hunspell_check(tokens)

final_text <- paste(corrected_tokens, collapse = " ")

final_data <- data.frame(
  cleaned_text = cleaned_text,
  tokens = paste(tokens, collapse = " "),
  cleaned_text_normalized = cleaned_text,
  tokens_no_stop_words = paste(tokens_no_stop, collapse = " "),
  stemmed_tokens = paste(stemmed_tokens, collapse = " "),
  lemmatized_tokens = paste(lemmatized_tokens, collapse = " "),
  cleaned_text_with_contractions = cleaned_text_with_contractions,
  cleaned_text_no_emojis = cleaned_text_no_emojis,
  final_text_with_spell_check = final_text
)

write.csv(final_data, "project.csv", row.names = FALSE)


