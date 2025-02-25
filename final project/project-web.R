library(rvest)
library(tm)
library(SnowballC)
library(textstem)
library(textclean)
library(hunspell)
library(topicmodels)
library(stringr)
library(dplyr)
library(stopwords)
library(tidytext)
library(ggplot2)

url <- "https://www.kippy.cloud/post/ways-technology-supercharges-strategic-decision-making"
webpage <- read_html(url)
page_text <- webpage %>% html_nodes("p") %>% html_text() %>% paste(collapse = " ")

cleaned_text <- tolower(page_text)

tokens <- unlist(strsplit(cleaned_text, "\\s+"))

tokens <- gsub("[^[:alpha:]]", "", tokens)
tokens <- tokens[tokens != ""]
tokens <- tokens[nchar(tokens) > 2]

data("stop_words")
tokens <- tokens[!(tokens %in% stop_words$word)]

stemmed_tokens <- wordStem(tokens)

lemmatized_tokens <- lemmatize_words(tokens)

final_cleaned_text <- paste(lemmatized_tokens, collapse = " ")

cleaned_text <- replace_contraction(cleaned_text)
cleaned_text <- replace_emoji(cleaned_text)
cleaned_text <- replace_emoticon(cleaned_text)

spell_check <- hunspell_check(tokens)
misspelled_words <- tokens[!spell_check]

if (length(misspelled_words) > 0) {
  suggestions <- hunspell_suggest(misspelled_words)
  corrected_tokens <- tokens
  for (i in seq_along(misspelled_words)) {
    if (length(suggestions[[i]]) > 0) {
      corrected_tokens[which(tokens == misspelled_words[i])] <- suggestions[[i]][1]
    }
  }
} else {
  corrected_tokens <- tokens
}

tokens <- corrected_tokens

steps <- list(
  "Text Cleaning" = unlist(str_split(cleaned_text, "\\s+")),
  "Tokenization" = tokens,
  "Normalization" = tokens,
  "Stop Words Removal" = tokens,
  "Spell Checking" = tokens,
  "Stemming" = stemmed_tokens,
  "Lemmatization" = lemmatized_tokens
)

max_rows <- max(sapply(steps, length))
steps_padded <- lapply(steps, function(step) {
  c(step, rep("", max(0, max_rows - length(step))))
})
output_df <- as.data.frame(steps_padded, stringsAsFactors = FALSE)
output_file <- "D:mypic/preprocessed_text.csv"

write.csv(output_df, output_file, row.names = FALSE)




corpus <- VCorpus(VectorSource(final_cleaned_text))
dtm <- DocumentTermMatrix(corpus)

dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
inspect(dtm_tfidf)


num_topics <- 2
lda_model <- LDA(dtm, k = num_topics)
terms(lda_model, 100)


terms_per_topic <- terms(lda_model, 6)
cat("Most probable words for each topic:\n")
print(terms_per_topic)




doc_topic_probs <- posterior(lda_model)$topics
cat("Topic proportions for the document:\n")
print(doc_topic_probs)

terms_per_topic_list <- as.list(as.data.frame(terms_per_topic))
terms_df <- data.frame(
  Topic = rep(1:num_topics, each = nrow(terms_per_topic)),
  Term = unlist(terms_per_topic_list),
  stringsAsFactors = FALSE
)


terms_df$Value <- rep(seq(nrow(terms_per_topic), 1), times = num_topics)

ggplot(terms_df, aes(x = reorder(Term, -Value), y = Value, fill = as.factor(Topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Topic, scales = "free") +
  labs(title = "Top Terms per Topic", x = "Terms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

