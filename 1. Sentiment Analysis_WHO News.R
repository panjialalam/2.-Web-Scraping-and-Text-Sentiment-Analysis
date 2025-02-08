library(tidyverse)
library(rvest)
library(tidytext)
library(textdata)
library(sentimentr)
library(readr)
library(countrycode)

read_path <- "/Users/panjialalam/Documents/GitHub/2.-Web-Scraping-and-Text-Sentiment-Analysis/"
text <- read_lines(paste0(read_path, "vol101_1_publichealthroundup.txt"))

text <- text[text != "" & !grepl("Go to:", text) & !grepl("Cover photo", text)]

# Sentiment Analysis
text_df <- data.frame(text = text)
text_words <- unnest_tokens(text_df, word_tokens,  text, token = "words")

# Obtain the sentiments
sentiment_nrc   <- get_sentiments("nrc") |> rename(nrc = sentiment)
sentiment_afinn <- get_sentiments("afinn") |> rename(afinn = value)
sentiment_bing  <- get_sentiments("bing") |> rename(bing = sentiment)

# Bigrams to check for any valence shifter
bigrams_text <- unnest_tokens(text_df, bigrams, text, token = "ngrams", n = 2)

bigrams_text <- bigrams_text |>
  separate(bigrams, c("word1", "word2"), sep = " ") |>
  left_join(sentiment_bing, by = c("word2" = "word")) |>
  mutate(adjusted_bing = case_when(
    word1 == "not" & bing == "positive" ~ "negative",
    word1 == "not" & bing == "negative" ~ "positive",
    TRUE ~ bing
  ))

head(bigrams_text[bigrams_text$bing != bigrams_text$adjusted_bing & !is.na(bigrams_text$bing),])
# There is no change due to valence shift

# Clean the words
text_words <- text_words |>
  anti_join(stop_words, by = c("word_tokens" = "word")) |>
  filter(is.na(as.numeric(word_tokens)))

text_words_joined <- text_words |>
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) |>
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) |>
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

# Word frequency
freq_words <- text_words |>
  group_by(word_tokens) %>%
  summarise(num = n())

slice_max(freq_words, order_by = num, n = 5)

# NRC
q1_nrc <- ggplot(filter(text_words_joined, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count", fill = "steelblue") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Public Health Round-up Vol.101 Jan, 2023 (NRC)",
    x = "Sentiment",
    y = "Count"
  ) +
  theme_minimal()

q1_nrc

# Descriptive statistics
text_words_joined |>
  select(nrc) |>
  na.omit() |> 
  group_by(nrc) |>
  summarize(
    count = n()
  ) |>
  arrange(desc(count))

# AFINN
q1_afinn <- ggplot(filter(text_words_joined, !is.na(afinn))) +
  geom_histogram(aes(afinn, fill = factor(afinn > 0)), stat = "count", binwidth = 1) +
  scale_fill_manual(name = "Sentiment",
                    values = c("FALSE" = "firebrick", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive")) +
  scale_x_continuous(n.breaks = 7) +
  labs(
    title = "Public Health Round-up Vol.101 Jan, 2023 (AFINN)",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()

q1_afinn

# Descriptive statistics
afinn_words <- text_words_joined |>
  select(word_tokens, afinn) |>
  na.omit()

summary(afinn_words)

# BING
q1_bing <- ggplot(filter(text_words_joined, !is.na(bing))) +
  geom_histogram(aes(bing, fill = factor(bing == "positive")), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(name = "Sentiment",
                    values = c("FALSE" = "firebrick", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive")) +
  labs(
    title = "Public Health Round-up Vol.101 Jan, 2023 (BING)",
    x = "Sentiment",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

q1_bing

# Descriptive statistics
text_words_joined|>
  select(bing) |>
  na.omit() |> 
  group_by(bing) |>
  summarize(
    count = n()
  ) |>
  arrange(desc(count))

# Sentiment R
sentences_token <- text_df |>
  unnest_tokens(sent_tokens, text, token = "sentences")

sentiment_r <- data.frame(sentiment(sentences_token$sent_tokens))
sentiment_r <- sentiment_r |>
  distinct(element_id, .keep_all = TRUE) # Choose the first from duplicated element_id
sentences_token <- cbind(sentences_token, sentiment_r)

mean_sentiment <- mean(sentences_token$sentiment, na.rm = TRUE)
mean_sentiment

q1_sentimentr <- ggplot(data = sentences_token) +
  geom_density(aes(sentiment), fill = "skyblue", color = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_sentiment), color = "firebrick", linetype = "dashed", linewidth = 0.75) +
  labs(
    title = "Public Health Round-up Vol.101 Jan, 2023 (SentimentR)", 
    x = "Sentiment", 
    y = "Density", 
    caption = "Note: All articles vs. Madagascar article") +
  theme_minimal()

q1_sentimentr

# Descriptive statistics
sentimentr_sent <- sentences_token |>
  select(sent_tokens, sentiment) |>
  na.omit()

summary(sentimentr_sent)

# Countries discussed
country_names <- countrycode::codelist$country.name.en
words_list <- udpipe(text, "english") |> select(token) |> pull(token)

countries <- words_list[words_list %in% country_names]
unique(countries)

# Saved as pictures
ggsave(paste0(read_path, "Pic 1 - News NRC.jpg"), plot = q1_nrc)
ggsave(paste0(read_path, "Pic 2 - News AFINN.jpg"), plot = q1_afinn)
ggsave(paste0(read_path, "Pic 3 - News BING.jpg"), plot = q1_bing)
ggsave(paste0(read_path, "Pic 4 - News SentimentR.jpg"), plot = q1_sentimentr)
