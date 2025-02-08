library(tidyverse)
library(rvest)
library(dplyr)
library(lubridate)
library(readr)
library(udpipe)

path <- "/Users/panjialalam/Documents/GitHub/2.-Web-Scraping-and-Text-Sentiment-Analysis/Saved articles/"

# WEB SCRAPING

# Functions starts here
get_articles <- function(input_year, input_month) {
  
  go_to_next_page <- 1
  pg_count <- 0
  links <- list()
  dates <- list()
  who_df_fin <- data.frame()
  while(go_to_next_page == 1) {
    if(pg_count == 0){
      url <-"https://www.afro.who.int/news/news-releases?page=0"
    }
    if(pg_count > 0){
      url <-paste0("https://www.afro.who.int/news/news-releases?page=", pg_count)
    }
    print(paste("Currently scraping page", pg_count + 1))
    
    # Extract every article's hyperlink in currently scraped page
    response <- read_html(url)
    page_links <- response |> html_elements(".teaser-long__link") |> html_attr("href")
    links <- c(links, page_links)
    hyperlinks <- paste0("https://www.afro.who.int", links)
    
    # Extract every article's date in currently scraped page
    page_dates <- response |> html_elements(".date") |> html_text()
    
    page_dates <- page_dates %>%
      gsub('\n', '', .) %>%
      str_trim() %>%
      as.Date(format = "%d %B %Y")
    
    dates <- c(as.character(dates), as.character(page_dates))
    
    # Combine into a data frame
    who_df <- data.frame(cbind(hyperlinks, dates))
    
    who_df <- who_df |>
      mutate(
        date_sep = dates
      ) |> 
      separate(date_sep, into = c("year", "month", "day"), sep = "-") |>
      mutate(year = as.numeric(year),
             month = as.numeric(month),
             day = as.numeric(day),
             dates = make_date(year, month, day))
    
    who_df_fin <- who_df |>
      filter(dates >= make_date(input_year, input_month))
    
    # Check the stop point based on the date
    current_year_obs <- who_df$year[nrow(who_df)]
    print(paste("Last article - Year:", current_year_obs))
    current_month_obs <- who_df$month[nrow(who_df)]
    print(paste("Last article - Month:", current_month_obs))
    if(current_year_obs < input_year){
      go_to_next_page <- 0
    }
    if(current_year_obs == input_year){
      if(current_month_obs < input_month){
        go_to_next_page <- 0 
      }
    }
    pg_count <- pg_count + 1
    
    print(paste("After scraping page", pg_count, "we will go to page", pg_count + 1))
  }
  
  # Loop to scrape the articles
  saved_text <- list()
  all_text <- list()
  for(i in 1:nrow(who_df_fin)) {
    hyperlinks <- who_df_fin$hyperlinks
    response <- read_html(hyperlinks[i])
    
    title <- response |> 
      html_elements("h1.page-header span") |> 
      html_text() |> first()
    
    text <- response |> 
      html_elements("p") |>
      html_text()
    
    full_text <- c(paste0(title, "."), text)
    
    # Clean the text from empty text, links, contact information
    full_text <- full_text[full_text != "" & !grepl("\\[at\\]", full_text)]
    cleaned_text <- gsub('\\s+', ' ', str_trim(full_text))
    saved_text[[i]] <- cleaned_text
    
    # Combined all text to one
    combined_text <- unlist(saved_text)
    all_text[[i]] <- combined_text
    
    # Save to text files
    clean_title <- gsub("\\s+$", "", title)
    file_name <- paste0(path, "WHO Article_", i, "_", clean_title, ".txt")
    writeLines(cleaned_text, file_name)
    
    combined_name <- paste0(path, "All WHO Article from today to ", input_year, "-", input_month, ".txt")
    writeLines(combined_text, combined_name)
  }
  return(saved_text)
  return(all_text)
  
}
# Function ends here

# Test the function
get_articles(2024, 1)

# SENTIMENT ANALYSIS (OVERALL)
text_path <- "/Users/panjialalam/Documents/GitHub/2.-Web-Scraping-and-Text-Sentiment-Analysis/Saved articles/"
full_text <- read_lines(paste0(text_path, "All WHO Article from today to 2024-1.txt"))
full_text_df <- data.frame(text = full_text)

# Clean the lemmas and remove the stopwords
full_lemmas <- udpipe(full_text, "english")

full_lemmas <- full_lemmas |>
  mutate(lemma = str_to_lower(lemma)) |> # Lower case for lemma
  filter(upos!= "PUNCT" & upos != "PART" & lemma != "'s") |>
  anti_join(stop_words, by = c("lemma" = "word"))

# Obtain the sentiments
sentiment_nrc   <- get_sentiments("nrc") |> rename(nrc = sentiment)
sentiment_afinn <- get_sentiments("afinn") |> rename(afinn = value)
sentiment_bing  <- get_sentiments("bing") |> rename(bing = sentiment)

# Preparation for comparison sentiment analysis with a specific country
sentence_tokens_full <- unnest_tokens(full_text_df, sent_tokens, text, token = "sentences")

bigrams_full <- unnest_tokens(full_text_df, bigrams, text, token = "ngrams", n = 2) # Bigrams for valence shifter

bigrams_full_sep <- bigrams_full |>
  separate(bigrams, c("word1", "word2"), sep = " ") |>
  left_join(sentiment_bing, by = c("word2" = "word")) |>
  mutate(adjusted_bing = case_when(
    word1 == "not" & bing == "positive" ~ "negative",
    word1 == "not" & bing == "negative" ~ "positive",
    TRUE ~ bing
  ))

# Sentiment analysis on word level
word_lemmas <- full_lemmas |>
  select(lemma)

word_lemmas_joined <- word_lemmas |>
  left_join(sentiment_nrc, by = c("lemma" = "word")) |>
  left_join(sentiment_afinn, by = c("lemma" = "word")) |>
  left_join(sentiment_bing, by = c("lemma" = "word"))

# NRC
q2_nrc <- ggplot(filter(word_lemmas_joined, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count", fill = "steelblue") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "WHO Article Sentiment (NRC): Jan 2024 - Today",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()

q2_nrc

# Descriptive statistics
word_lemmas_joined|>
  select(nrc) |>
  na.omit() |> 
  group_by(nrc) |>
  summarize(
    count = n()
  ) |>
  arrange(desc(count))

# AFINN
q2_afinn <- ggplot(filter(word_lemmas_joined, !is.na(afinn))) +
  geom_histogram(aes(afinn, fill = factor(afinn > 0)), stat = "count", binwidth = 1) +
  scale_fill_manual(name = "Sentiment",
                    values = c("FALSE" = "firebrick", "TRUE" = "steelblue"),
                    labels = c("Negative", "Positive")) +
  scale_x_continuous(n.breaks = 7) +
  labs(
    title = "WHO Article Sentiment (AFINN): Jan 2024 - Today",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()

q2_afinn

# Descriptive statistics
afinn_words <- word_lemmas_joined |>
  select(lemma, afinn) |>
  na.omit()

summary(afinn_words)

# SENTIMENT ANALYSIS (COUNTRY: UGANDA)

# A function to get an article with a country in the title
article_country <- function(input_country) {
  file_path <- "/Users/panjialalam/Documents/GitHub/2.-Web-Scraping-and-Text-Sentiment-AnalysisSaved articles"
  txt_files <- list.files(file_path, pattern = "\\.txt$", full.names = TRUE)
  
  matching_files <- txt_files[grepl("uganda", txt_files, ignore.case = TRUE)]
  return(matching_files)
}

# Use the function
country_article <- article_country("uganda")

country_text <- read_lines(country_article)
country_text_df <- data.frame(text = country_text)

# Word level analysis (BING)
bigrams_country <- unnest_tokens(country_text_df, bigrams, text, token = "ngrams", n = 2) # Bigrams for valence shifter

bigrams_country_sep <- bigrams_country |>
  separate(bigrams, c("word1", "word2"), sep = " ") |>
  left_join(sentiment_bing, by = c("word2" = "word")) |>
  mutate(adjusted_bing = case_when(
    word1 == "not" & bing == "positive" ~ "negative",
    word1 == "not" & bing == "negative" ~ "positive",
    TRUE ~ bing
  ))

bigrams_combined <- rbind(bigrams_full_sep |> mutate(category = "All Articles"),
                          bigrams_country_sep |> mutate(category = "Uganda"))

bigrams_proportions <- bigrams_combined |>
  na.omit() |>
  count(category, adjusted_bing) |>
  group_by(category) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

head(bigrams_combined[bigrams_combined$bing != bigrams_combined$adjusted_bing & !is.na(bigrams_combined$bing),])
# There are changes due to valence shift

q2_bing <- ggplot(bigrams_proportions, aes(x = category, 
                                y = proportion, 
                                fill = adjusted_bing)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Proportion", 
       x = "Category", 
       fill = "Sentiment") +
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "steelblue")) +
  labs(
    title = "WHO Articles Comparison (BING)"
  ) +
  theme_minimal()

q2_bing

# Saved as pictures
ggsave(paste0(read_path, "Pic 5 - Africa News NRC.jpg"), plot = q2_nrc)
ggsave(paste0(read_path, "Pic 6 - Africa News AFINN.jpg"), plot = q2_afinn)
ggsave(paste0(read_path, "Pic 7 - Africa News BING Country Comparison.jpg"), plot = q2_bing)
