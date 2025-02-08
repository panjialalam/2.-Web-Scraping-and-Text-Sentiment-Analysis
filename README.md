=== SENTIMENT ANALYSIS FOR ONE ARTICLE ===

Providing the report, I started by parsing the text at the word level for the analysis. I parsed using the unnest_tokens function for the word-level analysis. Based on the parsed text, the five words with the highest frequency are health, report, December, countries, and infections. Then, I checked for potential valence shifts using bigrams. Then, I joined the words to the three sentiment measurements.

In my analysis of word-level sentiment using NRC, I observed that positive and negative sentiments are prominently represented among words. Following this, I also noted that fear, trust, and sadness were among the top five most common sentiments. Using AFINN and BING, I found significantly more words with negative sentiments.

I tokenized the sentences using the unnest_tokens function to obtain the sentences. Within sentence-level analysis using SentimentR, I observed more negative sentiment sentences, and the mean is -0.166. In conclusion, the January 2023 report mostly has negative overall sentiments.

I used lemmatization to get the lemma and look for the countries. Then, the list of words from the lemma is checked with the country name from the country code library. The countries discussed are Oman, Switzerland, Sudan, Uganda, and Kenya.

Online sources:

•	Change histogram fill color: https://learn.saylor.org/mod/book/view.php?id=58485&chapterid=45033

•	Find country: https://stackoverflow.com/questions/70787674/is-it-possible-to-get-r-to-identify-countries-in-a-dataframe

-----------------------------------------------------------------------------------------------------------------------
  
=== WEB SCRAPING AND SENTIMENT ANALYSIS FOR ALL WHO ARTICLES (AFRICA REGION) ===

The scraping process has two essential processes: 1). Obtain the hyperlinks and date using the loop mechanism stopped at the year and month designated, and 2). Scrape all the articles based on the available hyperlinks filtered based on the filtered date. I employed the while loop as it will stop the scraping. The essential part of the code is when I need to add new scraped hyperlinks and dates to the stored data. Then, I saved all of them in Txt files with one additional file containing all the texts.

For the text-analysis, I utilized lemmatization as it can generate base words accurately which will later combined with the sentiment categories.

Based on the NRC, positive and trust sentiments are the highest, followed by negative, fear, and anticipation. Using AFINN, there were more negative sentiments in the overall texts. The tones are mixed between those sentiments because I noted a balanced number of words.

I chose Uganda as the country analysis and compared it with overall articles, and I prepared a function to get a country-specific article.
  
Online sources:

•	Accumulate data from inside a loop: https://stackoverflow.com/questions/28553872/in-r-how-do-you-accumulate-data-from-inside-a-loop-to-a-numeric-vector-above-th

•	Remove space in the end of a sentence string: https://stackoverflow.com/questions/28553872/in-r-how-do-you-accumulate-data-from-inside-a-loop-to-a-numeric-vector-above-th 

•	Combine all vector lists into one: https://www.r-bloggers.com/2023/08/the-unlist-function-in-r/ 
