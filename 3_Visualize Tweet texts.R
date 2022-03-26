# Tweet text posted by twitter users is unstructured, noisy, and raw. 
# It contains emoticons, URLs, and numbers. This redundant information has to be cleaned before analysis in order to yield reliable results. 

library(qdapRegex)
library(tm)
library(qdap)
library(ggplot2)
library(syuzhet)

# In this exercise, you will remove URLs and replace characters other than letters with spaces.
# The tweet data frame twt_telmed, with 1000 extracted tweets on "telemedicine", has been pre-loaded for this exercise. 
# Extract tweet text from the pre-loaded dataset
twt_txt <- twt_telmed$text
head(twt_txt)

# Remove URLs from the tweet text and view the output
twt_txt_url <- rm_twitter_url(twt_txt)
head(twt_txt_url)

# Replace special characters, punctuation, & numbers with spaces
twt_txt_chrs  <- gsub("[^A-Za-z]"," " , twt_txt_url)

# View text after replacing special characters, punctuation, & numbers
head(twt_txt_chrs)

# A corpus is a list of text documents. You have to convert the tweet text into a corpus to facilitate subsequent steps in text processing.
# When analyzing text, you want to ensure that a word is not counted as two different words because the case is different in the two instances. Hence, you need to convert text to lowercase. 
# In this exercise, you will create a text corpus and convert all characters to lower case.
# Convert text in "twt_gsub" dataset to a text corpus and view output
twt_corpus <- twt_gsub %>% 
  VectorSource() %>% 
  Corpus() 
head(twt_corpus$content)

# Convert the corpus to lowercase
twt_corpus_lwr <- tm_map(twt_corpus, tolower) 

# View the corpus after converting to lowercase
head(twt_corpus_lwr$content)

# The text corpus usually has many common words like a, an, the, of, and but. These are called stop words.
# Stop words are usually removed during text processing so one can focus on the important words in the corpus to derive insights. 
# Also, the additional spaces created during the removal of special characters, punctuation, numbers, and stop words need to be removed from the corpus.
# Remove English stop words from the corpus and view the corpus
twt_corpus_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english"))
head(twt_corpus_stpwd$content)

# Remove additional spaces from the corpus
twt_corpus_final <- tm_map(twt_corpus_stpwd, stripWhitespace)

# View the text corpus after removing spaces
head(twt_corpus_final$content)

# Popular terms in a text corpus can be visualized using bar plots or word clouds.
# However, it is important to remove custom stop words present in the corpus first before using the visualization tools.
# Extract term frequencies for top 60 words and view output
termfreq <- freq_terms(twt_corpus, 60)
termfreq

# Create a vector of custom stop words
custom_stopwds <- c("telemedicine", " s", "amp", "can", "new", "medical", 
                    "will", "via", "way",  "today", "come", "t", "ways", 
                    "say", "ai", "get", "now")

# Remove custom stop words and create a refined corpus
corp_refined <- tm_map(twt_corpus,removeWords, custom_stopwds) 

# Extract term frequencies for the top 20 words
termfreq_clean <- freq_terms(corp_refined, 20)
termfreq_clean

# Extract term frequencies for the top 10 words
termfreq_10w <- freq_terms(corp_refined, 10)
termfreq_10w

# Identify terms with more than 60 counts from the top 10 list
term60 <- subset(termfreq_10w, FREQ > 60)

# Create a bar plot using terms with more than 60 counts
ggplot(term60, aes(x = reorder(WORD, -FREQ), y = FREQ)) + 
  geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# Extract term frequencies for the top 25 words
termfreq_25w <- freq_terms(corp_refined, 25)
termfreq_25w

# Identify terms with more than 50 counts from the top 25 list
term50 <- subset(termfreq_25w, FREQ > 50)

# Create a bar plot using terms with more than 50 counts
ggplot(term50, aes(x = reorder(WORD, -FREQ), y = FREQ)) + 
  geom_bar(stat = "identity", fill="blue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# A word cloud is an image made up of words in which the size of each word indicates its frequency. 
# Create a word cloud in red with min frequency of 20
wordcloud(corp_refined, min.freq = 20, colors = "red", 
          scale = c(3,0.5),random.order = FALSE)

# Create word cloud with 6 colors and max 50 words
wordcloud(corp_refined, max.words = 50, 
          colors = brewer.pal(6, "Dark2"), 
          scale=c(4,1), random.order = FALSE)

# The document term matrix or DTM is a matrix representation of a corpus.
# Creating the DTM from the text corpus is the first step towards building a topic model.
# Can you create a DTM from the pre-loaded corpus on "Climate change" called corpus_climate?
# Create a document term matrix (DTM) from the pre-loaded corpus
dtm_climate <- DocumentTermMatrix(corpus_climate)
dtm_climate

# Find the sum of word counts in each document
rowTotals <- apply(dtm_climate, 1, sum)
head(rowTotals)

# Select rows with a row total greater than zero
dtm_climate_new <- dtm_climate[rowTotals > 0, ]
dtm_climate_new

# Topic modeling is the task of automatically discovering topics from a vast amount of text.
# You can create topic models from the tweet text to quickly summarize the vast information available into distinct topics and gain insights.

# Create a topic model with 5 topics
topicmodl_5 <- LDA(dtm_climate_new, k = 5)

# Select and view the top 10 terms in the topic model
top_10terms <- terms(topicmodl_5,10)
top_10terms 

# Create a topic model with 4 topics
topicmodl_4 <- LDA(dtm_climate_new, k = 4)

# Select and view the top 6 terms in the topic model
top_6terms <- terms(topicmodl_4, 6)
top_6terms 

# Sentiment analysis is useful in social media monitoring since it gives an overview of people's sentiments.
# Climate change is a widely discussed topic for which the perceptions range from being a severe threat to nothing but a hoax.
# In this exercise, you will perform sentiment analysis and extract the sentiment scores for tweets on "Climate change". 
# Perform sentiment analysis for tweets on `Climate change` 
sa.value <- get_nrc_sentiment(tweets_cc$text)

# View the sentiment scores
head(sa.value, 10)

# Calculate sum of sentiment scores
score <- colSums(sa.value[,])

# Convert the sum of scores to a data frame
score_df <- data.frame(score)

# Convert row names into 'sentiment' column and combine with sentiment scores
score_df2 <- cbind(sentiment = row.names(score_df),  
                   score_df, row.names = NULL)
print(score_df2)

# Plot the sentiment scores
ggplot(data = score_df2, aes(x = sentiment, y = score, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))