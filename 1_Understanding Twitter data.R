# stream_tweets() samples 1% of all publicly available tweets; 30 second time interval by default
# example: 
# live_tweets <- stream_tweets("")  # add timeout parameter to set a different time interval
# dim(live_tweets)  # shows a number representing 1% of live tweets in 30 seconds; shows number of columns in the dataframe

# twitter data can be used for seeing trending topics, evaluating customer opinion about a brand, analyzing public sentiment about politics, visualizing the reach of something, and detecting events

library(rtweet)
library(httpuv)
library(dplyr)

# Extract live tweets for 120 seconds window
tweets120s <- stream_tweets("", timeout = 120)

# View dimensions of the data frame with live tweets
dim(tweets120s)

# search_tweets() returns twitter data from the past 7 days matching a search query
# search_tweets("your_query", n = pick_a_number, include_rts = TRUE, lang = "en")
search_tweets("#rstats", n = 5) 

# get_timeline() extracts tweets posted by a specific twitter user
get_timeline("@katyperry", n = 5)

# In this exercise, you will extract tweets on the Emmy Awards which are American awards that recognize excellence in the television industry, by looking for tweets containing the Emmy Awards hashtag.
# Extract tweets on "#Emmyawards" and include retweets
twts_emmy <- search_tweets("#Emmyawards", n = 200, include_rts = TRUE, lang = "en")

# View output for the first 5 columns and 10 rows
head(twts_emmy[,1:5], 10)

# In this exercise, you will extract tweets posted by Cristiano Ronaldo, a very popular soccer player both on the field and on social media who has the @Cristiano twitter handle.
# Extract tweets posted by the user @Cristiano
get_cris <- get_timeline("@Cristiano", n = 320)

# View output for the first 5 columns and 10 rows
head(get_cris[,1:5], 10)

# To identify twitter users who are interested in a topic, you can look at users who tweet often on that topic. The insights derived can be used to promote targeted products to interested users.
# In this exercise, you will identify users who have tweeted often on the topic "Artificial Intelligence". 
# Create a table of users and tweet counts for the topic
sc_name <- table(tweets_ai$screen_name)  # tweets_ai dataset isn't loaded here so code won't work

# Sort the table in descending order of tweet counts
sc_name_sort <- sort(sc_name, decreasing = TRUE)

# View sorted table for top 10 users
head(sc_name_sort, 10)

# The follower count for a twitter account indicates the popularity of the personality or a business entity and is a measure of influence in social media. 
# Knowing the follower counts helps digital marketers strategically position ads on popular twitter accounts for increased visibility. 
# In this exercise, you will extract user data and compare followers count for twitter accounts of four popular news sites: CNN, Fox News, NBC News, and New York Times.
# Extract user data for the twitter accounts of 4 news sites
users <- lookup_users("nytimes", "CNN", "FoxNews", "NBCNews")

# Create a data frame of screen names and follower counts
user_df <- users[,c("screen_name","followers_count")]

# Display and compare the follower counts for the 4 news sites
user_df

# A retweet helps utilize existing content to build a following for your brand.
# The number of times a twitter text is retweeted indicates what is trending. The inputs gathered can be leveraged by promoting your brand using the popular retweets.
# In this exercise, you will identify tweets on "Artificial Intelligence" that have been retweeted the most. 
# Create a data frame of tweet text and retweet count
rtwt <- tweets_ai[,c("text", "retweet_count")]  # tweets_ai dataset isn't loaded here so code won't work
head(rtwt)

# Sort data frame based on descending order of retweet counts
rtwt_sort <- arrange(rtwt, desc(retweet_count))

# Exclude rows with duplicate text from sorted data frame
rtwt_unique <- unique(rtwt_sort, by = "text")

# Print top 6 unique posts retweeted most number of times
rownames(rtwt_unique) <- NULL
head(rtwt_unique)