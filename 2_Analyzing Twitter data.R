# An original tweet is an original posting by a twitter user and is not a retweet, quote, or reply. 
# The "-filter" can be combined with a search query to exclude retweets, quotes, and replies during tweet extraction.

library(plyr)
library(dplyr)
library(rtweet)
library(reshape)
library(ggplot2)

# In this exercise, you will extract tweets on "Superbowl" that are original posts and not retweets, quotes, or replies. 

# Extract 100 original tweets on "Superbowl"
tweets_org <- search_tweets("Superbowl -filter:replies -filter:quote -filter:retweets", n = 100)

# Check for presence of replies
count(tweets_org$reply_to_screen_name)

# Check for presence of quotes
count(tweets_org$is_quote)

# Check for presence of retweets
count(tweets_org$is_retweet)

# You can use the language filter with a search query to filter tweets based on the language of the tweet.
# The filter extracts tweets that have been classified by Twitter as being of a particular language.

# Can you extract tweets posted in French on the topic "Apple iphone"?
# Extract tweets on "Apple iphone" in French
tweets_french <- search_tweets("Apple iphone", lang = "fr")

# Display the tweets
head(tweets_french$text)

# Display the tweet metadata showing the language
head(tweets_french$lang)

# Popular tweets are tweets that are retweeted and favorited several times.
# They are useful in identifying current trends. A brand can promote its merchandise and build brand loyalty by identifying popular tweets and retweeting them.

# In this exercise, you will extract tweets on "Chelsea" that have been retweeted a minimum of 100 times and also favorited at least by 100 users.

# Extract tweets with a minimum of 100 retweets and 100 favorites
tweets_pop <- search_tweets("Chelsea min_retweets:100 AND min_faves:100")

# Create a data frame to check retweet and favorite counts
counts <- tweets_pop[c("retweet_count", "favorite_count")]
head(counts)

# View the tweets
head(tweets_pop$text)

# User information contains data on the number of followers and friends of the twitter user.
# The user information may have multiple instances of the same user as the user might have tweeted multiple times on a given subject. 
# You need to take the mean values of the follower and friend counts in order to consider only one instance.

# In this exercise, you will extract the number of friends and followers of users who tweet on #skincare or #cosmetics. 
# Extract user information from the pre-loaded tweets data frame
user_cos <- users_data(tweet_cos)

# View first 6 rows of the user data
head(user_cos)

# Aggregate screen name, follower and friend counts
counts_df <- user_cos %>%
  group_by(screen_name) %>%
  summarize(follower = mean(followers_count),
            friend = mean(friends_count))

# View the output
head(counts_df)

# The ratio of the number of followers to the number of friends a user has is called the golden ratio.

# In this exercise, you will calculate the golden ratio for the aggregated data frame counts_df.
# Calculate and store the golden ratio
counts_df$ratio <- counts_df$follower/counts_df$friend

# Sort the data frame in decreasing order of follower count
counts_sort <- arrange(counts_df, desc(follower))

# View the first few rows
head(counts_sort)

# Select rows where the follower count is greater than 50000
counts_sort[counts_sort$follower>50000,]

# Select rows where the follower count is less than 1000
counts_sort[counts_sort$follower<1000,]

# A twitter list is a curated group of twitter accounts. 
# Twitter users subscribe to lists that interest them.

# In this exercise, you will extract lists of the twitter account of "NBA".
# For one of the lists, you will extract the subscribed users and the user information for some of these users.
# Extract all the lists "NBA" subscribes to and view the first 4 columns
lst_NBA <- lists_users("NBA")
lst_NBA[,1:4]

# Extract subscribers of the list "nbateams" and view the first 4 columns
list_NBA_sub <- lists_subscribers(slug  = "nbateams", owner_user = "NBA")
list_NBA_sub[,1:4]

# Create a list of 4 screen names from the subscribers list
users <- c("JWBaker_4","towstend", "iKaanic", "Dalton_Boyd")

# Extract user information for the list and view the first 4 columns
users_NBA_sub <- lookup_users(users)
users_NBA_sub[,1:4]

# Location-specific trends identify popular topics trending in a specific location. You can extract trends at the country level or city level.
# Can you extract topics trending in Canada and view the trends?
# Get topics trending in Canada
gt_country <- get_trends("Canada")

# View the first 6 columns
head(gt_country[,1:6])

# Note: tweet_volume is returned for trends only if this data is available.
# Get topics trending in London
gt_city <- get_trends("London")

# View the first 6 columns
head(gt_city[,1:6])

# Aggregate the trends and tweet volumes
trend_df <- gt_city %>%
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume))

# Sort data frame on descending order of tweet volumes and print header
trend_df_sort <- arrange(trend_df, desc(tweet_vol))
head(trend_df_sort,10)

# Visualizing the frequency of tweets over time helps understand the interest level over a product. 
# Walmart operates a chain of supermarkets and stores around the world. It would be interesting to check the interest level and recall for the brand Walmart by visualizing the frequency of tweets.
# Extract tweets on #walmart and exclude retweets
walmart_twts <- search_tweets("#walmart", n = 18000, include_rts = FALSE)

# View the output
head(walmart_twts)

# Create a time series plot
ts_plot(walmart_twts, by = "hours", color = "blue")

# A time series object contains the aggregated frequency of tweets over a specified time interval.
# Creating time series objects is the first step before visualizing tweet frequencies for comparison. 
# In this exercise, you will be creating time series objects for the competing sportswear brands Puma and Nike.
# Create a time series object for Puma at hourly intervals
puma_ts <- ts_data(puma_st, by ='hourly')

# Rename the two columns in the time series object
names(puma_ts) <- c("time", "puma_n")

# View the output
head(puma_ts)

# Create a time series object for Nike at hourly intervals
nike_ts <- ts_data(nike_st, by ='hourly')

# Rename the two columns in the time series object
names(nike_ts) <- c("time", "nike_n")

# View the output
head(nike_ts)

# The volume of tweets posted for a product is a strong indicator of its brand salience. Let's compare brand salience for two competing brands, Puma and Nike. 
# You will merge the time series objects and create time series plots to compare the frequency of tweets.
# Merge the time series objects with "time" as the common column
merged_df <- merge(puma_ts, nike_ts, by = "time", all = TRUE)
head(merged_df)

# Stack the tweet frequency columns
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")

# View the output
head(melt_df)

# Plot frequency of tweets on Puma and Nike
ggplot(data = melt_df, aes(x = time, y = value, col = variable)) +
  geom_line(lwd = 0.8)