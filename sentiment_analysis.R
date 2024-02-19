source("01-clean-tweet.R")
library(sentimentr)
library(tidytext)
library(textdata)
library(tm)
library(gt)
library(ggthemes)


############
# better text cleaner that doesn't work!
###########

# str_replac
#cleanPosts <- function(text) {
#clean_texts <- text %>%
#  gsub("<.*>", "", .) %>% # remove emojis
#  gsub("&amp;", "", .) %>% # remove &
#  gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) #%>% # remove retweet entities
#  gsub("@\\w+", "", .) %>% # remove at people
#  hashgrep %>%
#  gsub("[[:punct:]]", "", .) %>% # remove punctuation
#  gsub("[[:digit:]]", "", .) %>% # remove digits
#  gsub("http\\w+", "", .) %>% # remove html links
#  iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
#  gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
#  gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
#  tolower
#return(clean_texts)
#}
############


#we used this simple text cleaner

text_cleaner <- function(x){x |> 
    removeNumbers() |> 
    tolower() |> 
    removePunctuation()
}



# From tesla related tweets with impression we clean the tweets
# texts first, then tokenized each tweet into its words and then
# exclude the stop words as they do not have any emotional or
# sentiment value, then using "nrc" lexicon, we analyze each
# word's sentiment and emotion and then regroup them based on 
# tweet_id and summarize emotions and sentiment for each tweet
# in tweet_sentiment dataset.

thetweet<- "tesla is making entertaining andthoughtful decisions that answer the essential needs for affordability and innovation in the ev market"


tweet |>  
  left_join(list_member |>  select(user_id, username), join_by(user_id)) |>
  mutate(created_at = format(created_at , "%y-%b-%d")) |> 
  filter(is_tesla_related & impression_count >0) |>
  mutate(text = text_cleaner(text)) |>
  select(created_at , tweet_id, text) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |> 
  inner_join(get_sentiments("nrc") , relationship = "many-to-many") |> 
  arrange(created_at) |> 
  select(tweet_id , sentiment) |> 
  group_by(tweet_id , sentiment) |> 
  count() |> 
  ungroup() |> 
  pivot_wider(names_from = sentiment ,
              values_from = n ,
              values_fill = 0) |> 
  relocate(positive , .before = anticipation) |>
  relocate(negative , .before = anticipation) -> tweet_sentiment



# Looking at the tweet_sentiment data frame:

tweet_sentiment |>
  head(5) |> 
  gt() |> 
  cols_label(
    tweet_id = md("**Tweet ID**")
  ) |> 
  tab_spanner(
    label = md("**Sentiment**"),
    columns = c(positive , negative)
  ) |> 
  tab_spanner(
    label = md("**Emotions**"),
    columns = c(anticipation , trust , joy , surprise ,
                anger , fear , sadness , disgust)
  ) |> 
  data_color(
    columns = c(positive , negative),
    colors = "aliceblue"
  ) |> 
  tab_header(
    title = md('**Sentiment Dataframe**'),
    subtitle = md('for each tweet ID, numbers represent how many
                  words represent a specific sentiment or emotion')
  ) |> 
  cols_align(
    align = "center"
  ) |> 
  tab_source_note(
    source_note = md("**Source:** tslatwtr package.")
  )



# now we can find the impact of using positive and negative words on
# engagement

tweet |> 
  right_join(tweet_sentiment, join_by(tweet_id)) |> 
  mutate(negative = -1 * negative) |>
  mutate(positivity = positive + negative) |> 
  group_by(positivity) |> 
  summarise(
    count              = n(),
    LikeCount          = sum(like_count, na.rm=TRUE),
    QuoteCount         = sum(quote_count, na.rm=TRUE),
    ReplyCount         = sum(reply_count, na.rm=TRUE),
    RetweetCount       = sum(retweet_count, na.rm = TRUE),
    engagement_all     = sum(LikeCount,QuoteCount,ReplyCount,RetweetCount),
    engagement         = engagement_all/count) |>
  select(positivity , engagement) |> 
  ggplot(aes(positivity, engagement , fill = positivity)) +
  geom_col() +
  scale_x_continuous(breaks = seq(-8, 12, by = 1) , limits = c(-8, 12)) + 
  geom_hline(yintercept = 560, color = "red", size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = md("Positivity and Engagement"),
    subtitle = "Relation between number of positive/ negative word usage with engagement",
    x = "number of positive or negative words",
    y = "Relative Engagement",
    caption = "Source: tslatwtr package."
  )



# now let's look at emotions and their relationship with
# engagement

tweet |>  
  left_join(list_member |>  select(user_id, username), join_by(user_id)) |>
  filter(is_tesla_related & impression_count >0) |>
  mutate(text = text_cleaner(text)) |>
  select(tweet_id, text) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |> 
  inner_join(get_sentiments("nrc") , relationship = "many-to-many") |> 
  select(tweet_id , sentiment) |>
  filter(sentiment != "negative" & sentiment != "positive") |> 
  left_join(tweet |> select(
    like_count, quote_count, reply_count, retweet_count, tweet_id),
    join_by(tweet_id)) |> 
  group_by(sentiment) |> 
  summarise(
    count              = n(),
    LikeCount          = sum(like_count, na.rm=TRUE),
    QuoteCount         = sum(quote_count, na.rm=TRUE),
    ReplyCount         = sum(reply_count, na.rm=TRUE),
    RetweetCount       = sum(retweet_count, na.rm = TRUE),
    engagement_all     = sum(LikeCount,QuoteCount,ReplyCount,RetweetCount)) |>
  select(sentiment, engagement_all) |>
  ggplot(aes(x = reorder(sentiment, -engagement_all), 
             y = engagement_all/1000)) + geom_col() +
  labs(
    title = md("Emotions and Engagement"),
    subtitle = "Relation between each ofthe 8 emotions usage with engagement",
    x = "Diferent Emotions",
    y = "Relative Engagement (scaled)",
    caption = "Source: tslatwtr package.") + theme_economist()






# Now let's find out what are the most used positive 
# words. I will take the top 20 most used positive words
# and hopefully there are some related to affordability


tweet |>  
  left_join(list_member |>  select(user_id, username), join_by(user_id)) |>
  filter(is_tesla_related & impression_count >0) |>
  mutate(text = text_cleaner(text)) |>
  select(tweet_id, text , like_count, retweet_count, quote_count, reply_count) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |> 
  inner_join(get_sentiments("nrc") , relationship = "many-to-many") |> 
  filter(sentiment == "positive") |> 
  select(word, like_count, retweet_count, quote_count, reply_count) |> 
  group_by(word) |> 
  summarise(count = n(),
            LikeCount          = sum(like_count, na.rm=TRUE),
            QuoteCount         = sum(quote_count, na.rm=TRUE),
            ReplyCount         = sum(reply_count, na.rm=TRUE),
            RetweetCount       = sum(retweet_count, na.rm = TRUE),
            engagement         = 
              round(mean(LikeCount,QuoteCount,ReplyCount,RetweetCount)/count, digits = 0)
            ) |> 
  select(word, engagement) |> 
  arrange(desc(engagement)) |> 
  head(10) |> 
  gt() |> 
  tab_header(
    title = md("**Top Positive Words**"),
    subtitle = md("Most impactful *positive* words <br> for tels-related tweets")
  ) |> 
  tab_source_note(
    source_note = md("**Source:** tslatwtr package <br> 
                     engagement = a function of *likes*, *quotes*, *retweets* <br>
                     and *replies* and word's frequency ")
  ) |> 
  tab_style(
    style = list(cell_fill(color = "aliceblue")),
    locations = cells_body(rows = c(2:7, 9:10))
  ) |> 
  cols_label(
    word = md("**Word**"),
    engagement = md("**Engagement**")
  )#|> gtsave("pos.png")




# same for trust

tweet |>  
  left_join(list_member |>  select(user_id, username), join_by(user_id)) |>
  filter(is_tesla_related & impression_count >0) |>
  mutate(text = text_cleaner(text)) |>
  select(tweet_id, text , like_count, retweet_count, quote_count, reply_count) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |> 
  inner_join(get_sentiments("nrc") , relationship = "many-to-many") |> 
  filter(sentiment == "trust") |> 
  select(word, like_count, retweet_count, quote_count, reply_count) |> 
  group_by(word) |> 
  summarise(count = n(),
            LikeCount          = sum(like_count, na.rm=TRUE),
            QuoteCount         = sum(quote_count, na.rm=TRUE),
            ReplyCount         = sum(reply_count, na.rm=TRUE),
            RetweetCount       = sum(retweet_count, na.rm = TRUE),
            engagement         = 
              round(mean(LikeCount,QuoteCount,ReplyCount,RetweetCount)/count, digits = 0)
  ) |> 
  select(word, engagement) |> 
  arrange(desc(engagement)) |> 
  head(10) |> 
  gt() |> 
  tab_header(
    title = md("**Top Trust Words**"),
    subtitle = md("Most impactful *trust* words <br> for tels-related tweets")
  ) |> 
  tab_source_note(
    source_note = md("**Source:** tslatwtr package <br> 
                     engagement = a function of *likes*, *quotes*, *retweets* <br>
                     and *replies* and word's frequency ")
  ) |> 
  tab_style(
    style = list(cell_fill(color = "aliceblue")),
    locations = cells_body(rows = c(1:3, 5,8:10))
  ) |> 
  cols_label(
    word = md("**Word**"),
    engagement = md("**Engagement**")
  ) #|> gtsave("trs.png")


# and same for anticipation

tweet |>  
  left_join(list_member |>  select(user_id, username), join_by(user_id)) |>
  filter(is_tesla_related & impression_count >0) |>
  mutate(text = text_cleaner(text)) |>
  select(tweet_id, text , like_count, retweet_count, quote_count, reply_count) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words) |> 
  inner_join(get_sentiments("nrc") , relationship = "many-to-many") |> 
  filter(sentiment == "anticipation") |> 
  select(word, like_count, retweet_count, quote_count, reply_count) |> 
  group_by(word) |> 
  summarise(count = n(),
            LikeCount          = sum(like_count, na.rm=TRUE),
            QuoteCount         = sum(quote_count, na.rm=TRUE),
            ReplyCount         = sum(reply_count, na.rm=TRUE),
            RetweetCount       = sum(retweet_count, na.rm = TRUE),
            engagement         = 
              round(mean(LikeCount,QuoteCount,ReplyCount,RetweetCount)/count, digits = 0)
  ) |> 
  select(word, engagement) |> 
  arrange(desc(engagement)) |> 
  head(10) |> 
  gt() |> 
  tab_header(
    title = md("**Top Anticipation Words**"),
    subtitle = md("Most impactful *anticipation* words <br> for tels-related tweets")
  ) |> 
  tab_source_note(
    source_note = md("**Source:** tslatwtr package <br> 
                     engagement = a function of *likes*, *quotes*, *retweets* <br>
                     and *replies* and word's frequency")
  ) |> 
  tab_style(
    style = list(cell_fill(color = "aliceblue")),
    locations = cells_body(rows = c(1:3, 7:8))
  ) |> 
  cols_label(
    word = md("**Word**"),
    engagement = md("**Engagement**")
  ) |> gtsave("ant.png")
















