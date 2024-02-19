
#Intro===================================================
# let's look at two accounts "⭐️HAPPY🤗⭐️"  and "
# Toyota USA" usage of emojies! 
# For my final project, I want to compare emotional
# component of an influencer with username @HappyJesse
# to an automaker, Toyota US, @Toyota. The reason behind
# this comparison is two folds; First, I want to see how
# Happy uses happy emojies and Second, I want to compare
# her emoji usage with that of an organization account.
# 
# I chose "⭐️HAPPY🤗⭐️"  beacuse of the  way they 
# epresent their name (using emoji). I also chose
# Toyota since it is my favourite brand, and it is
# not the usual choice for this class!

#Library and Sourcing calls===============================
source("01-clean-tweet.R")
library(tm) #for text cleaning
library(stringi) #for emojies and text manipulation
library(svglite) #for gtExtras links
library(gt) # for table
library(gtExtras) #for extra table features 



#Data Wrangling============================================

#getting the start and end dates for the tables footnotes

tweet |> 
  mutate(created_at = 
           format(floor_date(created_at, unit = "day"), "%y-%b-%e")) |> 
  arrange(created_at) |> 
  select(created_at) |>
  head(1) -> start_date

tweet |> 
  mutate(created_at = 
  format(floor_date(created_at, unit = "day"), "%y-%b-%e")) |> 
  arrange(created_at) |> 
  select(created_at) |>
  tail(1) -> end_date

# now data wrangling to extract and manipulate.
# I used list_member as the source to link tweets
# with accounts. Then I arranged based on date
# of creation and made all the text turned into
# lower case.

tweet |> 
  left_join(list_member |> select(username, name, user_id), join_by(user_id)) |> 
  filter(username == "HappyJesse" | username == "Toyota") |> 
  mutate(text = str_to_lower(text)) |> 
  arrange(created_at) |> 
  select(username, text) -> tweet
  


# text cleaning Function====================================
# cleaning text: lowercse, rmove puctuations, numbers stopwords
# and excessive space. Making a function for it
text_cleaner <- function(txt){
  Corpus(VectorSource(txt)) |>
    tm_map(content_transformer(tolower)) |>
    tm_map(removePunctuation) |>
    tm_map(removeNumbers) |>
    tm_map(removeWords, stopwords("english")) |>
    tm_map(stripWhitespace)
}

# Finding the top emojies====================================
#Now using the function for HappyJesse and Toyota tweets
#I have to put them in separate dataframes as text cleaner
#is working with corpous
tweet |> 
  filter(username == "HappyJesse") |> 
  select(text) |> 
  text_cleaner() -> happy_cleaned


tweet |> 
  filter(username == "Toyota") |> 
  select(text) |> 
  text_cleaner() -> toyota_cleaned

#loading the emojis dictionary from stringi
emoji_dictionary <- "\\p{So}"

# extracting all the emojis using the cleaned text and emoji dictionary 
happys_emojis <- stri_extract_all_regex(happy_cleaned, emoji_dictionary)
toyota_emojis <- stri_extract_all_regex(toyota_cleaned, emoji_dictionary)

#putting the extracted data from a
#list into a vector and then a dataframe
#for further analysis
happy_freq <- happys_emojis |>
  unlist() |>
  table()

toyota_freq <- toyota_emojis |>
  unlist() |>
  table() 


happy_df <- data.frame(Emoji = names(happy_freq), Frequency = happy_freq)
toyota_df <- data.frame(Emoji = names(toyota_freq), Frequency = toyota_freq)


#ranking emojis based on their frequency
#and then finding the top 7 in the ranking
#for each of the two users
happy_df |> 
  mutate(Frequency = Frequency.Freq) |> 
  arrange(desc(Frequency)) |> 
  mutate(Rank = as.numeric(row.names(happy_df)),
         username = "HappyJesse"
         ) |> 
  select(Rank, Emoji, Frequency, username) |> 
  top_n(5, Frequency) |> 
  head(5) -> happy_df

toyota_df |> 
  mutate(Frequency = Frequency.Freq) |> 
  arrange(desc(Frequency)) |> 
  mutate(Rank = as.numeric(row.names(toyota_df)),
         username = "Toyota"
         ) |> 
  select(Rank, Emoji, Frequency, username) |> 
  top_n(5, Frequency) |> 
  head(5) -> toyota_df


# Generating the gt() table===============================
c_df <- toyota_df |> select(Rank, Emoji, Frequency) |> 
  cbind(happy_df |> select(Rank, Emoji, Frequency))

#to avoid duplicate names when using gt()
c_df |>   
colnames() <- c("t_Rank", "t_Emoji", "t_Frequency", 
                  "h_Rank", "h_Emoji", "h_Frequency")

#the table
c_df |>
  gt() |>
  tab_header(
    title = md("**Comparin Happy Emotions with Toyota's**"),
    subtitle = md("Most used emojies used by 
    *@HappyJesse* compared to *@ToyotaUS*")
  ) |> 
  gt_plt_bar(column = t_Frequency,
             color = "steelblue",
             scale_type = "number") |> 
  gt_plt_bar(column = h_Frequency,
             color = "steelblue",
             scale_type = "number") |> 
  cols_label(
    t_Rank = "Rank",
    t_Emoji = "Emoji",
    t_Frequency = "Frequency",
    h_Rank = "Rank",
    h_Emoji = "Emoji",
    h_Frequency = "Frequency"
  ) |> 
  tab_spanner(
    columns = c(t_Rank, t_Emoji, t_Frequency),
    label = md("**Toyota**")
  ) |> 
  tab_spanner(
    columns = c(h_Rank, h_Emoji, h_Frequency),
    label = md("**Happy**")
  ) |> 
  tab_caption(
    caption = md('Account "⭐️HAPPY🤗⭐️" uses *happy*
    emoji as the dominant emotional component in their tweets
    while "Toyota" does not have a dominant emotional component
    in their tweets. Happy usage of one emoji is higher than Toyotas
    top emojies combined')
  ) |> 
  tab_options(
    table.width = pct(50),
    column_labels.padding = 0
  ) |> 
  tab_source_note(
    source_note = md("**source:** data sets in *{tslatwtr}*
                     package created by Prof. Jeffrey Boichuk <br> for the
                     *Digital Marketing Analytics* course."
    ) 
  ) |> 
  tab_footnote(
    footnote = paste("between", start_date,
                     "to", end_date, "(yy - m - d).", sep = " "),
    locations = cells_title(group = "subtitle")
  )#|> #SAVING AS HTML BECAUSE PNG DOES NOT RENDER THE TABLE AS SHOWN IN THE RUN
  #gtsave("individual_project_hossein_reihani.html")





