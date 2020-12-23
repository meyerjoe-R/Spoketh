install.packages("rtweet")
install.packages("tidyverse")
require(rtweet)
require(ggplot2)
require(tidytext)
require(tidyr)
require(tidyverse)
require(dplyr)
require(ggpubr)



#create keys and authenticate, this will produce a pop up
api_key <- "key here"

api_secret_key <- "secret key here"

token <- create_token(
  app = "bottomofthebarrel",
  consumer_key = api_key,
  consumer_secret = api_secret_key)



#look up 1k popular tweets that contain these key words

tweets <- search_tweets2(c("#election", "fraud", "trump"), n = 5000, include_rts = FALSE, type = "popular")


#examine tweets over time
tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #election Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


#tidytext format for tweets
tweets <- tweets %>%
  select(status_id,text) %>%
  unnest_tokens(word,text) #split into word per row

#create stop words we may see in twitter

my_stop_words <- tibble( #construct a dataframe
  word = c(
    "https",
    "t.co",
    "rt",
    "amp",
    "gt"
  ),
  lexicon = "twitter"
)



#bind twitter stop words to regular stop words

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)

#remove numbers using filter

no_numbers <- tweets %>%
  filter(is.na(as.numeric(word)))

#get rid of stop words
no_stop_words <- no_numbers %>%
  anti_join(all_stop_words, by = "word")



#sentiment analysis on tweets using nrc lexicon


nrc <- get_sentiments("nrc")

#attach sentiment to our words
nrc_words <- no_stop_words %>%
  inner_join(nrc, by="word")


pie_words<- nrc_words %>%
  group_by(sentiment) %>% # group by sentiment type
  tally %>% # counts number of rows
  arrange(desc(n)) # arrange sentiments in descending order based on frequency

#we see a fairly even split of emotions
#negative may take the pie
ggpie(pie_words, "n", label = "sentiment", fill = "sentiment", 
      color = "white",
      pallette = "Spectral")






