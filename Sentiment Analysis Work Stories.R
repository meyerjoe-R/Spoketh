
require(readxl)
require(dplyr)
require(ggplot2)
require(tidyr)
require(tidytext)
require(radarchart)
require(knitr)
require(kableExtra)
require(memery)
require(igraph)
require(wordcloud2)

#This data is from Mike Morrison's thesis, found online

data <- read.csv("/Users/joehome/Desktop/R Code/Natural Language Processing:Bag of Words/Data/work-stories-corpus-clean.csv")


#let's begin with some cleaning again


#this function will expand contractions typically seen
#gsub searchers for a match to the specified pattern then replaces it

fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("'s", "", doc)
  return(doc)
}


#Use the function to clean the data
#sapply applys the function across the vector
data$Work_Story <- sapply(data$Work_Story, fix.contractions)

#Now create a function and use gsub once again to remove contractions
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

#apply the function
data$Work_Story <- sapply(data$Work_Story, removeSpecialChars)
#now knock out capital letters using tolower
data$Work_Story <- sapply(data$Work_Story, tolower)
#check out the data, it should look different now
str(data[1, ]$Work_Story, nchar.max = 300)


data2 <- data %>%
  select(Work_Story, Meaning_SR_Very, SEX, INDUSTRY) %>% #select our response of interest
  unnest_tokens(word, Work_Story) %>% #separate out words from the stories
  anti_join(stop_words) %>% #remove stop words
  filter(nchar(word) > 3) #filter out words with less than three characters as they may be uninformative


#let's try some sentiment analysis, sentiments provides sentiment lexicons
#I'm choosing the nrc lexicon from tidytext
#it provides specific emotions


#This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.


data_nrc <- data2 %>%
  inner_join(get_sentiments("nrc"))



words <- data_nrc %>% 
  group_by(Meaning_SR_Very) %>% #group by meaning
  count(word, Meaning_SR_Very, sort = TRUE) %>% #sort
  slice(seq_len(8)) %>% #top words
  ungroup() %>%
  arrange(Meaning_SR_Very,n) %>%
  mutate(row = row_number()) 


#We don't see too much differentiation here
words %>%
  ggplot(aes(row, n, fill = Meaning_SR_Very)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Count") +
  ggtitle("Popular Words by Meaning") + 
  facet_wrap(~Meaning_SR_Very, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = words$row, 
    labels = words$word) +
  coord_flip()


#Let's expand our unit of analysis and examine sentiment at the response level, this time I'll examine a different response

data3 <- data %>%
  select(Overall_Why,)









