install.packages("tidytext")
install.packages("radarchart")
install.packages("knitr")
install.packages("kableExtra")
install.packages("memery")
install.packages("wordcloud2")

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
require(ggraph)
require(wordcloud2)

data <- read.csv("/Users/joehome/Desktop/R Code/Natural Language Processing:Bag of Words/Data/work-stories-corpus-clean.csv")

#let's begin with some cleaning


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
data$Work_Story <- sapply(data$Work_Story, fix.contractions)

#Now create a function and use gsub once again to remove contractions

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)


#apply the function
data$Work_Story <- sapply(data$Work_Story, removeSpecialChars)

#now knock out capital letters using tolower
data$Work_Story <- sapply(data$Work_Story, tolower)


#check out the data, it should look different now
str(data[1, ]$Work_Story, nchar.max = 300)



#now we're going to begin preparing the corpus

data2 <- data %>%
  select(Work_Story, Meaning_SR_Very, SEX, INDUSTRY) %>% #select our response of interest
  unnest_tokens(word, Work_Story) %>% #separate out words from the stories
  anti_join(stop_words) %>% #remove stop words
  filter(nchar(word) > 3) #filter out words with less than three characters as they may be uninformative


#let's examine the most frequent words
data2 %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = "green") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Count") +
  ggtitle("Wordz") +
  coord_flip()


#examine a word cloud with a grain of salt
#count and sort words
counts <- data2 %>%
  count(word, sort=TRUE)

#it's an html object, so you can view word counts in it
wordcloud2(counts[1:300, ], size = .5)



#I'm curious what word usage looks like in nuerotic and outgoing individuals, and I'm curious how industries differ

data3 <- data %>%
  select(Work_Story, Meaning_SR_Very, SEX, INDUSTRY, NEO_OUTGOING, NEO_NERVOUS) %>% #select our response of interest
  mutate(Nuerotic = 
           ifelse(data$NEO_NERVOUS %in% 3:6, "Nervous", "Calm")) %>% #if above 3, then nervous, if not then calm
  mutate(Extraverted = 
           ifelse(data$NEO_OUTGOING %in% 3:6, "Extraverted", "Introverted")) %>%
  unnest_tokens(word, Work_Story) %>% #separte out words from the stories
  anti_join(stop_words) %>% #remove stop words
  filter(nchar(word) > 3) #filter out words with less than three characters as they may be uninformative


#group by nervousness
#count top 8 words
popular_words <- data3 %>% 
  group_by(Nuerotic) %>%
  count(word, Nuerotic, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Nuerotic,n) %>%
  mutate(row = row_number()) 


#We don't see too much differentiation here
popular_words %>%
  ggplot(aes(row, n, fill = Nuerotic)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Count") +
  ggtitle("Popular Words by Nueroticism") + 
  facet_wrap(~Nuerotic, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, 
    labels = popular_words$word) +
  coord_flip()


#What about Extraversion?
#group by Industry
#count top 8 words
popular_words2 <- data3 %>% 
  group_by(Extraverted) %>%
  count(word, Extraverted, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Extraverted,n) %>%
  mutate(row = row_number()) 

#hmm, no initial insights here, nevertheless an interesting comparison
popular_words2 %>%
  ggplot(aes(row, n, fill = Extraverted)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Count") +
  ggtitle("Popular Words by Extraverted") + 
  facet_wrap(~Extraverted, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, 
    labels = popular_words$word) +
  coord_flip()




#group by Meaningfulness
#count top 8 words
popular_words3 <- data3 %>% 
  group_by(Meaning_SR_Very) %>%
  count(word, Meaning_SR_Very, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Meaning_SR_Very,n) %>%
  mutate(row = row_number()) 


#We see some interesting trends just based on a basic look
#Those who perceive their work to be very meaningful tend to talk about organizationally-related things
#e.g., "clients, teams, organizations"
popular_words3 %>%
  ggplot(aes(row, n, fill = Meaning_SR_Very)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Count") +
  ggtitle("Popular Words") + 
  facet_wrap(~Meaning_SR_Very, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, 
    labels = popular_words$word) +
  coord_flip()


#now lets use TF-IDF, which means term frequency inverse document frequency

#lower weights are assigned for ocmmonly used words, and higher for the opposite







  