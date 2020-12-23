
require(e1071)
require(caret)
require(tm)
require(readxl)
require(dplyr)


data2 <- read.csv("/Users/joehome/Desktop/R Code/Natural Language Processing:Bag of Words/Data/work-stories-corpus-clean.csv")




#set the seed
set.seed(12)
#partition data into training and testing
#sample size is 75% of the length of data
#floor takes an argument and returns number
smp_size2 <- floor(0.75 * nrow(data2)) 
#creates an integer length of 75% of data
#sample takes a sample, seq len creates a sequence of numbers 
#that ends at our smple size
train_ind2 <- sample(seq_len(nrow(data2)),
                    size = smp_size2)
#create our train set
train2 <- data2[train_ind2, ]

#create our test set
test2 <- data2[-train_ind2, ]

#create a corpus
#this creates a collection of objects
#our content is the sentences

corpus2 <- Corpus(VectorSource(c(train2$Overall_Why, test2$Overall_Why)))

#examine part of our corpus, read the text
corpus2[1]$content

#pre processing
#change to lower case
#remove punctuation
#remove stop words from english
#remove the xtra space
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))
corpus2 <- tm_map(corpus2, removeWords, "also")
corpus2[1]$content
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus2[1]$content
corpus2 <- tm_map(corpus2, stemDocument, language = "english")

#convert to a document term matrix
#this matrix shows us all words in our corpus and how much they show up
#each row is a document
dtm2 <- DocumentTermMatrix(corpus2)
dtm2


#remove words that are less frequent to focus on important words
#in this case, words must show up in at least 2% of the documents
sparse2 <- removeSparseTerms(dtm2, .98)
sparse2



#now convert matrix into a data frame for training
important_words_df2 <- as.data.frame(as.matrix(sparse2))

#make names out of
colnames(important_words_df2) <- make.names(colnames(important_words_df2))


#split into train and test set
important_words_train_df2 <- head(important_words_df2, nrow(train2))

important_words_test_df2 <- tail(important_words_df2, nrow(test2))


# Add to original dataframes
train2 <- cbind(train2, important_words_train_df2)
test2 <- cbind(test2, important_words_test_df2)


#analysis time

#let's use a naive bayes classification algorithm that has been shown historically to perform well in similar situations
#examines bayesian probability that A has occured given B

#most basic picture, posterior = prior x likelihood/evidence

#Naive bayes assumes that features are equally important and independent, how naive of it!!


train2 <- train2 %>%
  select(Meaning_SR_Very, abl:studi)


test2 <- test2 %>%
  select(Meaning_SR_Very, abl:studi)

features <- setdiff(names(train2), "Meaning_SR_Very")

#data frame with features
x <- train2[, features]

train2$Meaning_SR_Very <- as.factor(train2$Meaning_SR_Very)
test2$Meaning_SR_Very <- as.factor(test2$Meaning_SR_Very)



y<- train2$Meaning_SR_Very


#set train control, repeated k folds cross validation

train_control <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3
)



#model training
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

#examine confusion matrix, average accuracy of around 83
confusionMatrix(nb.m1)

#below is from https://uc-r.github.io/naive_bayes

#examine tuning grid for various hyperparameters
#use kernel is a non-parametric way to estimate probability densities, from my understanding, we will examine if this improves the model
#adjust allows the density estimate to be more flexible
#fl examines laplace smoother


#from my reading, when an individual class label is not there,
#a probability estimate will be zero
#Laplace smoothing adds counts to our data to prevent occurrences of zero, thus preventing the zero issue

search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)


#try an optimal second model using our search grid from above
#this model also contains several pre processing approaches



nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
)


#now we can examine our top 5 models
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


#predict test set, killer performance!
pred <- predict(nb.m2, newdata = test2)
confusionMatrix(pred, test2$Meaning_SR_Very)





