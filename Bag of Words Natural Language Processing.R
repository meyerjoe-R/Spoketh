install.packages("tm")
install.packages("caTools")
install.packages("qdapRegex")
install.packages("textclean")
install.packages("caret")
install.packages("glmnet")
install.packages("SnowballC")
install.packages("Latent Semantic Analysis")
install.packages("text2vec")
install.packages("quanteda")
install.packages("e1071")
require(e1071)
require(quanteda)
require(text2vec)
require(SnowballC)
require(caret)
require(glmnet)
require(caTools)
require(tm)
require(readxl)
require(dplyr)
require(plyr)
require(qdapRegex)
require(textclean)

#what is NLP? Natural language processing (NLP) is 
       # defined as the use of computers to process natural language (Matthews, 2016)
#what is sentiment analysis?
#what is machine learning? ML



# Data requirements for this type of analysis
# Practical utility of this type of analysis








data<- read.csv("/Users/joehome/Desktop/R Code/Natural Language Processing:Bag of Words/Data/NLPTweets.csv")

View(data)


table(data$airline_sentiment) #view breakdown

table <- table(data$airline_sentiment, data$dept)

plot(table)


#remove emojis, replace them with equivalent text


data$text <- replace_emoji(data$text)


#create a factor
data$airline_sentiment <- as.factor(data$airline_sentiment)


#filter out neutral sentiment using dplyr

data <- data %>%
  filter(airline_sentiment != "neutral")




#recode sentiment, mapvalues specifies new values, then knock out old column
data$sentiment <- revalue(data$airline_sentiment, c("positive"="1", "negative"="0"))
data$sentiment <- mapvalues(data$airline_sentiment, from = c("positive", "negative"), to = c("1", "0"))
data <- data %>%
  select(-c(airline_sentiment))
View(data)


#set the seed
set.seed(1)


#partition data into training and testing
#sample size is 75% of the length of data
#floor takes an argument and returns number
smp_size <- floor(0.75 * nrow(data)) 


#creates an integer length of 75% of data
#sample takes a sample, seq len creates a sequence of numbers 
#that ends at our smple size

train_ind <- sample(seq_len(nrow(data)),
                    size = smp_size)


#create our train set
train <- data[train_ind, ]

#create our test set
test <- data[-train_ind, ]



#create a corpus, which is basically a bunch of text
#this creates a collection of objects
#our content is the sentences
#vector source interprets each part of the vector as a document

corpus <- Corpus(VectorSource(c(train$text, test$text)))

#examine part of our corpus, read the text
corpus[1]$content
              
#pre processing


#change to lower case
#remove punctuation
#remove stop words from english
#remove the xtra space

corpus <- tm_map(corpus, content_transformer(tolower))


corpus <- tm_map(corpus, removePunctuation)


corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[1]$content


corpus <- tm_map(corpus, stripWhitespace)
corpus[1]$content

corpus <- tm_map(corpus, stemDocument, language = "english")


#convert to a document term matrix
#this matrix shows us all words in our corpus and how much they show up
#each row is a document


dtm <- DocumentTermMatrix(corpus)
dtm
#we have 4333 features



#remove words that are less frequent to focus on important words
#in this case, words must show up in at least 1% of the documents
sparse <- removeSparseTerms(dtm, 0.99)
sparse

#we now have 179 features

#now convert matrix into a data frame for training

important_words_df <- as.data.frame(as.matrix(sparse))


#view our bag of words
View(important_words_df)

#make names out of 
colnames(important_words_df) <- make.names(colnames(important_words_df))

View(important_words_df)



#split into train and test set
important_words_train_df <- head(important_words_df, nrow(train))



important_words_test_df <- tail(important_words_df, nrow(test))



# Add to original dataframes
train <- cbind(train, important_words_train_df)
test <- cbind(test, important_words_test_df)

# Get rid of the original Text field
train$Text <- NULL
test$Text <- NULL





#bag of words time, let's build a classifier


#documents are treated as frequencies of words


#so, sentiment classification will be based on what words are in the documents

#words are the features to help us train


#clean up unwanted data, retain features
train <- train %>%
  select(-c(tweet_id:user_timezone))



train$sentiment <- as.factor(train$sentiment)

test <- test %>%
  select(-c(tweet_id:user_timezone))

test$sentiment <- as.factor(test$sentiment)


#run the model

#binomial means logistic regression
#z coefficient shows us the importance of variables
#pay attention to the direction of variables
#p is significance

log_model <- glm(sentiment~., data=train, family=binomial)
summary(log_model)




#predict our test data
#probabilities are returned
#estimate is the coefficient
#fisher scoring iterations are how many times it took to fit it

log_pred <- predict(log_model, newdata=test, type="response")


#set thresehold based on what should be positive, ie probability of classification
#view table of predictions

table(test$sentiment, log_pred>.5)
#pretty good, pretty pretty good


#add diagonals to evaluate accuracy
(502 + 73) / nrow(test)



#lets try to automate feature selection, dont run!

#start with null model with no predictors
null_model <- glm(sentiment ~ 1, data = train, family = "binomial")



#model with all predictors
full_model <- glm(sentiment ~., data = train, family = "binomial")



#step wise model
step_model <- step(null_model, scope = list
                   (lower = null_model, upper = full_model),
                   direction = "forward")






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

corpus2 <- Corpus(VectorSource(c(train2$Work_Story, test2$Work_Story)))

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
#in this case, words must show up in at least 10% of the documents
sparse2 <- removeSparseTerms(dtm2, .90)
sparse2


#we now have 464 features
#now convert matrix into a data frame for training
important_words_df2 <- as.data.frame(as.matrix(sparse2))


#view our bag of words
View(important_words_df2)

#make names out of
colnames(important_words_df2) <- make.names(colnames(important_words_df2))

View(important_words_df2)


#split into train and test set
important_words_train_df2 <- head(important_words_df2, nrow(train2))



important_words_test_df2 <- tail(important_words_df2, nrow(test2))

View(important_words_test_df2)


# Add to original dataframes
train2 <- cbind(train2, important_words_train_df2)
test2 <- cbind(test2, important_words_test_df2)


#bag of words time


#let's attempt to predict nueroticism from text responses

trainpersonality <- train2 %>% 
  select(NEO_NERVOUS, can:come)



#now, we will use caret to set up some parameters
#I will use k-folds cross validation, models are developed on x chunks then
#are used to predict the nth model


tc <- trainControl(method = "cv", number = 8)

#predict neo, data, method is lm, training parameters come from above

lml_cv <- train(NEO_NERVOUS~., data = trainpersonality, method = 'lm',
                trControl = tc)

lml_cv


attach(train2)
View(train2)



#subset data
trainmeaning <- train2 %>%
  select(Meaning_SR_Very,access:recent)
View(trainmeaning)

#factor binary outcome
trainmeaning$Meaning_SR_Very <- as.factor(trainmeaning$Meaning_SR_Very)


#subset test
testmeaning <- test2 %>%
  select(Meaning_SR_Very, access:recent)


#factor test outcome
testmeaning$Meaning_SR_Very <- as.factor(testmeaning$Meaning_SR_Very)

attach(trainmeaning)

#run model
lml_cv2 <- train(Meaning_SR_Very~., data = trainmeaning, method = 'glm',
                family = "binomial",
                trControl = tc)

#view model
lml_cv2

#view importance
importance <- varImp(lml_cv2)

print(importance)



#let's try to retain important predictors
lml_cv3 <- train(Meaning_SR_Very ~ ship + old + includ + citi + appli +
                   sinc + order + direct + made + sit + sign + regular + drive
                 + site + opportun + knowledg + ask + year + vari + special, data = trainmeaning,
                 method = 'glm', family = "binomial", trControl =tc)

lml_cv3


#examine predictions
predict(lml_cv3, newdata = testmeaning, type = "prob")


#predict
log_pred2 <- predict(lml_cv3, newdata = testmeaning, type = "raw")

log_pred2

#examine accuracy
#kappa tells us agreement between machine classifier and true classification,
#while controlling for random agreement
#negative kappa is alarming
#specificity is proportion of negatives correctly identified
#sensitivity is the number of correctly classified positive over total positive cases
#balanced accuracy shows u that we may be less accurate because classes are imbalanced


confusionMatrix(data = log_pred2,
                reference = testmeaning$Meaning_SR_Very)







#don't run
#matrix of predictors, take out sentiment
x <- model.matrix(WAMI_Meaningful_Career ~., data = trainmeaning [,-1])

#if else is a test, assigns based on true or false
#y is now numeric
y <- ifelse(train$sentiment == "pos", 1,0)

#now we need to find lambda, which adjusts coefficient shrinkage
#the most ideal one is the one that minimizes cross validation
#prediction error rate
#let's use cv.glmnet to find our lambda
#lambda is the amount of shrinkage, or decrease in explanatory power out of sample

set.seed(111)

cv.lasso <- cv.glmnet(x,y,alpha = 1, family = "binomial", foldid = 4, nfolds = 4)

#use k folds cross validation to estimate lambda

#k folds basically creates multiple iterations of cross validation
#less biased that a typical train and test split
#if k = n, every piece of data can be train and test


model <- glmnet(x,y, alpha =1, family = "binomial",
                lambda = cv.lasso$lambda.min)

#alpha of 1 means ridge regression
glmnet(x,y, family = "binomial", alpha = 1)

#but first, knock out NA
has_NA = apply(is.na(train), 1, any) # 1 if any column in that row is NA
x = x[!has_NA,]
y = y[!hasNA,]


citation(package = "caret")





