install.packages("partykit") #downloaded packages
install.packages("rpart") #download packages
install.packages("caret", dependencies = c("Depends", "Suggests")) #download necessary packages, package helps build and test models
install.packages("rpart.plot") #download packages
install.packages("rattle") #download packages
install.packages("ISLR") #download packages


require(dplyr) #prep packages
require(ggplot2) #prep packages
library(readxl) #prep packages
require(rpart) #prep packages
require(rpart.plot) #prep packages
require(rattle) #prep packages
require(ISLR) #prep packages

chaiddata <-read_xlsx("/Users/joehome/Library/Containers/com.microsoft.Excel/Data/Desktop/CHAID.xlsx", range = "A1:G795") #open the file path

chaiddata <-read_xlsx(file.choose()) #another option

str(chaiddata) #examine variables and variable types


chaiddata <- chaiddata[colSums(is.na(chaiddata)) !=nrow(chaiddata)] #knock out NAs


head(chaiddata) #check data
tail(chaiddata) #check data

complete.cases(chaiddata) #check completed cases

chaiddata <- chaiddata[complete.cases(chaiddata), ] #knock out missing cases listwise #don't run

Hours <- factor(chaiddata$Hours) #alright, factor now yall
Accident <- factor(chaiddata$Accident) #alright, factor now yall
Pos <- factor(chaiddata$Pos) #alright, factor now yall
Season <- factor(chaiddata$Season) #alright, factor now yall
Education <- factor(chaiddata$Educational_level) #alright, factor now yall
Occupation <- factor(chaiddata$Occupation) #alright, factor now yall


levels(Occupation) #check levels

is.factor(Hours) #check if factored
is.factor(Accident) #check again


str(chaiddata) #check it

ggplot(chaiddata) + geom_bar(aes(x = Accident)) #visualize some of the data
ggplot(chaiddata) + geom_bar(aes(x = Education)) #visualize some of the data



analysis <- rpart(chaiddata$Accident ~ Education + Hours + Pos + Season + Occupation, data = chaiddata, method = "class")


rpart.plot(analysis, type = 2, extra = "auto", digits = 2, snip = TRUE, box.palette = "GnYlRd")

rpart.plot(analysis) #visualize

prp(analysis) #visualize

fancyRpartPlot(analysis) #visualize         


set.seed(123) #set the seed

shuffle_index <-sample(1:nrow(chaiddata)) #create an index for shuffling
chaiddata <- chaiddata[shuffle_index, ] #shuffle it

head(chaiddata) #check out the head

dt= sort(sample(nrow(chaiddata), nrow(chaiddata)*.7)) #set the stage for splitting data set

train_1 <- chaiddata[dt,] #training data
test_1 <- chaiddata[-dt,] #testing data

train_1 <- chaiddata[1:650, ] #create train and test (another way)
test_1 <- chaiddata[651:794, ] #another way to create training sets


analysis2 <- rpart(Accident ~., train_1, method = "class") #train the model


pred <- predict(analysis2, test_1, type = 'class') #test model performance


table_mat <- table(test_1$Accident, pred) #create a table to see predictions and accuracy
table_mat #view the table

accuracy_Test <- sum(diag(table_mat))/sum(table_mat) #divide the sum of the diaganol in my able by
# the sum of the matrix #true positives and negatives by total choices
print(paste('Accuracy for test', accuracy_Test)) #view the accuracy



