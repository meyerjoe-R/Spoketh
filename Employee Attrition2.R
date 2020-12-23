install.packages("expss")
install.packages("plotly")
install.packages("imbalance")
install.packages("ROSE")
install.packages("mltools")
install.packages("randomForest")
install.packages("h2o")
install.packages("ranger")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("GGally")


require(readxl)
require(tidyr)
require(mltools)
require(caret)
require(ROSE)
require(imbalance)
require(plotly)
require(expss)
require(tidyr)
require(data.table)
require(dplyr)
require(ggplot2)
require(randomForest)
require(h2o)
require(ranger)
require(gridExtra)
require(ggpubr)
require(corrplot)
require(GGally)





#this file is from a Kaggle competition, created by IBM
#the goal is to model employee attrition

data <- read.csv("/Users/joehome/Desktop/R Code/IBM Attrition/WA_Fn-UseC_-HR-Employee-Attrition.csv")




glimpse(data)
summary(data)

#prepare data --> factor
data$JobLevel <- as.factor(data$JobLevel)
data$OverTime <- as.factor(data$OverTime)
data$Gender <- as.factor(data$Gender)
data$Attrition <- as.factor(data$Attrition)
data$BusinessTravel <- as.factor(data$BusinessTravel)
data$JobRole <- as.factor(data$JobRole)
data$Department <- as.factor(data$Department)



#remove unwanted variables

data <- data %>% select(-EmployeeCount, -StandardHours, -Over18, -EducationField)



#explore the data some, group by attrition variable and create a total
dist_attr <- data %>%
  group_by(Attrition) %>%
  summarise(Total = n()) %>%
  print()

dist_attr %>% 
  ggplot(aes(x=Attrition, y=Total)) +
  geom_col() +
  ggtitle("Total Numbers of Attrition")

#Age distribution
plot1 <- data %>% 
  ggplot(aes(x=Age)) + 
  geom_density(fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "Age Distribution")

#Tenure
plot2 <- data %>% 
  ggplot(aes(x=YearsAtCompany)) + 
  geom_density(fill = "green", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(YearsAtCompany))) +
  labs(title = "Tenure Distribution")

#Tenure in role
plot3 <- data %>% 
  ggplot(aes(x=YearsInCurrentRole)) + 
  geom_density(fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(YearsInCurrentRole))) +
  labs(title = "Role Tenure")

#Hourly rates
plot4 <- data %>%
  ggplot(aes(x=HourlyRate)) + 
  geom_density(fill = "purple", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(HourlyRate))) +
  labs(title = "Rate Distribution")


#arrange them in a plot
ggarrange(plot1, plot2,
          nrow= 2)


ggarrange(plot2, plot3,
          nrow= 2)

#Research Sat
plot5 <- data %>% 
  filter(JobRole=="Research Scientist") %>%
  ggplot(aes(x=EnvironmentSatisfaction)) + 
  geom_density(fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(EnvironmentSatisfaction))) +
  labs(title = "Scientist Sat")

#Research performance
plot6 <- data %>% 
  filter(JobRole=="Research Scientist") %>%
  ggplot(aes(x=PerformanceRating)) + 
  geom_density(fill = "green", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(PerformanceRating))) +
  labs(title = "Scientist Perf")


ggarrange(plot5, plot6,
          nrow= 2)


#attrition by gender
gender_attr <- data %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())


gender_attr %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge")

#Examine correlation matrix
ggcorr(data, nbreaks = 5)



#alright, let's get ready for train/test

#80% train
set.seed(1)
index <- sample(nrow(data), nrow(data)*0.8)
data_train <- data[index, ]
data_test <- data[-index,]



#examine class balancing, it's biased towards no
table(data_train$Attrition)



#random forest (RF) is an extension of decision trees
#RF uses bagging, which build various decision trees
#Bagging aggregates predictions over the trees, hence it is called an ensemble model
#trees are grown randomly through bootstrapping


#let's try a default RF model with no tuned hyperparameters
#hyperparameters are basically levers that can be automatically/manually found and manipulated




RF1 <- ranger(
  Attrition ~ ., 
  data = data_train,
  seed = 123
)


RF1

#examine rmse
(default_rmse <- sqrt(RF1$prediction.error))



#now let's talk about, then examine hyperparameters and other "levers"

#the following is from https://bradleyboehmke.github.io/HOML/random-forest.html, which is a great ML source


#Consideration 1 --> The number of trees in the forest
#rule of thumb it to start with 10 x number of features

#2 ---> The number of features to consider at any given split
#this is called mtry, which controls how splitting occurs
#defaults will depend on whether you are doing regression or classification
#also, it depends on how noisy your data is (irrelevant variables)
#when there is a lot of noise a higher value is better, lower for the opposite


#The complexity of each tree
#this refers to various things about how simple or complex the tree is
#such as node size, max depth, max # of terminal nodes


#The sampling scheme - default scheme is 100% of variables are sampled with replacement
#i.e., each boostrap is the size of training n
#decreasing the sample size can sometimes increase prediction because trees will be less correlated
#when you have many categorical features with varying levels, sampling without replacement may be more preferable


#The splitting rule to use during tree construction - the default option aims to minimize error 


#So, let's search through combinations of hyperparameters


#First, set a control for training, this will be a repeated 10 fold cross validation
#this basically separates our data into 10 mutually exclusive subsets, aggregates performance across them, then repeats


control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")

set.seed(1)
#set the stage, we're interested in tuning our model based on the metric of accuracy
metric <- "Accuracy"
mtry <- sqrt(ncol(data_train))
tunegrid <- expand.grid(.mtry=mtry)


#perform a grid search for mtry values using Caret, don't run because I use a different method in the end
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Attrition~., data=data_train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


#but, there are various other hyperparameters, so I'm just going to expand to examine combos of other hypers

n_features <- round(ncol(data_train)/3)

hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)



#we'll apply our hyper list here, we have like 120 combinations of parameters


for(i in seq_len(nrow(hyper_grid))) {
  fit2 <- ranger(
    formula         = Attrition ~ ., 
    data            = balanced, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
  )
  
  
  # grab error of each one
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}


#now arrange by rmse to examine best performing models


hyper_grid %>%
  arrange(rmse) %>%
  head(10)

#Models seem to be consistent, hmm



#We see that we have a low balanced accuracy, one may consider oversampling the data set at this point
#This basically means balancing attirtion and no attrition for training


data_train$MaritalStatus <- as.factor(data_train$MaritalStatus)
data_test$MaritalStatus <- as.factor(data_test$MaritalStatus)

#Use ROSE package, p is probability of resampling from the rare class
balanced <- ovun.sample(Attrition~., data=data_train, 
                                  p=0.5, seed=1, 
                                  method="over")$data

#We know have a nearly balanced data set, let's see how we can improve prediction now
table(balanced$Attrition)



#we'll apply our hyper again here


for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = Attrition ~ ., 
    data            = data_train, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
  )
  
  
  # grab error of each one
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}


#now arrange by rmse to examine best performing models


hyper_grid %>%
  arrange(rmse) %>%
  head(10)

RF3 <- ranger(
  Attrition ~ ., 
  data = balanced,
  mtry = 1,
  importance = 'impurity',
  min.node.size = 1,
  sample.fraction = .50,
  num.trees = n_features*10,
  verbose = TRUE,
  seed = 3
)

#Oh baby, a big OOB improvement
RF3


#Still a low specificity, but better balanced accuracy and perhaps an acceptable accuracy and sensitivity
pred2 <- predict(RF3, data_test)
confusionMatrix(pred2$predictions, data_test$Attrition)


#Finally, let's examine variable importance
#Total working years and age seem to important players here and may require further inspection in practice
(importance(RF3))




