###################Are you Measuring Your Climate Correctly?###################
#######La Tech I-O PhD Program
#Matthew Vosburg 
#Joe Meyer
#Anna Crawford 
#Marley Walter 

#Code can be found at: <https://github.com/meyerjoe-R/Spoketh/blob/main/ClimateAnalysisR.R>


###################Set Up Environment###################
#install multilevel
install.packages("MASS")#install MASS
install.packages("multilevel") #install multilevel
install.packages("tidyverse")
install.packages("psych")


#require packages
require(MASS)
require(multilevel)# https://cran.r-project.org/web/packages/multilevel/multilevel.pdf
require(tidyverse)
require(psych)


###################Create simluated data###################
#Set seed allows for reproducibility of random processes
set.seed(111)

#Specify relationships between variables for simulated data
sig <- matrix(c(1.0, 0.8, 0.5, 0.2,
                0.8, 1.0, 0.5, 0.5,
                0.5, 0.5, 1.0, 0.5,
                0.2, 0.5, 0.5, 1.0), nrow = 4)

#Create dataframe with n of 5000
df <- data.frame(MASS::mvrnorm(n = 5000, mu = rep(0, 4), Sigma = sig, empirical = TRUE))
gripid <- sample(c("1","2"), 5000, replace=TRUE, prob = c(0.35,0.65)) #create group id variable
df <- cbind(df, gripid)

#View preview of data
head(df)


###################Basic Summary Statistics###################
#Summarize data
summary(df)
#View correlation matrix on numeric columns only
cor(df[sapply(df,is.numeric)])

#Basic summary stats (n, mean, median, trimmed mean, min, skew, etc.)
describe(df)

###################Rwg Documentation###################
#Source: https://www.rdocumentation.org/packages/multilevel/versions/2.6/topics/rwg
###Package Description: This function calculates the within group agreement measure rwg for single item measures 
#as described in James, Demaree and Wolf (1984). 
#The rwg is calculated as rwg = 1-(Observed Group Variance/Expected Random Variance). James et al. (1984)


###################Agreement###################
#Chan (1998): In a direct consensus or referent shift model
#Rwg value justifies validity of aggregated scores


#Create scale scores
df <- df %>% #pass back into dataframe
  rowwise() %>% #perform rowwise operations on dataframe
  mutate(Scale_score_1 = mean(c(X1, X2))) %>% #Create a new variable that is the mean of the first two items
  mutate(Scale_score_2 = mean(c(X3, X4))) #Same


#Examine agreement for single item (aggregated) scales 
#Standard code: rwg(x, grpid, ranvar=2)
###Arguments
# x: A vector representing the construct on which to estimate agreement.
#grpid: A vector identifying the groups from which x originated.


#Calculate rwg
### randvar needs to be changed for surveys that do not have 5 answer options
RWGOUT <- rwg(df$Scale_score_1, df$gripid, ranvar = 2)
#View rwg for groups
head(RWGOUT)
summary(RWGOUT)###.7 is considered a heuristic for adequate agreement

#Rwg.j Examine agreement for multi-item scales
#Documentation Source: https://www.rdocumentation.org/packages/multilevel/versions/2.6/topics/rwg.j.lindell
#Similar to the James, Demaree and Wolf (1984) rwg and rwg(j) indices. 
#The r*wg(j) index is calculated by taking the average item variability as the Observed Group Variance, and using the average item variability in the numerator of the rwg formula (rwg=1-(Observed Group Variance/ Expected Random Variance))
#Standard code: rwg.j.lindell(x, grpid, ranvar=2)

#Select item-level columns in the original data frame
df2 <- df %>%
  dplyr::select(-c(Scale_score_1, Scale_score_2))

#rwg.j for agreement at the item level
#x is now our data frame where each column is an item
### randvar needs to be changed for surveys that do not have 5 answer options
RWGJOUT <- rwg.j(df, df$gripid, ranvar = 2)
#Review results
RWGJOUT


###ICC (Intraclass Correlation Coefficient)
#Fist estimate an ANOVA on variable of interest with group ID as the factor
mod <-aov(X1~as.factor(gripid),data=df2)
summary(mod)

#Apply multilevel's ICC function to the ANOVA object
ICC1(mod)
ICC2(mod)

#Interpretation: e.g., ICC1 of .20 indicates that 20% of the variance
#in individual perceptions can be explained by group membership
#ICC2 is a measurement of group-mean reliability
#For example, an ICC2 of .92 indicates that groups can be reliably
#Differentiated in terms of the variable of interest
#Source: http://rsync.udc.es/CRAN/doc/contrib/Bliese_Multilevel.pdf


#Interested readers can see several data sources within Multilevel's r package

data("klein2000")
data("bhr2000")
data("lq2002")


