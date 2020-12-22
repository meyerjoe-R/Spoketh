install.packages("MatchIt")
require(MatchIt)
require(dplyr)
require(ggpubr)

#Propensity score matching helps us reduce selection bias
#when observing the differences between control and treatment groups

#Typically, logistic regression is used to identify covariates that may influence self-selection
#into A or B groups

#Then matching algorithms, typically nearest neighbor, are used to identify matches across groups


#After that, comparisons are made between groups, supporting causal inference



#download data from the matchit package
#This data contains various variables, and income
data(lalonde)


#if we examine unmatched differences between the treatment and control group,
#we find a differences in about $635 dollars
summary(lm(re78~treat,data=lalonde))


#This code will now match covariates using nearest neighbors

#Matches are based on propensity scores, so we have a balance in scores across groups
m.out1 <- matchit(treat ~ age + educ + race + nodegree + married + re74 + re75, data = lalonde, method = "nearest", distance = "logit")


#Examine standardized differences of groups before and after matching
s.out <- summary(m.out1, standardize = TRUE)
plot(s.out)



#We can also examine the distribution of matching across propensity scores
plot(m.out1, type = "jitter", interactive = FALSE)

#We see we have an equal number of matched subjects across groups
summary(m.out1)

#Create our matched dataset

matched <- match.data(m.out1)

#examine matched data

group_by(matched, treat) %>%
  summarise(
    count=n(),
    mean = mean(re78, na.rm = TRUE),
    sd = sd(re78, na.rm = TRUE),
    education = mean(educ, na.rm = TRUE)
  )

#Compare groups, we see large overlap, may want to consider removing outliers in practice
ggboxplot(matched, x = "treat", y = "re78", 
          color = "treat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Income", xlab = "Group")

#tests

summary(lm(re78~treat+age + educ + race + nodegree + married + re74 + re75, data=matched))

t.test(re78~treat, data=matched, paired = FALSE)


