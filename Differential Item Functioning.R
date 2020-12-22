

install.packages("difR", repos = "http://cran.rstudio.com", dependencies = TRUE)
install.packages("nFactors")
install.packages("/Users/joehome/Downloads/difR_5.1.tgz", repos = NULL, type = "source")
install.packages("dplyr")
install.packages("faux")
install.packages("knitr")

install.packages("difR")

#What is DIF?


#Why does it matter?




require(difR)
require(faux)
require(ltm)
require(readxl)
require(dplyr)







data(verbal, package = "difR")
cor(verbal)


fit1 <- factanal(verbal, 4, rotation = "varimax")
print(fit1, digits = 2, cutoff = .3, sort = TRUE)


verbal <- verbal %>%
  select(S1WantShout, S2WantShout, S4WantShout, S1DoShout, S2DoShout, S4DoShout, Gender)


#estimate parameters
IRT2PL <- ltm(verbal ~ z1, IRT.param = TRUE)

#examine em
coef(IRT2PL)



#plot it
plot(IRT2PL, type = "ICC", items = 1:6, legend = TRUE)

#plot it
plot(IRT2PL, type = "IIC", items = 1:6, legend = TRUE)

#lords model
dif<- difLord(verbal, group = "Gender", model = "2PL", focal.name = 1)

dif
plot(dif)


#logistic - probability of answering item correctly, based on total test score and the interaction between group membership
#lord - chi square, compares parameters across the groups, sig means nonequal parameters across groups
#raju - examine space in item curves, area between curves to be zero assumed

dichoDif(verbal, group= "Gender", focal.name = 1, 
         method= c("Logistic", "Lord", "Raju"),
         model = "2PL")











