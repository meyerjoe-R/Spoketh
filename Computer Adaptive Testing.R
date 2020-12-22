install.packages("catR")
require(catR)


#load data
data(tcals)
?tcals

#create a bank, take out content balancing info


bank <- as.matrix(tcals[,1:4])



#how should it start?
start <- list(nrItems = 1, theta = 0)



#create item select parameters, maximum likelihood and maximum fisher

test <- list(method = "ML", itemSelect = "MFI")




#EAP for final, Expected a posteriori (EAP) estimator 

final <- list(method = "EAP")



#stop at precision of .3

stop <- list(rule = c("precision", "length"), thr = c(0.3, 50))




res <- randomCAT(0, bank, start = start, test = test, stop = stop, final = final)



res


plot(res)




#simulation, with 10 respondents, create a list
thetas <- rnorm(10)



#dichotomous model, thetas are -4 to 4 in simulation

ressim <- simulateRespondents(thetas = thetas, itemBank = bank, model = NULL,
                              start = start, stop = stop, final = final)



ressim

plot(ressim)


#Accuracy shows us how good it is

#test length

#conditional test length: theta and length

#examine mean standard error with theta

#exposure rate shows us how items are shown across people, and if it reached the limit

#rmse for between true and estimated ability levels

#create another response matrix

stop2 <- list(rule = "classification", thr = 1.0, alpha = 0.05)


#another example with different stopping parameters


ressim2 <- simulateRespondents(thetas = thetas, itemBank = bank, model = NULL,
                              start = start, stop = stop2)


ressim2

plot(ressim2)



#polytomous

#create some more items, 100, maximum number of response categories is 3
#model is graded response



m.GRM <- genPolyMatrix(items = 100, nrCat = 5, model = "GRM")

#create a matrix

m.GRM <- as.matrix(m.GRM)


# CAT options


#thetas, start select is maximum fisher information
start3 <- list(theta = c(-1, 0), startSelect = "MFI")


#method is maximum likelihood, select is maximum expected information
test3 <- list(method = "ML", itemSelect = "MEI")

#stop based on precision
stop3 <- list(rule = "precision", thr = 0.4)

#EAP, predicted distribution of scores based on previous information 
final3 <- list(method = "EAP")

# CAT test
res3 <- randomCAT(0, m.GRM, model = "GRM", start = start3, test = test3, stop = stop3,
                 final = final3)


res3




