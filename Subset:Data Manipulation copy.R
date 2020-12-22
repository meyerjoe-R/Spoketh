

#column rename
colnames(education) <- c("X","State","Region","Urban.Population","Per.Capita.Income","Minor.Population","Education.Expenditures")



#take rows, then columns
ed_exp1 <- education[c(10:21),c(2,6:7)]


#omit rows, then columns
ed_exp2 <- education[-c(1:9,22:50),-c(1,3:5)]



#take rows where region equals two, take out column names from eduction data set
ed_exp3 <- education[which(education$Region == 2),names(education) %in% c("State","Minor.Population","Education.Expenditures")]



#subset takes the dataframe, the conditons you want, and the columns you want



ed_exp4 <- subset(education, Region == 2, select = c("State","Minor.Population","Education.Expenditures"))



#use dplyr


#filter is the first argument, condition you want, grabs all those rows


#then, select takes the column names

ed_exp5 <- select(filter(education, Region == 2),c(State,Minor.Population:Education.Expenditures))






