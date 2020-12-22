install.packages("ltm")

require(readxl)
require(ltm)


#load data
data <- read.csv("/Users/joehome/Desktop/AAMAIO/IRT 2PL Data.csv")


#ltm create a 2PL model
latent <- ltm(data ~ z1)

#view output
latent



#plot the output


plot(latent, items = 1:3)

plot(latent, items = 4:6)
















