install.packages("tidyverse")
install.packages("cluster")
install.packages("VIM")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("fps")
install.packages("pvclust")



require(tidyverse)  #data manipulation
require(cluster)  #clustering
require(readxl) #data
require(VIM) #examine missing daa
require(ggpubr) #visual
require(ggplot2) #visual
require(corrplot) #corr
require(RColorBrewer) #colors
require(pvclust)  #bootstrap




data1 <- read_xlsx("/Users/joehome/Desktop/AAMAIO/Cluster Analysis and Latent Profile Analysis/country_clusters.xlsx")


str(data1) #view structure

data1 <- na.omit(data1) #remove NAs


matrix1 <- cor(data1) #corr



corrplot(matrix1, type = "upper", order = "hclust",
         col=brewer.pal(n=8, name = "RdYlBu")) #corr


ggplot(data1, aes(x = data1$Revenue, y = data1$Stock)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw()


data1 <- scale(data1) #standardize the data to level out units


head(data1)  #check it out




#example graph


kmean_withinss <- function(k) {
  cluster <- kmeans(data1, k)
  return (cluster$tot.withinss)
}

#create a function that runs k times

#run the function k times

#store the total within clusters sum of squares


max_k <- 3  #maximum number is 20


#run the function kmeans 

wss <- sapply(2:max_k, kmean_withinss) 


#create a data frame with the output


elbow <- data.frame(2:max_k, wss)


ggplot(elbow, aes(x=X2.max_k, y = wss)) +
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = seq(1,20, by = 1))

cluster1 <-kmeans(data1, nstart = 25, 2) #clusta



str(cluster1)  #check it out


clusplot(data1, cluster1$cluster, color = TRUE, shade = TRUE, lines = 0)


d <-dist(data1, method = "euclidian") #distance matrix

fit <- hclust(d, method = "ward.D")  #hierarchical cluster, with wards


plot(fit) #model


group <-cuttree(fit, k=2)  #examine dendogram


rect.hclust(fit, k=2, border="red")  #examine dendogram



#look at p-values for hierarchical clustering, uses bootstrapping

#high p values = good


data(Boston, package = "MASS")  #grab some data



fit1 <- pvclust(Boston, nboot =1000, method.hclust = "ward",
                method.dist="euclidian")


plot(fit1)   #examine



pvrect(fit1, alpha=.95)  #examine groups highly supported

#LPA - find clusters of "individuals" based on latent profiles


library(tidyverse)  

#grab some data yall


install.packages("tidyLPA")

#Now, I will utilize latent profile analysis quickly, which can identify latent subpopulations within a dataset


require(tidyLPA)
require(dplyr)

#bring in a dataset
#impute
#estimate 3 profiles

data <- pisaUSA15[1:100, ] 

d1 <- pisaUSA15[1:100, ] %>%
  single_imputation() %>%
  estimate_profiles(1:3)

d1


d2 <- pisaUSA15[1:100, ] %>%
  single_imputation() %>%
  estimate_profiles(3)



plot_profiles(d2)


plot_profiles(d2, to_center = TRUE)




write.csv(data, file = "LPAdata.csv")



