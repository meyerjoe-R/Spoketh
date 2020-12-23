require(ggmap)
require(readxl)
install.packages("DT")
require(DT)
install.packages("dygraphs")

datatable(data, options = list(pageLength = 10))


ggmap::register_google(key = "AIzaSyCzk5YeyKH7zldNkLTr73C6ct6Ny6fUuys")





data <- read_xls("/Users/joehome/Library/Containers/com.microsoft.Excel/Data/Desktop/us-zip-code-latitude-and-longitude.xls")



data <- na.omit(data)


data <- subset(data, select = -c(Zip, City, State, 
                                 Timezone, geopoint))  #remove unecessary columns


#get the map, location is the US
#satellite map
#zoom of 4 can see a continent

map1 <- ggmap(get_googlemap(location = 'united states',
                                maptype = "satellite",
                            zoom = 4,
                            source = 'google',
                            color = "color"))





map1 + geom_point(aes(x = Longitude, y= Latitude,
                  color = Revenue),
              data = data, size = 0.5) +
                theme(legend.position = "right") +
                      scale_color_gradient(low = "red", high = "green")





data3 <- read_xlsx("/Users/joehome/Desktop/AAMAIO/Data Visualization/Organizational Metrics.xlsx")




require(corrplot)  









data3 <- subset(data3, select = -c(Region))  #remove unecessary columns



matrix <- cor(data3)





corrplot(matrix, method = "circle",
         title = "Regional Metrics")



data2 <- read_xlsx("/Users/joehome/Desktop/AAMAIO/Data Visualization/Organizational Metrics.xlsx")


require(tidyverse)

data2 <- as.data.frame(data2)

p <- ggplot(data2, aes(x=as.factor(Region), y=Turnover)) +
  geom_bar(stat = "identity", fill=alpha("blue", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  coord_polar(start = 0)

p










empty_bar <- 4  #space between end of each group




# Add lines to the initial dataset

to_add <- matrix(NA, empty_bar, ncol(data2))
colnames(to_add) <- colnames(data2)
data3 <- rbind(data2, to_add)
data2$region <- seq(1, nrow(data2))


# Get the name and the y position of each label


label_data <- data2
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$region-0.5)/number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)


exdata <- data.frame(
  individual=paste("Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T))












p <- ggplot(data2, aes(x = as.factor(region), y=Turnover, fill=region)) +
  geom_bar(stat = "identity", alpha=0.5) +
  ylim(-100,120) +  #limit the plot, negative value is inner circle size and positive is size over bars
  theme(plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar() + #coordinate system
  geom_text(data = label_data, aes(x=region, y=Turnover+10, 
            hjust=hjust), label = region,
            color="black", fontface="bold",alpha=0.6, 
            size=2.5, angle= label_data$angle, inherit.aes = FALSE )
           
p      
