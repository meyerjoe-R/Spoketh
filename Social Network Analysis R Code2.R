
install.packages("igraph") #install it baby
require(igraph) #load it 
install.packages("rgl")
require(rgl)

g <- graph(c(1,2,3,4)) #create a simple network
plot(g, 
     vertex.color = "purple",
     vertex.size = 50,
     edge.color = "red")


g1 <- graph(c(1,2,2,3,4,1)) #create another network
plot(g1, 
     vertex.color = "purple",
     vertex.size = 50,
     edge.color = "red")


g1[] #view matrix representation, first node is connected to second


g2 <- graph(c("Joe", "The Wall", "The Wall", "Lin", "Lin", "Grandma", "Grandma", "Joe", "The Wall", "Joe")) #try some names

plot(g2,
     vertex.color = "grey",
     vertex.size = 50,
     edge.color = "green") #visualize it


degree(g2) #view connections


degree(g2, mode = 'in') #view in degrees


degree(g2, mode = 'out') #view out degrees


edge_density(g2, loops = FALSE) #view density, total number of connections/number of potential connections


reciprocity(g2)  #view reciprocity in the graph, percentage of ties that are reciprocal


closeness(g2, mode = 'all', weights = NA) #examine centrality



#alright, data now yall


set.seed(123) #set the seed
network3 <- sample_pa(50) #create a network

plot(network3)

igraph_options(plot.layout=layout_as_tree)
plot(make_tree(20, 2))
plot(make_tree(50, 3), vertex.size=3, vertex.label=NA)
tkplot(make_tree(50, 2, mode="undirected"), vertex.size=10,
       vertex.color="green")
#some r example code


plot(network3)


networkdata <- read.csv("/Users/joehome/Library/Containers/com.microsoft.Excel/Data/Desktop/AAMAIO/network2.csv", header = TRUE)



d <- data.frame(networkdata$Employee.1, networkdata$Employee.2) #data frame time



network <- graph.data.frame(d, directed=TRUE) #directed graph




V(network) #view vertices/nodes


E(network) #view edges or connections




V(network)$label <- V(network)$name #use the names as our labels
V(network)$degree <- degree(network) #use degrees as well



V(network)$label #now can view matrix of labels

V(network)$degree #now can view matrix of degrees



hist(V(network)$degree,                      #examine frequency of degrees
       col = 'Yellow',
       main = 'Historgram of Node Degrees',
       ylab = 'Frequency',
       xlab = 'Degree of vertices')
     

set.seed(123) #set the seed bro

plot(network)

plot(network,
     vertex.color = rainbow(7),
     vertex.size = 15,
     arrow.size = 10,
     arrow.width = .5,
     curved = TRUE,
     layout = layout.graphopt)


plot(network,
     vertex.color = rainbow(7),     #colors for number of edges
     vertex.size = V(network)$degree*5, #degrees determine size
     arrow.size = 0.1,
     edge.arrow.size = 0.1,    
     layout=layout.fruchterman.reingold) #specify type of layout




id <- rownames(d) #disregard
d <- cbind(id=id, d) #disregard


plot(network,
     vertex.color = rainbow(7),     #colors for number of edges
     vertex.size = V(network)$degree*10, #degrees determine size
     arrow.size = 0.1,
     edge.arrow.size = 0.1, 
     label = NA,
     layout=layout.fruchterman.reingold) #specify type of layout


plot(network,
     vertex.color = rainbow(7),     #colors for number of edges
     vertex.size = V(network)$degree*0.5,   #degrees determine size
     layout=layout.auto) #specify type of layout


plot(network,
     vertex.color = rainbow(7),     #colors for number of edges
     vertex.size = V(network)$degree*0.5,   #degrees determine size
     layout=layout.graphopt) #specify type of layout


network2 <- tkplot(network, canvas.width = 450, canvas.height = 450)



#adapted from Dr. Bharatendra Rai 





