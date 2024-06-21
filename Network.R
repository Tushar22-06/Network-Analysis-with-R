library(igraph)
library(readxl)

# Read new dataset
NetworK_Strong_data <- read_excel("C:/Users/KIIT/Downloads/N/NetworK_Strong_data.xls")
View(NetworK_Strong_data)

# Assuming the Excel file has columns named 'first' and 'second'
if(!all(c('first', 'second') %in% colnames(NetworK_Strong_data))) {
  stop("The required columns 'first' and 'second' are not present in the Excel file.")
}

# Create a data frame with the relevant columns for graph creation
y <- data.frame(NetworK_Strong_data$first, NetworK_Strong_data$second)

# Create the network graph
net <- graph.data.frame(y, directed=T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# Histogram of node degree
hist(V(net)$degree, main = "Histogram of Node Degree", xlab = "Degree", ylab = "Frequency")

# Network diagram
plot(net, main = "Network Diagram")

# Highlighting degrees & layouts
plot(net,
     vertex.color = rainbow(length(V(net))),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold,
     main = "Network Diagram with Degree Highlighted")

# Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(length(V(net))),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(length(V(net))),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))

# Community detection
net_undirected <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net_undirected)
plot(cnet, net_undirected, main = "Community Detection")
