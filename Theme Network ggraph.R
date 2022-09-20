library(ggraph)
library(igraph)

nodes <- themes[,c(1,2,4)]
links <- themes[,c(3,1)]
names(links) <- c("from", "to")

nodes2 <- left_join(nodes, node_sizes, by = c("id" = "parent_id"))
nodes2$size[which(is.na(nodes2$size))] <- 0

# Some nodes don't have a parent, so "to" is NA
#   These lines aren't really edges, so remove them
links <- links[-which(is.na(links$from)),]

# get list of nodes that are actually connected to other nodes
conn.nodes <- unique(c(links$to, links$from))
# remove unconnected nodes
nodes2 <- nodes2[which(nodes2$id %in% conn.nodes),]

net <- graph_from_data_frame(d = links, vertices = nodes2, directed = T)

E(net)
V(net)
V(net)$size
E(net)$width <- 1

#first look at graph
ggraph(net, layout = 'lgl') +
  geom_edge_link() +
  geom_node_point()

#first look at graph
ggraph(net, layout = 'nicely') +
  geom_edge_link() +
  geom_node_point()

# Add labels
###   Alpha is calculated based on whether a node is a 
#####     progenitor node (has no parents of its own).
##### A boolean field has been created indicating whether
#####     a node is at the center of a group.
##### Given True = 1, False = 0, alpha is calculated 
#####     as either 0.7 * 1 + 0.3 = 1 for center nodes
#####     or        0.7 * 0 + 0.3 = 0.3 for other nodes
###   Node label size is also based on whether a node has
#####     parents or not.
##### Size = 2.5 * 1 + 3 = 5.5 for central nodes
#####      = 2.5 * 0 + 3 = 3 for child nodes

max_size <- max(nodes2$size)

pdf(file = 'LegoNetwork.pdf')
set.seed(100)
ggraph(net, layout = 'nicely') +
  geom_edge_link(color = 'gray') +
  geom_node_point(size = V(net)$center) +
  geom_node_text(cex = V(net)$center * 2.5 + 3, 
                 color = 'black', 
                 aes(label = name), 
                 alpha = 0.7 * V(net)$center + 0.3) +
  annotate(geom = "text", 
           label ='L', color = 'red', 
           size = 10,
           x = -15.18, y = 14.5) +
  annotate(geom = "text", 
           label ='e', color = 'blue', 
           size = 10,
           x = -14.8, y = 14.5) +
  annotate(geom = "text", 
           label ='g', color = 'orange', 
           size = 10,
           x = -14.38, y = 14.5) +
  annotate(geom = "text", 
           label ='o', color = 'dark green', 
           size = 10,
           x = -14, y = 14.5) +
  annotate(geom = "text", 
           label ='Theme Groups', 
           size = 10,
           x = -11.3, y = 14.5) +
  annotate(geom = "text",
           label = 'Data from https://rebrickable.com/downloads/',
           size = 3,
           x = 10, y = -15.5) +
  theme(panel.background = element_blank())
dev.off()
detach('package:igraph')