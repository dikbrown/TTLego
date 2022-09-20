library(igraph)

nodes <- themes[,1:2]
links <- themes[,c(3,1)]
names(links) <- c("from", "to")


# Some nodes don't have a parent, so "to" is NA
#   These lines aren't really edges, so remove them
links <- links[-which(is.na(links$from)),]

# get list of nodes that are actually connected to other nodes
conn.nodes <- unique(c(links$to, links$from))
# remove unconnected nodes
nodes <- nodes[which(nodes$id %in% conn.nodes),]

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

E(net)
V(net)

E(net)$width <- 1

#first look at graph
plot(net)

plot(net, edge.arrow.size = .1, edge.curved = 0.1)

deg <- degree(net, mode = "out") # calculate number of outgoing edges
V(net)$size <- deg/5
V(net)$alpha <- .1 + 0.9*deg/max(deg)

plot(net, edge.arrow.size = .1)

# layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
# # Remove layouts that do not apply to our graph.
# layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]
# 
# par(mfrow=c(5,3), mar=c(1,1,1,1))
# for (layout in layouts) {
#   print(layout)
#   l <- do.call(layout, list(net)) 
#   plot(net, edge.arrow.mode=0, layout=l, main=layout) }
# dev.off()

graph_attr(net, "layout") <- layout_as_star
plot(net)

graph_attr(net, "layout") <- layout_nicely
plot(net, edge.arrow.size = .05, 
     vertex.label.color = adjustcolor('green', alpha = deg),
     vertex.label.alpha = 0.1)

deg
detach('package:igraph')