library(tidyverse)
library(igraph)
my_nodes <- b %>% relocate(Name_abbr)


my_nodes_subset  <- my_nodes %>% filter(r1 >=4)
my_links <- as.data.frame(t(combn(my_nodes_subset$Name_abbr, 2)))
my_links <- setNames(my_links, c("from", "to"))
my_links$weight <-1
net_r1 <- graph_from_data_frame(d=my_links, vertices=my_nodes, directed = F )
#plot(net, vertex.label.color="black", vertex.label.dist=1,vertex.size=5)
# class(net)
#net
# plot(net)
#net[]
#E(net)
#V(net)

my_nodes_subset  <- my_nodes %>% filter(r2 >=4)
my_links <- as.data.frame(t(combn(my_nodes_subset$Name_abbr,2)))
my_links <- setNames(my_links, c("from", "to"))
my_links$weight <-1
net_r2 <- graph_from_data_frame(d=my_links, vertices=my_nodes, directed = F )
#plot(net, vertex.label.color="black", vertex.label.dist=1,vertex.size=5)


my_nodes_subset  <- my_nodes %>% filter(r3 >=4)
my_links <- as.data.frame(t(combn(my_nodes_subset$Name_abbr,2)))
my_links <- setNames(my_links, c("from", "to"))
my_links$weight <-1
net_r3 <- graph_from_data_frame(d=my_links, vertices=my_nodes, directed = F )
#plot(net, vertex.label.color="black", vertex.label.dist=1,vertex.size=5)


my_nodes_subset  <- my_nodes %>% filter(r4 >=4)
my_links <- as.data.frame(t(combn(my_nodes_subset$Name_abbr,2)))
my_links <- setNames(my_links, c("from", "to"))
my_links$weight <-1
net_r4<- graph_from_data_frame(d=my_links, vertices=my_nodes, directed = F )
#plot(net, vertex.label.color="black", vertex.label.dist=1,vertex.size=5)



plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_on_sphere)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_on_sphere, vertex.shape="none")
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_in_circle)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_in_circle, vertex.shape="none")
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_nicely)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_components)
plot(net, vertex.color="red", main="R1", layout=layout_components, vertex.label.dist=1,vertex.size=2, edge.color="red")
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_on_grid)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_dh)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_dh, vertex.shape="none")
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_drl)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_gem)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_graphopt)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_kk)
plot(net, vertex.color="orange", main="Tie: Hyperlink", layout=layout_with_lgl)

3par(mar=c(1,0,1,0))
par(mfrow=c(2,2), margin(-1,0,-1,0))   # plot twofigures - 1 rows, 1 ccolumns
  l <- layout_with_fr(net)
    plot(net_r1, vertex.color="red", main="R1", layout=layout_components, vertex.label.dist=1,vertex.size=0, edge.color="red")
    box(lty = '1373', col = 'red')
    plot(net_r2, vertex.color="red", main="R2", layout=layout_components, vertex.label.dist=1,vertex.size=2, edge.color="red")
    box(lty = '1373', col = 'red')
    plot(net_r3, vertex.color="red", main="R3", layout=layout_components, vertex.label.dist=1,vertex.size=2, edge.color="red")
    box(lty = '1373', col = 'red')
    plot(net_r4, vertex.color="red", main="R4", layout=layout_components, vertex.label.dist=1,vertex.size=2, edge.color="red")
    box(lty = '1373', col = 'red')
  dev.off()
