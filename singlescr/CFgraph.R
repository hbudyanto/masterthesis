library(ggplot2)
library(plyr)
library(arm)
library(reshape2)

load('~/PycharmProjects/dissertation/raw_data/features/features.itembasedCF/itembasedCFs_list.Rdata')
load('~/PycharmProjects/dissertation/raw_data/features/features.itembasedCF/itembasedCFs_colNames.Rdata')

CFcolnames
# Create item similarity table
dst1 <- data.matrix(CFlist[[16]])
dim1 <- ncol(dst1)

for (i in 1:dim1) {
  dst1[i,i] <- NA
}

image(1:dim1, 1:dim1, dst1, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim1, rownames(dst1), cex.axis = 0.5, las=3)
axis(2, 1:dim1, rownames(dst1), cex.axis = 0.5, las=1)
text(expand.grid(1:dim1, 1:dim1), sprintf("%0.2f", dst1), cex=0.6)

colnames(dst1) <- sub("^.......", "",colnames(dst1))
rownames(dst1) <- sub("^.......", "",rownames(dst1))

termMatrix <- dst1
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = 'undirected')
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (E(g)$weight) / max(E(g)$weight)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
jpeg('~/Desktop/example_graph.jpg', width=1000, height=1000, unit='px')
plot(g, layout=layout1)
dev.off()
