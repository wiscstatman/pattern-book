
rm( list=ls() )
library(igraph)

 A <- matrix(0,4,4)
 A[1,3] <- 1
 A[1,4] <- 1
 A[2,1] <- 1
 A[3,2] <- A[4,2] <- 1
 g <- graph.adjacency(A)

 ll <- rbind( c(0,1), c(0,0), c( -1,-1 ),  c(1,-1) )


 pdf( file="Y2K-v1.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-1.2,1.2), ylim=c(-1.2,1.2),
	edge.arrow.width=2, edge.width=3, vertex.label=c("A","B","C","D"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
