
rm( list=ls() )
library(igraph)

 A <- matrix(0,3,3)
 A[1,2] <- A[2,1] <- 1
 A[1,3] <- A[3,1] <- 1
 A[2,3] <- A[2,3] <- 1
 g <- graph.adjacency(A)

 ll <- rbind( c(0,1), c( -0.577,0 ),  c(0.577,0) )


 pdf( file="tri.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-.577-.2,.577+.2), ylim=c(-.2,1.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
