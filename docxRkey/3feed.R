
rm( list=ls() )
library(igraph)

 A <- matrix(0,3,3)
 A[1,2] <- A[2,1] <- 1
 A[1,3] <- A[3,1] <- 1
 g <- graph.adjacency(A)

 ll <- rbind( c(0,1), c( -1/2,0 ),  c(1/2,0) )


 pdf( file="3feed.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-1/2-.2,1/2+.2), ylim=c(-.2,1.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
