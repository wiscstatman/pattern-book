
rm( list=ls() )
library(igraph)

 A <- matrix(0,4,4)
 A[1,2] <- 1
 A[2,3] <- 1
 A[3,4] <- 1
 A[4,1] <- 1
 g <- graph.adjacency(A)

 ll <- rbind( c(3,1), c(2,0), c( 1,0 ),  c(0,1) )


 pdf( file="canoe.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-0.2,3.2), ylim=c(-0.2,1.2),
	edge.arrow.width=1, edge.width=3, vertex.label=c("A","B","C","D"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
