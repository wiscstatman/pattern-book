
## still not working like it should (like line.pdf for 3)

rm( list=ls() )
library(igraph)

 A <- matrix(0,4,4)
 A[1,3] <- A[3,1] <- 1
 A[2,4] <- A[4,2] <- 1
 A[2,3] <- A[3,2] <- 1
 g <- graph.adjacency(A)

 ll <- rbind( c(0,0), c(1,0), c( 2,0 ),  c(3,0) )


 pdf( file="lockedlines.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-.2,3.2), ylim=c(-1.2,1.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
