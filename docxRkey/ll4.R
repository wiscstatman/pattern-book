# more locked lines
rm( list=ls() )
library(igraph)

##  A B C D  [ A and B face to the right; C and D face to the left]

 A <- matrix(0,4,4)
 A[1,4] <- 1
 A[1,3] <- A[3,1] <- 1
 A[2,3] <- A[3,2] <- 1
 A[2,4] <- A[4,2] <- 1
 A[4,1] <- 1

 g <- graph.adjacency(A)

 ll <- rbind( c(0,0), c(1.2,0), c( 1.8,0 ),  c(3,0) )


 pdf( file="ll4.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-0.2,3.2), ylim=c(-0.2,0.2),
	edge.arrow.width=1, edge.width=3, vertex.label=c("A","B","C","D"),
	edge.color="black" , rescale=FALSE, vertex.color="grey", edge.curved=1,
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
