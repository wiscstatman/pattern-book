
# a switch sides feed
rm( list=ls() )
library(igraph)

 A <- matrix(0,6,6)
 A[1,2] <- A[2,1] <- 1
 A[1,3] <- A[3,1] <- 1
 A[1,4] <- A[4,1] <- 1
#
 A[5,3] <- A[3,5] <- 1
 A[6,3] <- A[3,6] <- 1

 A[2,5] <- 1
 A[4,6] <- 1

 g <- graph.adjacency(A)

 ll <- rbind( c(0,1), c( -1,0 ), c(0,0), c(1,0), c(-1,1), c(1,1) )

  le <- rep(1,12)
  le[c(5,10)] <- 3
  lw <- rep(3, 12)
  lw[le==3] <- 5


 pdf( file="swfeed.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-1.2,1.2), ylim=c(-.2,1.2),
	edge.arrow.width=0, edge.width=lw, vertex.label=c("A","B","C","D",
	"B", "D"), edge.lty = le, 
	edge.color="black" ,  rescale=FALSE, 
	
	vertex.color= c( rep("grey",4), rep("white",2) ),
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
