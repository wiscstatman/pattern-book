
rm( list=ls() )
library(igraph)

 A <- matrix(1,4,4)
 diag(A) <- 0
 g <- graph.adjacency(A)

 ll <- rbind( c(0,-1), c( 0,1 ), c(1,0), c(-1,0) )

 pdf( file="box2.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll, xlim=c(-1.2,1.2), ylim=c(-1.2,1.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
