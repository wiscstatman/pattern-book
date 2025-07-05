
rm( list=ls() )
library(igraph)

 A <- matrix(0,5,5)

 #A[1,2] <- A[2,1] <- 1
 #A[1,3] <- A[3,1] <- 1
 #A[1,4] <- A[4,1] <- 1

 g <- graph.adjacency(A)

 # get the shapes right for A B vs C D E
 ll.a <- rbind( c(-1/2,2), c(1/2,2), c( -1,0 ), c(0,0), c(1,0) )
 ss <- sqrt(2)
 ll.b <- rbind( c(0,3/ss), c(0,2/ss), c( -1,0 ), c(0,0), c(1,0) )

 # wedgee
 ll.c <- rbind( c(-1,-1/2), c(1,-1/2), c(0, 3/ss), c(0,2/ss), c( 0,1/ss ) )

 # circle/star
 theta <- c( 2*pi/5, 2*2*pi/5, 3*2*pi/5, 4*2*pi/5, 2*pi )
 rr <- 1
 x <- rr*cos(theta)
 y <- rr*sin(theta)
 ll.d <- cbind(x,y)


 ## 2 on 3
 pdf( file="5a.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll.a, xlim=c(-1.2,1.2), ylim=c(-.2,2.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D", "E"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()


 # apollo
 pdf( file="5b.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll.b, xlim=c(-1.2,1.2), ylim=c(-.2,3.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D", "E"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()

 # wedgee
 pdf( file="5c.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll.c, xlim=c(-1.2,1.2), ylim=c(-1.2,3.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D", "E"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()

 ## circle/star
 
 pdf( file="5d.pdf" )
 par( mgp=rep(1,3) )
 plot(g,  layout=ll.d, xlim=c(-1.2,1.2), ylim=c(-1.2,1.2),
	edge.arrow.width=0, edge.width=3, vertex.label=c("A","B","C","D", "E"),
	edge.color="black" , rescale=FALSE, vertex.color="grey",
	vertex.label.cex=3, vertex.size=45 )
 dev.off()
