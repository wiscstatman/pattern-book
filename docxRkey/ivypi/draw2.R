
# updated Oct 25

rm( list=ls() )
library(igraph)
#library(plotrix)  ## use draw.arc function


# adjacency matrix for ivy-pi

A <- matrix(0, 10, 10 ) ## 10 positions
A[1,2] <- 1
A[2,3] <- 1
A[3,4] <- 1
A[4,5] <- 1
A[5,6] <- 1
A[6,7] <- 1
A[7,8] <- 1
A[8,9] <- 1
A[9,10] <- 1
A[10,1] <- 1

g <- graph.adjacency(A)

# give edge attributes I think

ll <- rbind( c(-1,0),
	c( -1/4, 1*0.866/2 ),
        c( 1/4, -1*.866/2), 
        c( 5/8, -1.6*.866/2),                              # new 4
        c( 1, -1.6*.866/3),                      # new 5
        c( 1, 0),
        c( 1/4, 1*.866/2),
        c( -1/4, -1*.866/2 ),
        c( -5/8, -1.6*.866/2),                      
        c( -1, -1.6*.866/3) )



A2 <- matrix(0, 10, 10 ) ## 10 positions
A2[1,7] <- 1; A2[7,1] <- 1
A2[6,2] <- 1; A2[2,6] <- 1
A2[4,8] <- 1; A2[8,4] <- 1
A2[3,9] <- 1; A2[9,3] <- 1
g2 <- graph.adjacency(A2)

pdf( file="ivypi-pass-2.pdf", height=5, width=5 )
par( mar=rep(0,4)  )
plot( g2, layout=ll/1.05 ,
        vertex.size=23, edge.arrow.size=0, edge.arrow.width=0,
                 edge.lty=1, edge.color="grey",
                edge.width=4, vertex.color="grey", margin=c(0,.4,0,.4), asp=0,
                rescale=FALSE, vertex.label.cex=1.5 )
symbols( -1/2, 0, circles=1/2, add=TRUE, lwd=1/4 , inches=FALSE, col="grey" )
symbols( 1/2, 0, circles=1/2, add=TRUE , lwd=1/4, inches=FALSE, col="grey" )

plot( g2, layout=ll/1.05 ,
        vertex.size=23, edge.arrow.size=0, edge.arrow.width=0,
                 edge.lty=1, edge.color="black",
                edge.width=3, vertex.color="grey",  asp=0,
                rescale=FALSE, add=TRUE, vertex.label.cex=1.5 )
dev.off()



pdf( file="ivypi-pass-2b.pdf", height=5, width=5 )
par( mar=rep(0,4)  )
plot( g2, layout=ll/1.05 ,
        vertex.size=23, edge.arrow.size=0, edge.arrow.width=0,
                 edge.lty=1, edge.color="grey",
                edge.width=4, vertex.color="grey", margin=c(0,.4,0,.4), asp=0,
                rescale=FALSE, vertex.label=NA )
symbols( -1/2, 0, circles=1/2, add=TRUE, lwd=1/4 , inches=FALSE, col="grey" )
symbols( 1/2, 0, circles=1/2, add=TRUE , lwd=1/4, inches=FALSE, col="grey" )

plot( g2, layout=ll/1.05 ,
        vertex.size=23, edge.arrow.size=0, edge.arrow.width=0,
                 edge.lty=1, edge.color="black",
                edge.width=3, vertex.color="grey",  asp=0,
                rescale=FALSE, add=TRUE, vertex.label = NA )
dev.off()


