
# a circle for 5

rm( list=ls() )
library(igraph)



A <- matrix(0, 5, 5 )
A[1,3] <- 1; A[3,1] <- 1
A[1,4] <- 1; A[4,1] <- 1
A[2,4] <- 1; A[4,2] <- 1
A[2,5] <- 1; A[5,2] <- 1
A[3,5] <- 1; A[5,3] <- 1
g <- graph.adjacency(A)

A <- matrix(0, 5, 5 )
A[1,4] <- 1
A[4,2] <- 1
A[2,5] <- 1
A[5,3] <- 1
A[3,1] <- 1
g.star5 <- graph.adjacency(A)




A <- matrix(0, 5, 5 )
A[1,2] <- 1; A[2,1] <- 1
A[1,3] <- 1; A[3,1] <- 1
A[1,4] <- 1; A[4,1] <- 1
A[1,5] <- 1; A[5,1] <- 1
A[2,3] <- 1; A[3,2] <- 1
A[2,4] <- 1; A[4,2] <- 1
A[2,5] <- 1; A[5,2] <- 1
A[3,4] <- 1; A[4,3] <- 1
A[3,5] <- 1; A[5,3] <- 1
A[4,5] <- 1; A[5,4] <- 1


g.feast <- graph.adjacency(A)




pdf( file="star2.pdf", height=6, width=6 )
labs <- c("A", "B", "C", "D", "E" )
th <- c( pi/2, pi/2 - 2*pi/5 , pi/2 - 4*pi/5, pi/2 - 6*pi/5, pi/2 - 8*pi/5 )
yy <- sin(th)
xx <- cos(th)
ll <- cbind( xx, yy )
plot( g,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, vertex.label.cex=2 )

symbols( 0, 0, circles=1, add=TRUE , lwd=1, inches=FALSE , col="grey")
plot( g,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, add=TRUE , vertex.label.cex=2)
dev.off()

pdf( file="star5.pdf", height=6, width=6 )
labs <- c("I", "III", "V", "II", "IV" )
th <- c( pi/2, pi/2 - 2*pi/5 , pi/2 - 4*pi/5, pi/2 - 6*pi/5, pi/2 - 8*pi/5 )
yy <- sin(th)
xx <- cos(th)
ll <- cbind( xx, yy )
plot( g.star5,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, vertex.label.cex=2 )

symbols( 0, 0, circles=1, add=TRUE , lwd=1, inches=FALSE , col="grey")
plot( g.star5,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, add=TRUE , vertex.label.cex=2)
dev.off()

pdf( file="star5extra.pdf", height=6, width=6 )
labs <- c("I", "III", "V", "II", "IV" )
th <- c( pi/2, pi/2 - 2*pi/5 , pi/2 - 4*pi/5, pi/2 - 6*pi/5, pi/2 - 8*pi/5 )
yy <- sin(th)
xx <- cos(th)
ll <- cbind( xx, yy )
plot( g,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, vertex.label.cex=2 )

symbols( 0, 0, circles=1, add=TRUE , lwd=1, inches=FALSE , col="grey")
plot( g,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, add=TRUE , vertex.label.cex=2)
dev.off()






pdf( file="feast5.pdf", height=6, width=6 )
labs <- c("A", "B", "C", "D", "E" )


th <- c( pi/2, pi/2 - 2*pi/5 , pi/2 - 4*pi/5, pi/2 - 6*pi/5, pi/2 - 8*pi/5 )
yy <- sin(th)
xx <- cos(th)
ll <- cbind( xx, yy )


plot( g.feast,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, vertex.label.cex=2 )

symbols( 0, 0, circles=1, add=TRUE , lwd=1/2, inches=FALSE , col="grey")

plot( g.feast,  layout=ll, vertex.size=30, edge.arrow.size=.8, edge.arrow.width=.8,
                 edge.lty=1, edge.color="black", vertex.color="grey",
        edge.width=2, margin=c(0,.4,0,.4), vertex.label=labs, add=TRUE , vertex.label.cex=2)

dev.off()



