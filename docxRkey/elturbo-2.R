

drawit <- function(x,y)
 {
  symbols( x, y+1, circles=1 , lwd=1/2 , add=TRUE, inches=FALSE, col="grey")
  symbols( x, y-1, circles=1 , lwd=1/2 , add=TRUE, inches=FALSE, col="grey")
 }

jug <- function( x, y, vv, pp=19, cx=1.7 )
 { 
  # x, y are centers of the figure 8
  # vv is a vector over the 8 standard positions, whose in each
  # among 8 standard positions 
  dx <- c(0,1,0,-1,0,1,0,-1)
  dy <- c(2,1,0,-1,-2,-1,0,1)
  for( i in 1:8 )
   {
    if( !is.na(vv[i]) )
     {
      #points( x+dx[i], y+dy[i], pch=pp, cex=cx, col=vv[i] )
      text( x+dx[i], y+dy[i], pch=pp, cex=cx, labels=vv[i] )
     }
   }
 }

pass <- function(x,y,i,j,...)
 {
  dx <- c(0,1,0,-1,0,1,0,-1)
  dy <- c(2,1,0,-1,-2,-1,0,1)
  lines( x + dx[c(i,j)] , y+ dy[c(i,j)], lwd=1.7, col="black", ... )
 }


pdf( file="elturbo-2.pdf" , height=(26/3), width=(18/3) )
par( mar=c(0,0,0,0) )




plot( 0,0, xlim=c(0,18), ylim=c(0,26), type="n", axes=FALSE )

#text( 9, 26, "El Turbo (Madison, 8/25/11)" )
lines( c(1,17), rep(22,2) )
lines( c(1,17), rep(19,2) )
lines( c(1,17), rep(16,2) )
lines( c(1,17), rep(13,2) )
lines( c(1,17), rep(10,2) )
lines( c(1,17), rep(7,2) )
lines( c(1,17), rep(4,2) )
lines( rep(17,2), c(22,19) )
lines( rep(1,2), c(19,16) )
lines( rep(17,2), c(16,13) )
lines( rep(1,2), c(13,10) )
lines( rep(17,2), c(10,7) )
lines( rep(1,2), c(7,4) )
points( 1,22, pch=19, cex=1.5 )
points( 17,4, pch=19, cex=1.5 )

xc <- 3; yc <- 22
drawit( xc, yc )
vv <- c("A", NA, "C", "D", NA, "B", "C", NA )
pass( xc, yc, 1, 6 )
jug( xc, yc, vv )

xc <- 7; yc <- 22
vv <- c("A", NA, NA, "D", "B", "C", NA, NA )
drawit( xc, yc )
pass( xc, yc, 1, 4 )
jug( xc, yc, vv )

xc <- 11; yc <- 22
vv <- c("A", NA, "D", "B", NA ,  "C", "D", NA )
drawit( xc, yc )
pass( xc, yc, 1, 6 )
jug( xc, yc, vv )

xc <- 15; yc <- 22
vv <- c("A", "D", NA, "B", "C",NA, NA, NA )
drawit( xc, yc )
pass( xc, yc, 1, 4 )
pass( xc, yc, 2, 5, lty=2 )
jug( xc, yc, vv )

# 2nd row

xc <- 3; yc <- 16
vv <- c(NA,  "D", "B", NA, "C",NA, "B", "A"  )
drawit( xc, yc )
pass( xc, yc, 2, 5 )
jug( xc, yc, vv )

xc <- 7; yc <- 16
vv <- c( "D", "B", NA, NA, "C",NA, NA, "A"  )
drawit( xc, yc )
pass( xc, yc, 5, 8 )
jug( xc, yc, vv )

xc <- 11; yc <- 16
vv <- c( NA, "B", "A", NA, "C",NA, "A", "D"  )
drawit( xc, yc )
pass( xc, yc, 5, 2 )
jug( xc, yc, vv )

xc <- 15; yc <- 16
vv <- c( "B", NA, NA, NA, "C","A", NA, "D"  )
drawit( xc, yc )
pass( xc, yc, 5, 8 )
pass( xc, yc, 1, 6, lty=2 )
jug( xc, yc, vv )

## row 3

xc <- 3; yc <- 10
vv <- c( "B", NA, "D",  "C", NA, "A", "D" ,NA  )
drawit( xc, yc )
pass( xc, yc, 1, 6 )
jug( xc, yc, vv )

xc <- 7; yc <- 10
vv <- c( "B", NA, NA,  "C", "A", "D" , NA, NA )
drawit( xc, yc )
pass( xc, yc, 1, 4 )
jug( xc, yc, vv )

xc <- 11; yc <- 10
vv <- c( "B", NA,  "C", "A", NA, "D" , "C", NA )
drawit( xc, yc )
pass( xc, yc, 1, 6 )
jug( xc, yc, vv )

xc <- 15; yc <- 10
vv <- c( "B",  "C", NA, "A", "D" , NA, NA, NA )
drawit( xc, yc )
pass( xc, yc, 1, 4 )
pass( xc, yc, 2, 5, lty=2 )
jug( xc, yc, vv )

# last row
xc <- 3; yc <- 4
vv <- c( NA, "C", "A", NA, "D" , NA, "A", "B" )
drawit( xc, yc )
pass( xc, yc, 5, 2 )
jug( xc, yc, vv )

xc <- 7; yc <- 4
vv <- c( "C", "A", NA, NA, "D" , NA, NA, "B" )
drawit( xc, yc )
pass( xc, yc, 5, 8 )
jug( xc, yc, vv )

xc <- 11; yc <- 4
vv <- c( NA, "A", "B", NA, "D" , NA, "B", "C" )
drawit( xc, yc )
pass( xc, yc, 5, 2 )
jug( xc, yc, vv )

xc <- 15; yc <- 4
vv <- c(  "A", NA, NA, NA, "D",  "B", NA,  "C" )
drawit( xc, yc )
pass( xc, yc, 5, 8 )
pass( xc, yc, 1, 6 , lty=2)
jug( xc, yc, vv )






dev.off()
