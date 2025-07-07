
** messing up the intermediate steps **
** use reboot-ani.R 

# preparing for IJA
# an animation attempt on lemniscate
# adding the in-between passing beat configurations compared to reboot-ani.R

# I have notes for a moving pattern, 'lemniscate', where movers follow the lemniscate of Bernoulli trajectory.
# There's an intersting cross-over maneuver described here


## see https://mathworld.wolfram.com/Lemniscate.html
aa <- 22.8
t0 <- seq( 0, 2*pi, length=100 )
x0 <- aa*cos(t0)/( 1 + (sin(t0))^2 )
y0 <- aa*sin(t0)*cos(t0)/(1+(sin(t0))^2 )

###

library(jacobi)

sl(1+1i) * cl(1+1i) # should be 1
## | the lemniscate ####
# lemniscate parameterization
p <- Vectorize(function(s) {
  a <- Re(cl(s))
  b <- Re(sl(s))
  c(a, a * b) / sqrt(1 + b*b)
})
# lemnniscate constant

N <- 22
ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))

#aa <- 11*6/(2*ombar)   ### 11 jugglers x 6 ft apart

aa <- 22.8

rr <- numeric(N)
for( i in 1:N ){ rr[i] <- Re( sl( 2*(ombar*(i)/N) ) ) }
xx <- rr*sqrt( (1+rr^2)/2 )
yya <- sqrt( rr^2 * (1-rr^2 )/2 )
sgn <- (-1)^floor( 4*(1:N)/N ) 
yy <- yya*sgn

xx2 <- xx*aa
yy2 <- yy*aa

rn.x <- range( xx2 )
rn.y <- range( yy2 )
kap <- (rn.y[2] -rn.y[1])/(rn.x[2]-rn.x[1])
#print(kap)


# knock out center point
xx2[c(11,22)] <- NA; yy2[c(11,22)] <- NA



## tangent slopes

yp <- (aa^2 * xx2 - 2*xx2^3 -2*xx2*yy2^2)/(2*xx2^2 * yy2 + 2* yy2^3 + aa^2 * yy2 )

# so the negatives are slopes of the ortho lines

# y = y0 + (slope)*(m-x0) = (y0-slope*x0) + slope*x

ss <- -1/yp
ii <- (yy2 - ss*xx2)
ok <- !is.na(ii) & !is.na(ss)
#for( j in 1:length(ii) ){ if(ok[j]){ abline( a=ii[j], b=ss[j] )} }
## works

# let's do segments

wid <- 1.2
wx <- wid/sqrt( 1+ ss^2 )
wy <- wid/sqrt( 1+1/ss^2 )

xxleft <- xx2 - sign(ss)*wx
xxright <- xx2 + sign(ss)*wx
yyleft <-yy2 - wy
yyright <-  yy2+wy

quad <- rep(0,length(ss) )
quad[ xx2 > 0 & yy2 > 0 ] <- 1
quad[ xx2 < 0 & yy2 > 0 ] <- 2
quad[ xx2 < 0 & yy2 < 0 ] <- 3
quad[ xx2 > 0 & yy2 < 0 ] <- 4
## flip left and right in quad 1 and quad 3
tmpx <- xxleft
xxleft[quad ==1 | quad == 3] <- xxright[quad ==1 | quad==3]
xxright[quad ==1 | quad == 3] <- tmpx[quad ==1 | quad==3]
tmpy <- yyleft
yyleft[quad ==1 | quad == 3] <- yyright[quad ==1 | quad==3]
yyright[quad ==1 | quad == 3] <- tmpy[quad ==1 | quad==3]


hh.left <- cbind( xxleft, yyleft )  ## coords for group going clockwise on right...squares in Jan version
hh.left[11,] <- hh.left[10,]  ## this is the stopsign
hh.left[22,] <- hh.left[21,]  ## this is the stopsign

nms.left <- rep(NA,22)
nms.left[c(1,3,5,7,9,11,13,15,17,19,21)] <- rev( c("A","B","C","D","E","F","G","H","I","J","K") )
nms.left[seq(2,22,by=2)] <- rev( c("A","B","C","D","E","F","G","H","I","J","K") )



hh.right<- rbind( c(xxright[1], yyright[1]),
			cbind(xxright[1:21], yyright[1:21] ) )
hh.right[12,] <- hh.right[13,]
  ## coords for group going counterclockwise on right...circlesin Jan version
hh.right <- hh.right[22:1,]

nms.right<- rep(NA,22)
nms.right[c(2,4,6,8,10,12,14,16,18,20,22)] <- ( c("k","a","b","c","d","e","f","g","h","i","j") )
nms.right[seq(1,21,by=2)] <-  c("k", "a","b","c","d","e","f","g","h","i","j") 
nms.right <- rev(nms.right)

dr <- data.frame( nms.right, hh.right )

## 
od <- seq(1,21,by=2)
ev  <- seq(2,22,by=2)
ev0 <- ev; od0 <- od

foo <- c(xxleft, xxright)
xl <- range( foo[!is.na(foo)] )
foo <- c(yyleft, yyright)
yl <- range( foo[!is.na(foo)] )

## lines are still wrong

library(animation)


xstop <- 3/2
ystop <- 6
x.odd <- rbind(  c( hh.left[1,1], hh.right[19,1] ), 
		 c( hh.left[3,1], hh.right[17,1] ),
		 c( hh.left[5,1], hh.right[15,1] ),
		 c( hh.left[7,1], hh.right[13,1] ), 
		 c( hh.left[9,1], xstop ), 
		 c( hh.left[11,1], hh.right[11,1] ), 
		 c( -xstop, hh.right[9,1] ), 
		 c( hh.left[13,1], hh.right[7,1] ) ,
		 c( hh.left[15,1], hh.right[5,1] ) ,
		 c( hh.left[17,1], hh.right[3,1] ) ,
		 c( hh.left[19,1], hh.right[1,1] ) )

y.odd <- rbind(  c( hh.left[1,2], hh.right[19,2] ), 
		 c( hh.left[3,2], hh.right[17,2] ),
		 c( hh.left[5,2], hh.right[15,2] ),
		 c( hh.left[7,2], hh.right[13,2] ), 
		 c( hh.left[9,2], -ystop ),
		 c( hh.left[11,2], hh.right[11,2] ), 
		 c( ystop, hh.right[9,2] ), 
		 c( hh.left[13,2], hh.right[7,2] ), 
		 c( hh.left[15,2], hh.right[5,2] ) ,
		 c( hh.left[17,2], hh.right[3,2] ) ,
		 c( hh.left[19,2], hh.right[1,2] ) )

x.even<- rbind(  c( hh.left[2,1], hh.right[18,1] ), 
		 c( hh.left[4,1], hh.right[16,1] ),
		 c( hh.left[6,1], hh.right[14,1] ),
		 c( hh.left[8,1], hh.right[12,1] ), 
		 c( hh.left[12,1], hh.right[8,1] ) ,  
		 c( hh.left[14,1], hh.right[6,1] ) ,  
		 c( hh.left[16,1], hh.right[4,1] ) ,  
		 c( hh.left[18,1], hh.right[2,1] ) ,  
		 c( hh.left[20,1], -xstop ) ,  
		 c( hh.right[20,1], +xstop ) ,  
		 c( hh.right[22,1], hh.left[22,1] ) )  

y.even<- rbind(  c( hh.left[2,2], hh.right[18,2] ), 
		 c( hh.left[4,2], hh.right[16,2] ),
		 c( hh.left[6,2], hh.right[14,2] ),
		 c( hh.left[8,2], hh.right[12,2] ),
		 c( hh.left[12,2], hh.right[8,2] ) ,  
		 c( hh.left[14,2], hh.right[6,2] ) ,  
		 c( hh.left[16,2], hh.right[4,2] ) ,  
		 c( hh.left[18,2], hh.right[2,2] ) ,  
		 c( hh.left[20,2], -ystop ) ,  
		 c( hh.right[20,2], +ystop ) ,  
		 c( hh.right[22,2],  hh.left[22,2] ) )  



cx <- 3

saveGIF( {
par( mar=rep(0,4) )
plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl, asp=1 )
lines( x0, y0, col="yellow", lwd=20 )
points( xx2, yy2, col="blue" )
  for( i in 1:nrow(x.odd) ){ lines( x.odd[i,], y.odd[i,], col="blue", lwd=2 ) }
text( hh.right[od,1], hh.right[od,2], nms.right[od], col="red", cex=cx)
text( hh.left[od,1], hh.left[od,2], nms.left[od] , cex=cx)
text(-15,0,"1", cex=2)
points( c(xstop,xstop,-xstop,-xstop), c(ystop,-ystop,ystop,-ystop), pch=19, col="green", cex=3 )


# beat 2
plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl , asp=1)
lines( x0, y0, col="yellow", lwd=20 )
points( xx2, yy2, col="blue" )
  for( i in 1:nrow(x.even) ){ lines( x.even[i,], y.even[i,], col="blue", lwd=2, lty=2 ) }
text( hh.right[ev,1], hh.right[ev,2], nms.right[(ev)], col="red", cex=cx)
text( hh.left[ev,1], hh.left[ev,2], nms.left[ev] , cex=cx)
text(-15,0,"2", cex=2)
points( c(xstop,xstop,-xstop,-xstop), c(ystop,-ystop,ystop,-ystop), pch=19, col="green", cex=3 )

cnt <- 2
while( cnt <= 21 )
 {
  # shift the labels
  shift <- c( 11, 1:10 )
  od <- od[shift]
  ev <- ev[shift]

mid.right.x <- (hh.right[od0,1] + hh.right[od0[shift],1])/2
mid.right.y <- (hh.right[od0,2] + hh.right[od0[shift],2])/2
plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl , asp=1)
lines( x0, y0, col="yellow", lwd=20 )
points( xx2, yy2, col="blue" )
text( mid.right.x, mid.right.y, nms.right[od], col="red", cex=cx )

  ## odd beat
  cnt <- cnt+1
  plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl , asp=1)
  lines( x0, y0, col="yellow", lwd=20 )
  points( xx2, yy2, col="blue" )
  text( hh.right[od0,1], hh.right[od0,2], nms.right[od], col="red", cex=cx)
  text( hh.left[od0,1], hh.left[od0,2], nms.left[od], cex=cx )
  text(-15,0,cnt, cex=2)
  for( i in 1:nrow(x.odd) ){ lines( x.odd[i,], y.odd[i,], col="blue", lwd=2 ) }
  points( c(xstop,xstop,-xstop,-xstop), c(ystop,-ystop,ystop,-ystop), pch=19, col="green", cex=3 )



  # even beat
  cnt <- cnt+1
  plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl, asp=1 )
  lines( x0, y0, col="yellow", lwd=20 )
  points( xx2, yy2, col="blue" )
  text( hh.right[ev0,1], hh.right[ev0,2], nms.right[ev], col="red", cex=cx)
  text( hh.left[ev0,1], hh.left[ev0,2], nms.left[ev], cex=cx )
  text(-15,0,cnt, cex=2)
  for( i in 1:nrow(x.even) ){ lines( x.even[i,], y.even[i,], col="blue", lwd=2, lty=2 ) }
  points( c(xstop,xstop,-xstop,-xstop), c(ystop,-ystop,ystop,-ystop), pch=19, col="green", cex=3 )
 }

}, interval=1.5, movie.name="lem2.gif", ani.width=960, ani.height=960 )
