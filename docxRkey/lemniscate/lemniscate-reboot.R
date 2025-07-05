
# preparing for Madfest January 2025
# Here's another version that has all movers [lemniscate 2]

# I have notes for a moving pattern, 'lemniscate', where movers follow the lemniscate of Bernoulli trajectory.
# There's an intersting cross-over maneuver described here


## see https://mathworld.wolfram.com/Lemniscate.html

aa <- 12.6  ## 1/2 width, feet; see notes from 12/29;  11 jugglers * 6 ft apart/ 5.244 (lemniscate arc length)

##
aa <- 22.8

tt <- seq( 0, 2*pi, length=22 )
xx <- aa*cos(tt)/( 1 + (sin(tt))^2 )
yy <- aa*sin(tt)*cos(tt)/(1+(sin(tt))^2 )

#plot( xx, yy )  ## those are equ-angle positions...let's do equi-arc-length positions..

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
ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))
# plot
#s_ <- seq(0, ombar, length.out = 100)
#s_ <- seq(0, ombar, length.out = 22)
#lemniscate <- t(p(s_))
#plot(lemniscate, type = "b", col = "yellow", lwd = 5)
#lines(cbind(lemniscate[, 1L], -lemniscate[, 2L]), col="red", lwd = 3)

##
## https://en.wikipedia.org/wiki/Lemniscate_elliptic_functions

library(jacobi)

N <- 22
ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))

aa <- 11*6/(2*ombar)   ### 11 jugglers x 6 ft apart

aa <- 22.8

##
#N <- 15*2
#aa <- 15*6/(2*ombar)  ### 15 jugglers per direction

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
print(kap)

#pdf( file="lB.pdf", width=6, height=4 )

# knock out center point
xx2[c(11,22)] <- NA; yy2[c(11,22)] <- NA
#xx2[c(15,30)] <- NA; yy2[c(15,30)] <- NA

plot( xx2, yy2, xlim= rn.x, ylim=rn.y, asp=1, axes=FALSE, xlab="",ylab="", type="n")
#rn <- range( c(rn.x,rn.y) )
#plot( xx2, yy2, xlim= rn, ylim=rn, asp=1, axes=FALSE, xlab="",ylab="", type="n")

axis(side=1, las=1, cex.axis=.7)
axis(side=2, las=1, cex.axis=.7)
abline(h=0,col="grey")
abline(v=0,col="grey")

## make a curve
tt <- seq( 0, 2*pi, length=100 )
xx3 <- aa*cos(tt)/( 1 + (sin(tt))^2 )
yy3 <- aa*sin(tt)*cos(tt)/(1+(sin(tt))^2 )
lines( xx3, yy3, col="yellow", lwd=20 )

## put in starting points
oddspots.x <- xx2[c(1,3,5,7,9,10,13,15,17,19,21)]
oddspots.y <- yy2[c(1,3,5,7,9,10,13,15,17,19,21)]
# adjust F
eps <- 1/3
m <- length(oddspots.x)
oddspots.x[m] <- oddspots.x[m] + eps
oddspots.y[m] <- oddspots.y[m] - eps
m <- 6
oddspots.x[m] <- oddspots.x[m] + eps
oddspots.y[m] <- oddspots.y[m] + eps


#points( oddspots.x, oddspots.y, pch=22, col="white", cex=1.6 )
#points( oddspots.x, oddspots.y, pch=15, col="blue", cex=1.5 )

## put in 2nd points
espots.x <- xx2[c(2,4,6,8,10,12,14,16,18,20,21)]
espots.y <- yy2[c(2,4,6,8,10,12,14,16,18,20,21)]
m <- 5 
espots.x[m] <- espots.x[m] - eps
espots.y[m] <- espots.y[m] - eps
m <- length(espots.x)
espots.x[m] <- espots.x[m] - eps
espots.y[m] <- espots.y[m] + eps

#points( espots.x, espots.y, pch=21, col="white", cex=1.6 )
#points( espots.x, espots.y, pch=16, col="green", cex=1.5 )


#dev.off()

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

wid <- 1/2
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

for( j in 1:length(ii) )
 { 
 if(ok[j])
  { 

   if( j %% 2 == 0 )
     pp <- c(0,1)
   else
     pp <- c(15,16)

#   lines( c(xxleft[j],xxright[j]), c(yyleft[j], yyright[j]), col="magenta", lwd=2 ) 
   points( c(xxleft[j],xxright[j]), c(yyleft[j], yyright[j]), col="blue", 
		pch= pp, cex=2)
	print(pp)
  } 
 }

