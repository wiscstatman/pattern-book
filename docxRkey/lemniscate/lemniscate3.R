
# After madfest 26...writing up the description

## see https://mathworld.wolfram.com/Lemniscate.html

aa <- 12.6  ## 1/2 width, feet; see notes from 12/29;  11 jugglers * 6 ft apart/ 5.244 (lemniscate arc length)

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

N <- 38
ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))

aa <- 19*5/(ombar)   ### 19 jugglers x 5 ft apart

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

ww <- 8
hh <- ww*rn.y[2]/rn.x[2]
plot( xx2, yy2, xlim= rn.x, ylim=rn.y,  axes=FALSE, xlab="",ylab="" )
lines(xx2,yy2)
lines(xx2[c(1,38)], yy2[c(1,38)] )
points( xx2, yy2, pch=19, col="grey", cex=.9 )
points( xx2[38], yy2[38], col="red", cex=.9, pch=19 )

#axis(side=1, las=1, cex.axis=.7)
#axis(side=2, las=1, cex.axis=.7)
abline(h=0,col="grey")
abline(v=0,col="grey")
