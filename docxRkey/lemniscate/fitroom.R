
# 
# How does the lemniscate fit into Ballroom B at IJA Evansville? 

# I have notes for a moving pattern, 'lemniscate', where movers follow the lemniscate of Bernoulli trajectory.
# There's an intersting cross-over maneuver described here


## see https://mathworld.wolfram.com/Lemniscate.html


## N= 22 movers; aa<-21
## N= 26 movers; aa<-24.8
aa <- 21  ## ok for plot; really a=(1/2) width = (N/2)*10'/5.244  , assuming 10' passing PBJ; ## a=21.0 would be better
							## and we'll use this in the spacing  ; depth=a/sqrt(2)
							## N/2 = 11...a=21.0 width= 42'         14.8
                                                        ## N/2 = 13   a=24.8        49.6        17.6
                                                        ## N/2 = 15   a=28.6        57.2        20.2
                                                        ## N/2 = 17   a=32.4        64.8        22.9
aa <- 21
N <- 22

#aa <- 24.8; N <- 26 ## too big

## ballroom B in Evansville is 48' x 35', evidently, so that can only hold N/2=11; i.e. N=22 movers + 4 feeders
## maybe on diagonal we can get N=26 movers?

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
ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))

library(jacobi)

ombar <- 2.622 # gamma(1/4)^2 / (2 * sqrt(2*pi))



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



plot( x0, y0, type="l", col="grey",  axes=FALSE , xlab="", ylab="", xlim=xl, ylim=yl, asp=1 )
lines( xxleft, yyleft, col="red", lwd=2 )
lines( xxright, yyright, col="blue", lwd=2 )


theta <- -atan( 35/48 )  ## angle of room diagonal

p0 <- cbind(x0,y0)
pp <- p0 %*% rbind(  c( cos(theta), -sin(theta) ), c( sin(theta), cos(theta) ) )
plot( pp, type="l", asp=1, xlim=c(-24,24), ylim=c(-35/2, 35/2),  xlab="", ylab="" )

# room  walls
 abline( v=-24)
 abline(v=24)
 abline(h=17.5)
 abline(h=-17.5)
 
# compare to parallel set up
lines( x0, y0, col="red" )



