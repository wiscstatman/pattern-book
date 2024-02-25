
## February , 24
## code to figure out collisions in Mark's killswitch pattern

N <- 100
z1 <- rep(0,N)
z1[seq(1,N,by=6)] <- 1  ## 6 count, starting first
z2 <- rep(0,N)
z2[seq(2,N,by=7)] <- 1  ## 7 count, starting 2nd

z <- z1+z2 ## number of passes at same time

