
## February , 24
## code to figure out collisions in Mark's killswitch pattern

N <- 37
z1 <- rep(0,N)
z1[seq(1,N,by=6)] <- 1  ## 6 count, starting first
z2 <- rep(0,N)
z2[seq(2,N,by=7)] <- 1  ## 7 count, starting 2nd

z <- z1+z2 ## number of passes at same time

print( cbind(z1,z2) )

x <- (1:N)[z1==0 & z2==0]

## it looks like a 4-cnt/5-cnt alternating pair starting on beat 3 might work

z3 <- c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1)
cbind(z1,z2,z3)

cls <- rep(NA,N)
cls[z1==1] <- "magenta"
cls[z2==1] <- "blue"
cls[z3==1] <- "green"
cls[z1==1&z2==1&z3==1] <- "red"

pdf( file="ks-6-7-6.5.pdf", width=9, height=3 )
plot( 1:N, pmax(z1,z2,z3), type="h", axes=FALSE, lwd=2, col=cls , 
	xlab="magenta = 6 cnt; blue = 7 cnt; green = 6 cnt/5 cnt; kill switch@37",
		main="Mark's 6-7-6/5 Kill Switch: Hex formation (6 jugglers)", ylab="" )
axis(side=1, at=1:37, cex.axis=1/2, line=-1)
dev.off()
