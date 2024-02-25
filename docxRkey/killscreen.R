
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
	xlab="magenta = 6 cnt; blue = 7 cnt; green = 6 cnt/5 cnt; kill screen@37",
		main="Mark's 6-7-6/5 Kill Screen: Hex formation (6 jugglers)", ylab="" )
axis(side=1, at=1:37, cex.axis=1/2, line=-1)
dev.off()

## ideas for a note about these sequences
## might be nice to have the kill screen in the 20's 


## E.g.

## Most passing patterns are designed so that when they go well, they avoid collisions , i.e., when
## performed with precision.  But Mark had the nice idea to have a planned set of collisions
## that would just emerge at some point by the regular passing sequence done precisely.
## Think of a pattern with 4 jugglers in a box, say (A,A') passing with other, and (B,B') passing
## with each other, and with passing lanes that cross.   Usually this is set up with both teams
## on 4-count rights, say alternating  which team passes the right through the middle (and all 
## passes on the odd counts 1 (A,A'), 3 (B,B'), 5 (A,A'), etc.   That's the most basic pattern
## which works as long as the passers are reasonably timed; but even this gets out of wack when
## passers go off time!    Good passers can pass a 1-count with precision, but only if the passes
## are delivered and received at a wide stance; they will surely collide if done well and if following
## the midline.

## Here we're interested in passing sequences that will eventually collide when done well, and done
## in a way to not artificially avoid collisions (e.g. by sending a wide throw).

## Let's have all jugglers start at the same time with two-in-the right
## Patterns that mesh without a kil-screen are things like
## (A,A') on 2-count rights; (B,B') on 2-count lefts
## (A,A') on 3-count, (B,B') on 3/2-count, starting with self
## The double-tree box

## Patterns that have a kill-screen  that's very early include

## (A,A') on 4-count rights; (B,B') on 3-count, starting on beat 2
##
##  beat: 1 2 3 4 5 6 7 8 9
## (A,A') p - - - p 
## (B,B') - p - - p 

## that's boring
## even a bigger delay is boring

##  beat: 1 2 3 4 5 6 7 8 9
## (A,A') p - - - p - - - p
## (B,B') - - p - - p - - p

## We seek interesting patterns for 4, 6, or 8 jugglers that have a kill-screen in the 20's or 30's
## the ks-6-7-6.5 is one for three pairs should work nicely, with a kill-screen at beat 37

## draw box/hex etc 
