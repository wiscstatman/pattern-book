# FEAST: periodic numerical optimization and animation for any n >= 3.
# Install once: install.packages(c('ggplot2','gganimate','gifski'))
library(ggplot2)
library(gganimate)

n <- 8L
adjacent_home_spacing <- 7 # feet; >= 6 for nonoverlapping home discs
personal_radius <- 3       # feet
alpha <- 0.16              # home displacement
beta <- 0.65               # throw length above 6 ft
Gamma <- 16                # strong throw penalty above 12 ft
eta <- 0.65                # step-to-step motion
collision_weight <- 900    # approximate collision constraint

stopifnot(n >= 3, adjacent_home_spacing >= 2 * personal_radius)
theta <- pi/2 - 2*pi*(0:(n-1))/n
home_radius <- adjacent_home_spacing/(2*sin(pi/n))
home <- cbind(home_radius*cos(theta), home_radius*sin(theta))

# Beat t=1 corresponds to t=0 in the mathematical formula.
# partner(i,t)=1+((t-1-i) mod n), for i,t=1,...,n.
partner <- function(i, t) 1L + ((t - 1L - i) %% n)
passes <- lapply(seq_len(n), function(t) {
  i <- seq_len(n)
  j <- partner(i,t)
  cbind(i=i[i<j],j=j[i<j])
})
all_pairs <- t(combn(seq_len(n),2))

# Each row is a beat; each column is one jugglers' coordinate.
# x is stored as an array [beat, juggler, xy].
initial <- array(0, c(n,n,2))
for (t in seq_len(n)) initial[t,,] <- home

energy <- function(z) {
  x <- array(z,dim=c(n,n,2))
  result <- alpha * sum((x-initial)^2)
  # Cyclic first differences, including last beat to first beat.
  next_index <- c(2:n,1)
  result <- result + eta*sum((x[next_index,,,drop=FALSE] - x)^2)
  for (t in seq_len(n)) {
    xt <- cbind(x[t,,1],x[t,,2])
    pp <- passes[[t]]
    if(nrow(pp)) {
      d <- sqrt(rowSums((xt[pp[,1],,drop=FALSE]-xt[pp[,2],,drop=FALSE])^2))
      result <- result + beta*sum(pmax(d-6,0)^2) + Gamma*sum(pmax(d-12,0)^2)
    }
    d_all <- sqrt(rowSums((xt[all_pairs[,1],,drop=FALSE]-
                              xt[all_pairs[,2],,drop=FALSE])^2))
    result <- result + collision_weight*sum(pmax(2*personal_radius-d_all,0)^2)
  }
  result
}

fit <- optim(as.vector(initial), energy, method='L-BFGS-B',
             control=list(maxit=1600, factr=1e6))
x <- array(fit$par,dim=c(n,n,2))

positions <- expand.grid(beat=seq_len(n),juggler=seq_len(n))
# expand.grid makes beat the faster index, matching the array ordering.
positions$x <- as.vector(x[,,1])
positions$y <- as.vector(x[,,2])
positions$juggler <- factor(positions$juggler)

segments <- do.call(rbind,lapply(seq_len(n),function(t) {
  pp <- passes[[t]]
  if(!nrow(pp)) return(NULL)
  data.frame(beat=t,x=x[t,pp[,1],1],y=x[t,pp[,1],2],
             xend=x[t,pp[,2],1],yend=x[t,pp[,2],2],
             pair=seq_len(nrow(pp)))
}))

traces <- positions
traces$beat <- NULL # no beat column => static layer over the whole animation
traces <- traces[order(traces$juggler,positions$beat),]
traces <- do.call(rbind,lapply(split(traces,traces$juggler), function(v)
  rbind(v,v[1,,drop=FALSE])))

homes <- data.frame(x=home[,1],y=home[,2],juggler=factor(seq_len(n)))

p <- ggplot() +
  geom_path(data=traces,aes(x,y,group=juggler,color=juggler),
            linewidth=.7,alpha=.45) +
  geom_point(data=homes,aes(x,y),shape=3,color='grey65',size=3) +
  geom_segment(data=segments,aes(x=x,y=y,xend=xend,yend=yend,group=pair),
               linewidth=.85,color='grey30',alpha=.65) +
  geom_point(data=positions,aes(x,y,color=juggler,group=juggler),size=5) +
  geom_text(data=positions,aes(x,y,label=juggler,group=juggler),
            color='white',size=3,fontface='bold') +
  coord_fixed() +
  scale_color_hue(guide='none') +
  labs(title=paste0(n,'-person feast — passing beat {closest_state}'),
       subtitle='Crosses = home positions; colored loops = optimized traces',
       x='Feet',y='Feet') +
  theme_minimal(base_size=13) +
  transition_states(beat,transition_length=3,state_length=1,wrap=TRUE) +
  ease_aes('cubic-in-out')

animate(p,nframes=n*12,fps=12,width=800,height=800,
        renderer=gifski_renderer(paste0('feast_',n,'.gif')))
write.csv(positions,paste0('feast_',n,'_positions.csv'),row.names=FALSE)

# Post-optimization diagnostics: the collision penalty makes the >= 6 ft
# constraint approximate, NOT exact. Check min separation before use.
min_separation <- min(vapply(seq_len(n),function(t) {
  xt <- cbind(x[t,,1],x[t,,2]);
  min(sqrt(rowSums((xt[all_pairs[,1],,drop=FALSE]-
                    xt[all_pairs[,2],,drop=FALSE])^2)))
},numeric(1)))
cat('Convergence code:',fit$convergence,'\n')
cat('Minimum keyframe separation (ft):',min_separation,'\n')
cat('WARNING: also check separation along interpolated paths.\n')
