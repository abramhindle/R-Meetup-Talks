# R Plotting Demo Talk
# By abram hindle (abram.hindle@softwareprocess.es)
#    Department of Computing Science
#    University of Alberta
# FOR the Edmonton R User's Group!

# Code is CC0 no rights reserved 

# This talk is a live demo without slides

# For this demo you might to run these commands
# install.package("MASS")
# install.package("hexbin")
# install.package("gplots")
# install.package("ggplot2")
# install.package("corrplot")
X11.options(type="nbcairo")
# remedial:
avector <- c(1:100) # make a vector of 1 to 100
afunction <- function(x) { x * x } # make a function that squares
anewvector <- sapply(avector, afunction) # apply afunction to avector
# this line ^^^^ is equal to these lines
# anewvector <- c(1:100)
# for (i in 1:100) {
#     anewvector[i] <- afunction(avector[i])
# }
plot(avector,anewvector) # plot it

alistfunction <- function(x) { c(1:x) }
# return a list
anothervector <- sapply(avector, alistfunction) # apply afunction to avector
plot(anothervector[[50]]) # plot the 50th element (a list)

avector[50] # 50th element
anewvector[[50]] # 50th element in a list (often another list)
sapply(anothervector, max) # for all lists get the max value # returns a vector
lapply(anothervector, max) # for all lists get the max value # returns a list



require(MASS)

# Use case: Your data consists of points!
# So many points that they tend to overlap!
                                        
n <- 10000
v <- c()
# measurements of 10000 milliseconds
v$x <- trunc(runif(n)*n)
v$y <- c(rnorm(n/4), rnorm(3*n/4,mean=8))
v$x <- c(v$x, 1:n)
v$y <- c(v$y, (4 + sin(1:n/100) + rnorm(n)))
plot(v$x,v$y)

# example with duplicate measures
# Or grid data
u <- c()
u$x <- sample(c(1:n),n)
u$y <- c((1:(3*n/4) %% 20),(5 + (1:(n/4) %% 8)))
plot(u)

# In both cases there is too much overlap!

# But what does the data actually look like?
# What regions are we measuring?

# Density Plots
# kde2d does density plots of points
ncol <- 50 # number of colors
# topological colours, like islands
colors <- topo.colors(ncol) 
# n means 500x500 grid
d <- kde2d(v$x, v$y, n = c(500,500))
image(d,col=colors)
# add some contours to make the highpoints noticable
contour(d, add=T, nlevels=ncol/5)
# Do you see where the sinusoid is now?
plot(v)
# Ok let's see that on the grid data
d <- kde2d(u$x, u$y, n = c(500,500))
image(d,col=colors)
contour(d, add=T, nlevels=ncol/5)

contour(d, nlevels=ncol/5)
# See the overlap between the 2 datasets?

# But that's too smooth and touchy feely. 
library(hexbin)
# Hexbin can help us make a 2D Histogram with a nice legend, plus it avoid Grids
plot(hexbin(v$x,v$y))
# the sinusoid comes out!
plot(hexbin(u$x,u$y))
# the overlap is far more apparent

# GGplot can make it look nicer
library(ggplot2)
ggplot(data.frame(v), aes(v$x,v$y)) + # aes maps
  geom_hex() + # use hex
scale_fill_continuous(low = "blue", high = "red") # choose a colour range
# we can use bins
ggplot(data.frame(v), aes(v$x,v$y)) + 
  geom_bin2d()
# now for the integers
ggplot(data.frame(u), aes(u$x,u$y)) + 
  geom_bin2d()
ggplot(data.frame(u), aes(u$x,u$y)) + 
  geom_hex()

library(gplots)
hist2d(v$x,v$y)
hist2d(u$x,u$y)

# Use case: You have multiple runs and you collect statistics on each run

# the data
nwalks <- 30
walk <- c(1:nwalks)
walk[1] <- 0
for (x in 2:nwalks)  {
  walk[x] = walk[x-1] + runif(1) - 0.5
}
walks <- lapply(walk, function(x) { rnorm(1000, mean=x,sd=abs(runif(1))) })

# right let's look at it
plot(sapply(walks,mean),col="red")
lines(sapply(walks,median),col="green")
lines(sapply(walks,mean),col="red")
# lets see how big they can be
lines(sapply(walks,max))
# or small
lines(sapply(walks,min))


# oh wait that doesn't look right..
minwalk <- min(sapply(walks,min))
maxwalk <- max(sapply(walks,max))
# ylim lets us scale the plot better
plot(sapply(walks,mean),col="red")
plot(sapply(walks,mean),col="red",ylim=c(minwalk,maxwalk))
lines(sapply(walks,median),col="green")
lines(sapply(walks,mean),col="red")
lines(sapply(walks,max))
lines(sapply(walks,min))

# we can run a boxplot on it
boxplot(walks)

# we can also add lines to the boxplot
lines(sapply(walks,median),col="green")
lines(sapply(walks,mean),col="red")
lines(sapply(walks,max))
lines(sapply(walks,min))

# ok that's neat enough but can we color that region between min and max?
# 1st make a plot with nothing on it
plot(c(),xlim=c(1,length(walks)),ylim=c(minwalk,maxwalk),xaxt="n",ylab="Y Values",xlab="X Version",main="A title")
# xlim is

# from the R help(par) page:
#     ‘xaxt’ A character which specifies the x axis type.  Specifying
#          ‘"n"’ suppresses plotting of the axis.  The standard value is
#          ‘"s"’: for compatibility with S values ‘"l"’ and ‘"t"’ are
#          accepted but are equivalent to ‘"s"’: any value other than
#          ‘"n"’ implies plotting.
# polygon(c(1,5,10,5,1),c(1,10,1,1,1),col="red")

w <- c()
w$x <- c(1:length(walks))
w$x <- c(w$x,rev(w$x)) # from left to right, turn around right to left
w$y <- c(sapply(walks,max),rev(sapply(walks,min)))
# lty=2 dashed
polygon(w$x,w$y,col="lightgrey",border="grey",lty=2)
# ok now we add a boxplot..
boxplot(walks)
# ok what happened?
help(boxplot)
# right we have to tell it to ADD
plot(c(),xlim=c(1,length(walks)),ylim=c(minwalk,maxwalk),xaxt="n",ylab="Y Values",xlab="X Version",main="A title")
polygon(w$x,w$y,col="lightgrey",border="grey",lty=2)
boxplot(walks,add=1)
# uhhhh something doesn't look right
boxplot(walks,add=1,col="white")

# With polygon you can make some nice plots
plot(walk)
polygon(1:length(walk),walk,col="lightgrey",border="grey",lty=2)
# that doesn't look right does it.
plot(walk)
polygon(c(1:length(walk),rev(1:length(walk))),c(walk,0*(1:length(walk))),col="lightgrey",border="grey",lty=2)
points(walk)

#constant
below <- 1
# what about labels?
plot(walk,xaxt="n")
# ok no x axis label
axis(below,c(1:5,10,20,30),c("One","Two","Tree","Four","Five","Ten","Twenty","Thirty"))


library(corrplot)
# flatten our walks and show a correlation
corrplot(cor(sapply(walks,function(x){x}),method="spearman"))
image(cor(sapply(walks,function(x){x}),method="spearman"))
demo <- c()
demo$r <- rnorm(100,sd=0.001)
demo$s <- rnorm(100,sd=0.001)
demo$q <- runif(100)
demo$v <- runif(100)
corrplot(cor(data.frame(demo)))
