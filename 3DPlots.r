#############  Surface plots #####################
# standard bivariate density
f <- function(x, y){
 return(dnorm(x) * dnorm(y))
#  this also works: return( 1/(2*pi) * exp(-0.5 * (x^2+y^2)) )
}

# Create sequences and evaluate density
y <- x <- seq(from=-3, to=3, length=50)
z <- outer(x, y, f)

# Plot in 3D
persp(x,y, z)

# Customize plot and save perspective matrix
M <- persp(x, y, z, theta = 45, phi=30, expand=0.6, ltheta=120, shade=0.75, ticktype="detailed", xlab="X", ylab="Y", zlab="f(x,y)", col="red", main="Bivariate Normal Density")

# Instead of col="red", try the following fancy colors:
# col=rainbow(30) 
# col=terrain.colors(10)

# Add text 
x.text <- 0
y.text <- 0
z.text <- dnorm(x.text) * dnorm(y.text) * 1.1 # This is 1.1 * f(x,y)
text(trans3d(x.text, y.text, z.text, pmat=M), "f(x, y)")

# Add curve y= - x^2/3 to the plot
x.par <- seq(from=-3, to=3, by=0.1) 
y.par <- -x.par^2/3
z.par <- dnorm(x.par) * dnorm(y.par)  
lines(trans3d(x.par, y.par, z.par, pmat=M), lty=1)                                        
                                                                             
   
# Add points along a circle

sequence <- seq(from=-pi, to=pi, by=pi/16)

x.cir <- cos(sequence)
y.cir <- sin(sequence)
z.cir <- rep(0, length(sequence))  # zeros 
# This circle is too small
points(trans3d(x.cir, y.cir, z.cir, pmat=M))

# Increase circle radius
x.cir <- 2 * cos(sequence) 
y.cir <- 2 * sin(sequence)

# This one is better!
points(trans3d(x.cir, y.cir, z.cir, pmat=M))


dev.off() # Quit plot

# Standard bivariate density ; alternate approach
library(lattice)
# Create sequences
y <- x <- seq(from=-3, to=3, length=50)
# Create 50^2 x 2 matrix of all possible x-y pairs
xy <- expand.grid(x, y)
# Evaluate joint density
z <- dnorm(xy[, 1]) * dnorm(xy[, 2])
# Plot joint density in 3D
wireframe(z ~ xy[, 1] * xy[, 2], xlab="X", ylab="Y", main="Bivariate Normal Density", col="blue")

# Check out this demo of the Rgl package
install.packages("rgl")
library(rgl)
demo(bivar)


#############  Contour plots #####################
library(graphics)
library(lattice)
library(rgl)

# Standard bivariate density
f <- function(x, y){
  return(dnorm(x) * dnorm(y))
  #  this also works: return( 1/(2*pi) * exp(-0.5 * (x^2+y^2)) )
}

# Create sequences and evaluate density
y <- x <- seq(from=-3, to=3, length=50)
z <- outer(x, y, f)

# Contour plot
contour(z)
# Filled contuor plot
filled.contour(z)
# Using terrain colors
filled.contour(z, color.palette = terrain.colors)


# Volcano example 

?volcano  # Get info about dataset

contour(volcano)  # Contour plot

# More filled contour plots

filled.contour(volcano, color.palette = terrain.colors)

image(volcano)
image(volcano, col=terrain.colors(100))

levelplot(volcano)
levelplot(volcano, xlab="", ylab="")

########### 2D - Histograms ################

install.packages("hexbin")
library(hexbin)

mymatrix <- matrix(rnorm(4000), nrow=2000, ncol=2)
hexbin(mymatrix[,1],  mymatrix[, 2])
plot(hexbin(mymatrix[,1],  mymatrix[, 2]))

install.packages("gplots")
library(gplots)

hist2d(mymatrix, nbins=30)
hist2d(mymatrix, nbins=30, col=c("white", rev(terrain.colors(30))))
         