# Function for generating path of the transformations


createPath <- function( f, x0, nStep){
  vec_x1 <- c()
  vec_x2 <- c()
  while(nStep >= 0){
    vec_x1 <- append(vec_x1, x0[1])
    vec_x2 <- append(vec_x2, x0[2])
    x0 <- f(x0)
    nStep <- nStep - 1
  }
  return (list(x1=vec_x1, x2=vec_x2))
}

# Function that visualizes generated path
par(mar=c(1,1,1,1))

plotPath <- function(path){
  plot.new()
  min_x1 <- min(path$x1)
  min_x2 <- min(path$x2)
  max_x1 <- max(path$x1)
  max_x2 <- max(path$x2)
  k = length(path$x1)
  colors = rainbow(k, start = 0.1, end = 0.9)
  plot.window(c(min_x1 ,max_x1), c(min_x2 ,max_x2))
  for (i in 2:length(path$x1))
  {
    j <- 1
    arrows(path$x1[i-1], path$x2[i-1], path$x1[i], path$x2[i], length = 0.10*(0.999**k), col = colors[i])
    j <- i + 1
    }
}
### EXAMPLES
# Examples of R2 -> R2 transormations

f1 <- function( x){
  c( sin( x[1] + x[2]), cos( x[2]) + sin( x[1]))
}

f2 <- function( x){
  c(
    log( 1/2 + x[1]^2),
    sqrt( abs( x[2] * x[1]))
  )
}

f3 <- function( x){
  c(
    sqrt( 1/2 + x[1]^2) + rnorm( 2, 0, 1/10),
    sin( x[2] + x[1])
  )
}

f4 <- function( x){
  z <- x[1] + x[2] * ( 0 + 1i)
  phi <- Arg( z)
  r <- Mod( z)
  nphi <- phi + pi/36
  if( nphi > pi){
    nphi <- -2 * pi + nphi
  }
  r * rnorm( 1, 1, 1/50)  * c( cos( nphi), sin( nphi))
}

f5 <- function( x){
  c(
    sin( 0.7 + x[1]^ 3 + rnorm( 2, 0, 1/10)),
    cos( x[2] + 10*x[1])
  )
}

# Examples of usage

plotPath( createPath( f1, c( 1, 1), 20))
plotPath( createPath( f2, c( 1, 1), 100))
plotPath( createPath( f3, c( 1, 1), 300))
plotPath( createPath( f4, c( 1, 0), 1000))
plotPath( createPath( f5, c( -1,0), 10))

