Dis1DStabPlot <- function() {
    plot(c(-2, 2), c(0, 0), ylim = c(0, 1),
         type = "n",
         yaxt = "n",
         xlab = "Slope of the linear map", ylab = "")
    abline(v = c(-1, 0, 1))
    polygon(c(-1, 0, 0, -1), c(-2, -2, 2, 2), col = grey(0.8))
    polygon(c(1, 0, 0, 1), c(-2, -2, 2, 2), col = grey(0.6))
    text(-1.5, 0.5, "unstable\n(amplified\noscillations)")
    text(-0.5, 0.5, "stable\n(damped\noscillations)")
    text(0.5, 0.5, "stable")
    text(1.5, 0.5, "unstable")
    axis(3, at = c(-1, 1),
         labels = c(
             "stable\noscillations",
             "neutrally\nstable"))
}

Cont1DStabPlot <- function() {
    plot(c(-2, 2), c(0, 0), ylim = c(0, 1),
         type = "n",
         yaxt = "n",
         xlab = "Slope of the linear\ntime-derivative function", ylab = "")
    polygon(c(-3, 0, 0, -3), c(-2, -2, 2, 2), col = grey(0.8))
    text(-1, 0.5, "stable")
    text(1, 0.5, "unstable")
    axis(3, at = 0,
         labels = "neutrally\nstable")
}

Dis2DStabPlot <- function(x, y, yesx = TRUE, yesy = TRUE) {
  if(missing(x)) {x <- 0; yesx <- FALSE}
  if(missing(y)) {y <- 0; yesy <- FALSE}
  plot(c(-3, -3, 3, 3, x), c(-1, 2, -1, 2, y), type = "n",
       xlab = "trace", ylab = "determinant", las = 1,
       main = paste(
         "Stability of a 2-d discrete",
         "time dynamical affine model",
         "(red line indicates multiple FPs)",
         sep = "\n"))
  xx <- c(seq(-3, -2, length = 50),
          seq(2, 3, length = 50))
  yy <- 0.25*xx^2
  polygon(xx, yy, col = grey(1))
  xx <- seq(-2, 2, length = 100)
  yy <- 0.25*xx^2
  polygon(xx, yy, col = grey(0.7))
  polygon(c(xx, 0), c(yy, -1), col = grey(0.5))
  ## abline(h = 0, lty = 2)
  text(0, 1.60, "unstable FP\n(amplified\noscillations)")
  text(0, 0.60, "stable FP\n(damped\noscillations)")
  text(0, -0.3, "stable\nFP")
  text(-2.5, -0.5, "unstable\nFP")
  text( 2.5, -0.5, "unstable\nFP")
  abline(a = -1, b = 1, col = "red", lwd = 0.5)
  if(yesx & yesy) points(x, y, pch = 16)
}
Cont2DStabPlot <- function(x, y, yesx = TRUE, yesy = TRUE, mainLab = TRUE) {
  if(missing(x)) {x <- 0; yesx <- FALSE}
  if(missing(y)) {y <- 0; yesy <- FALSE}
  if(mainLab) {
      ml <- paste(
         "Stability of a 2-d continuous",
         "time dynamical affine model",
         "(red line indicates multiple FPs)",
         sep = "\n")
  } else {
      ml <- ""
  }
  plot(c(-3, -3, 3, 3, x), c(-1, 2, -1, 2, y), type = "n",
       xlab = "trace", ylab = "determinant", las = 1,
       main = ml)
  xx <- c(seq(-3, 0, length = 100))
  yy <- 0.25*xx^2
  polygon(c(xx, -4, -4), c(yy, 0, 3), col = grey(0.5))
  polygon(c(xx, 0), c(yy, 3), col = grey(0.7))
  polygon(c(-xx, 0), c(yy, 3), col = grey(1))
  text(-2.5, 0.5, "stable\nFP")
  text(-1, 1.2, "stable FP\n(damped\noscillations)")
  text(1, 1.2, "unstable FP\n(amplified\noscillations)")
  text(2.5, 0.5, "unstable\nFP")
  text(0, -0.5, "saddle point\n(one stable and one unstable eigendirection)")
  abline(h = 0)
  if(yesx & yesy) points(x, y, pch = 16)
}


Dis1DStabPlot()
Cont1DStabPlot()
Dis2DStabPlot()
Cont2DStabPlot()



A <- matrix(c(
	1, -2,
	1/2, -1
), 2, 2, byrow = TRUE)

(Delta <- det(A))
(tau <- sum(diag(A)))

Delta <- -0.5
tau <- 0.1
a22 <- 0.1
a21 <- 0.1
A <- matrix(c(
	tau-a22, (a22/a21)*(tau-a22) - (Delta/a21),
	a21, a22
), 2, 2, byrow = TRUE)
A
b <- rep(5, 2)

(V <- eigen(A)$vectors)
(lam <- eigen(A)$values)

## FP
Ident <- diag(1, 2, 2)
(xstar <- try(solve(Ident - A)%*%b))

## stable?
(abs(tau) < Delta + 1) & (abs(Delta) < 1)

## TDS
n <- 100 # number of time steps
X <- matrix(0, n, 2) # two means the system is 2 by 2
X[1,] <- x0 # initial values
for(i in 2:n) X[i, ] <- A%*%X[i-1,] + b


## plot
par(mfrow = c(2, 1))
Dis2DStabPlot(tau, Delta)
plot(rep(1:n,2), as.numeric(X), type = 'n',
     xlab = "time", ylab = "state")
for(i in 1:2) lines(1:n, X[,i], col = i)
if(!inherits(xstar, "try-error")) abline(h = xstar,
                                         lty = 2,
                                         col = 1:2)


