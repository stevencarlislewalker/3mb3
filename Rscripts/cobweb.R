############################################################
## Draw cobweb diagrams
############################################################
cobweb <- function(expr,start,N=50,
                   scol="black",slty=3,slwd=1,
                   svcol=scol,
                   rcol="gray",rlty=2,
                   add=FALSE,
                   interact=FALSE,
                   ylab=NULL,from,to,
                   xlim,ylim, ...) {
  ## magic taken from curve()
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    fcall <- paste(sexpr, "(x)")
    expr <- parse(text = fcall)
    if (is.null(ylab)) 
      ylab <- fcall
  }
  else {
    if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0L))) 
      stop("'expr' must be a function or an expression containing 'x'")
    expr <- sexpr
    if (is.null(ylab)) 
      ylab <- deparse(sexpr)
  }
  x <- numeric(N)
  x[1] <- start
  for (i in 2:N) {
    x[i] <- eval(expr,envir=list(x=x[i-1]))
  }
  if (!add) {
    if (missing(from)) {
      if (!missing(xlim)) from <- xlim[1]
      else from <- min(x)
    }
    if (missing(to)) {
      if (!missing(xlim)) to <- xlim[2]
      else to <- max(x)
    }
    eval(substitute(curve(expr,from=from,to=to,xlim=xlim,ylim=ylim,...)))
    abline(a=0,b=1,lty=rlty,col=rcol)
  }
  for (i in 2:N) {
    segments(c(x[i-1],x[i-1]),
             c(x[i-1],x[i]),
             c(x[i-1],x[i]),
             c(x[i],x[i]),col=c(svcol,scol),lty=slty,lwd=slwd)
    if (interact) scan(n=1)
  }
  invisible(x)
}


############################################################
## Example
############################################################
r <- 2
b <- r
(x0 <- log(r)/b)
par(mfrow = c(2, 1))
out <- cobweb(r*x*exp(-b*x), start = 0.5, N = 20, ylim = c(0,1), from=0,to=1,
       scol="blue",svcol="red",interact=FALSE)
abline(v = x0)
plot(out, type = 'l')
