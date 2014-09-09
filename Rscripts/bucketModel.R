N <- numeric(20)
N[1] <- 0
for (i in 2:20) {
  N[i] <- N[i-1]-round(0.25*N[i-1])+10
}
N
tvec <- 1:20
plot(tvec,N,type="b",xlab="Time (hours)",ylab="Queue length")

