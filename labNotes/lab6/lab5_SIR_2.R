### Library you have to include
library("deSolve")

SIRfunc <- function(t, x, parms)
{
	### Initial conditions
	S <- x[1] 
	I <- x[2]
	
	### ODE implementation
	with(as.list(parms),   
{ 
	dS <- mu - beta*S*I - mu*S
	dI <- beta*S*I - (gamma+mu)*I
	
	out <- c(dS, dI) 
	list(out)
})
} 



SIR.simul <- function(horizon, timestep,
					  S.0,I.0,
					  mu,gamma,R0)
{
	### Conviniently name parameters
	parms <- c( mu=mu, gamma=gamma, beta=beta )
	
	### Set initial conditions
	inits <- c( S = S.0, 
				I =I.0  )
	
	### Create the vector of times the ODE will be solved on
	dt <- seq(0,horizon,timestep)
	
	### Solve ODE using 'lsoda'
	### and save result in a 'data frame'
	simulation <- as.data.frame(lsoda(inits, dt, 
									  SIRfunc, 
									  parms=parms))
	
	### Return the data frame of simulated values
	return(simulation)
}


#### TEST ####
if(1)
{
	horizon = 500           #500
	timestep = 10/365      #10/365
	
	S.0 = 0.8
	I.0 = 1E-5  #  = 1/100,000
	
	mu <- 1/60
	gamma <- 365/12
	R0 <- 1.5
	eps <- mu/(gamma+mu)
	beta <- R0*(gamma+mu)
	
	# Run Simulation
	
	simulation <- SIR.simul(horizon, timestep,
							S.0,I.0,
							mu,gamma,R0)
	
	
	# Run Simulation with another set of parameters
	simulation2 <- SIR.simul(horizon, timestep,
							S.0*0.99,I.0,
							mu,gamma,R0)
	
	
	#Endemic equilibrium
	S.ee <- 1/R0
	I.ee <- (1-1/R0)*eps
	
	
	### ===== PLOTS ====
	
	par(mfrow=c(1,2))
	
	plot(x = simulation$S,
		 y = (simulation$I),
		 cex = 0.5,
		 xlab="Susceptibles", ylab="Infected",
		 typ="l")
	points(x=S.ee,y=(I.ee),col="green",pch=16)
	grid()
	
	plot(simulation$time,simulation$I, type="l", col="red", 
		 lwd=5,
		 xlab="Time", ylab="Proportion of infectious",
		 cex=2)
	grid()
	
	if(0)
	{lines(simulation2$time,simulation2$I,
		  col="blue", lwd=5)}
}