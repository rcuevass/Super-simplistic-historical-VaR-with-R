n <- 10000
# Fixing the seed gives us a consistent set of simulated returns
set.seed(106)
z <- rnorm(n)        # mean = 0 and sd = 1 are defaults
mu <- 0.10
sd <- 0.15
delta_t <- 0.25
# Under the oft-used assumption of Brownian Motion 
# dynamics, the return of a single 
# security (eg, an equity) over a period of 
# time Δt is approximately [See Pelsser for example.]

# return = μΔt + σZ・sqrt(Δt)     

# apply to expression (*) above
qtr_returns <- mu*delta_t + sd*z*sqrt(delta_t) 
hist(qtr_returns, breaks = 100, col = "blue")

stats <- c(mean(qtr_returns) * 4, sd(qtr_returns) * 2)   # sqrt(4)
names(stats) <- c("mean", "volatility")
stats
