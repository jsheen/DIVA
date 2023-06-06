# ------------------------------------------------------------------------------
# Model to explore how DIVA helps target culling
# ------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(deSolve)

# Parameters -------------------------------------------------------------------
N = 1e6
gamma = 1 / 5
mu = 1 / 365
beta = 1 / 1000000
p = 0
v = 1 / 30

# Model ------------------------------------------------------------------------
mod_eqn <- function(time, state, parameters){
  with(as.list(c(state, parameters)),{
    dS = -(beta/((S + I + R)^p))*S*I -v*S -mu*S
    dI = (beta/((S + I + R)^p))*S*I -gamma*I -mu*I
    dR = gamma*I -mu*R
    dV = v*S -mu*V
    return(list(c(dS, dI, dR, dV)))})}
parameters <- c(gamma = gamma, beta = beta, mu = mu, p = p)
init <- c(S = N - 1,
          I = 1,
          R = 0,
          V = 0)
times <- seq(1, 730, 1)
out <- ode(y=init, times=times, mod_eqn, parms=parameters)
out.df <- as.data.frame(out)
plot(out.df$time, out.df$V, type='l', ylim=c(0, max(out.df$V, out.df$I, out.df$R, out.df$V + out.df$R)), col='green')
lines(out.df$time, out.df$I, type='l', col='red')
lines(out.df$time, out.df$R, type='l', col='black')
lines(out.df$time, out.df$R + out.df$V, col='blue')









