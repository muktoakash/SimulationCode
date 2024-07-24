## By Marius Hofert

## Simulating paths from a Brownian motion, Brownian bridge and geometric
## Brownian motion


### 0 Auxiliary function #######################################################

##' @title Plotting Paths and Densities in a Single Plot
##' @param x (n, p)-matrix containing p simulated paths at discretization points t
##' @param t n-vector of time points t at which the paths are simulated
##' @param true.density true density function used in the right-hand side plot
##' @param ylab y-axis label for the left-hand side plot
##' @return invisible()
##' @author Marius Hofert
##' @note We pass a whole function here for the true model as it can be normal
##'       or log-normal the way we call the function later.
plot_with_densities <- function(x, t, true.density, ylab)
{
    ## Basics
    stopifnot(is.matrix(x), (n <- nrow(x)) == length(t))
    n <- nrow(x) # number of discretization points
    p <- ncol(x) # number of paths

    ## Setup layout and plot parameters
    layout(cbind(1, 2), width = c(1, 1/2)) # plot layout (with space to the right)
    opar <- par(mar = c(4.5, 4.2, 1, 0.5)) # reduce space around the main plot, especially to the right

    ## Plot of paths
    yran <- range(x) # range of all paths (so that all are visible in the plot(s))
    plot(NA, xlim = range(t), ylim = yran, xlab = "Time t", ylab = ylab)
    for(k in 1:p) # include all sample paths
        lines(t, x[,k], col = adjustcolor("black", alpha.f = 1/(1+p/50)))
    ## Note: matlines(t, x, col = adjustcolor("black", alpha.f = 1/(1+p/50)))
    ##       would be much faster but we keep the 'pedagogically meaningful'
    ##       loop here to see how the paths look like while being plotted.

    ## Plot of the density estimate and true density of all p-many values of the last row of x
    dens <- density(x[nrow(x),]) # density estimate of x[<last row>,]
    true <- true.density(dens$x) # true density N(0, T) evaluated at the same points
    par(mar = c(4.5, 0.5, 1, 4)) # reduce space around the second plot, especially to the left
    plot(dens$y, dens$x, type = "l", yaxt = "n", xlim = range(c(dens$y, true)),
         ylim = yran, # forces the two plots to have aligned y-axes
         xlab = "Density at T") # plot density estimate
    lines(true, dens$x, lty = 2) # plot true density
    legend("topright", bty = "n", lty = 1:2, cex = 0.6,
           legend = c("Estimate", "True density"))
    axis(4) # secondary y-axis
    mtext(substitute("Based on"~p.~"sample paths with"~n.~"discretization points",
                     list(p. = p, n. = n)), side = 4, line = 3) # secondary y-axis label

    ## Restore original plot parameters and layout
    par(opar) # restore plot parameters
    layout(1) # restore layout
    invisible()
}


### 1 Simulating a Brownian motion #############################################

## Simulation
T <- 2 # time horizon
n <- 520 # = 2 * 260; number of discretization points
t <- (1:n) * T/n # x-values for the paths
n.paths <- 200 # number of paths to be sampled
s <- sqrt(T/n) # scaling factor for each summand
set.seed(271) # for reproducibility
W.t <- replicate(n.paths, expr = cumsum(s * rnorm(n))) # (n, n.paths)-matrix
str(W.t)

## Plot paths with estimated and true density
plot_with_densities(W.t, t = t, true.density = function(x)
    dnorm(x, mean = 0, sd = sqrt(T - 0)),
    ylab = expression("Brownian motion sample paths"~(italic(W)[t])))


### 2 Simulating a Brownian bridge #############################################

## Simulation (transformation from W.t)
B.t <- apply(W.t, 2, function(w) w - ((1:n)/n) * tail(w, n = 1))

## Plot paths
plot(NA, xlim = range(t), ylim = range(B.t), xlab = "Time t",
     ylab = expression("Brownian bridge sample paths"~(italic(B)[t])))
for(k in 1:n.paths) # include all sample paths
    lines(t, B.t[,k], col = adjustcolor("black", alpha.f = 1/(1+n.paths/50)))
mtext(substitute("Based on"~n.paths.~"sample paths with"~n.~"discretization points",
                 list(n.paths. = n.paths, n. = n)), side = 4, line = 0.5, adj = 0) # secondary y-axis label


### 3 Simulating a geometric Brownian motion ###################################

## Simulation (transformation from W.t)
mu <- 0.1
sig <- 0.2
S.0 <- 1
X.t <- mu * t + sig * W.t # BM with drift
S.t <- S.0 * exp(X.t) # GBM

## Plot paths with estimated and true density
plot_with_densities(S.t, t = t, true.density = function(x)
    dlnorm(x, meanlog = log(S.0) + mu * T, # mean of log(S.t)
           sdlog = sig * sqrt(T - 0)), # standard deviation of log(S.t)
    ylab = expression("Geometric Brownian motion sample paths"~(italic(S)[t])))
