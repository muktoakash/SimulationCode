## By Marius Hofert

## Simulating paths from homogeneous Poisson processes


### 0 Auxiliary functions ######################################################

##' @title Simulate Arrival Times of a Homogeneous Poisson Process
##' @param T time horizon T > 0
##' @param lambda intensity lambda > 0
##' @param method character string specifying the sampling algorithm used
##' @return sorted arrival times of one simulated path from a HPP with intensity lambda
##' @author Marius Hofert
HPP <- function(T, lambda, method = c("forward", "backward"))
{
    stopifnot(T > 0, lambda > 0)
    method <- match.arg(method)
    switch(method,
           "forward" = {
               res <- numeric(0) # for arrival times
               T.sum <- 0 # sum of arrival times
               repeat {
                   T.sum <- T.sum + rexp(1, rate = lambda) # update sum of arrival times by adding next Exp(lambda)
                   if(T.sum <= T) res <- c(res, T.sum) else break # add exponential to vector of arrival times or break
                   ## Note: If we knew from the beginning how many to draw, we
                   ##       could simply do cumsum(rexp(<so-many>, rate = lambda))
               }
               res
           },
           "backward" = {
               N.T <- rpois(1, lambda = lambda * T) # number of events until T
               U.sort <- sort(runif(N.T)) # order statistics of N.T-many standard uniforms
               U.sort * T # arrival times
           },
           stop("Wrong 'method'"))
}

##' @title Plotting Paths and Probability Mass Function in a Single Plot
##' @param x p-list containing the arrival times of p simulated paths of the HPP
##' @param T end point for plotting paths
##' @param lambda true lambda used to generate x
##' @param include.normal logical indicating whether to include
##' @return invisible()
##' @author Marius Hofert
##' @note The hist() pmf estimation becomes non-smooth for larger lambda
plot_with_pmf <- function(x, T, lambda, include.normal = FALSE)
{
    ## Basics
    stopifnot(is.list(x))
    p <- length(x) # number of paths

    ## For each sample path, compute the arrival times and corresponding values
    ## of N_t including the two end points t = 0 and t = T
    t <- list(p) # arrival times
    N.t <- list(p) # N_t values
    for(k in 1:p) {
        t[[k]] <- c(0, x[[k]], T) # arrival times including first and last point with no jump (for plotting)
        l <- length(x[[k]])
        N.t[[k]] <- c(0:l, l) # 0 in the beginning and constant at T (for plotting)
    }

    ## Setup layout and plot parameters
    layout(cbind(1, 2), width = c(1, 1/2)) # plot layout (with space to the right)
    opar <- par(mar = c(4.5, 4.2, 1, 0.5)) # reduce space around the main plot, especially to the right

    ## Plot of paths
    yran <- c(0, max(sapply(x, length))) # range of all paths (so that all are visible in the plot(s))
    plot(NA, xlim = c(0, T), ylim = yran, xlab = "Time t",
         ylab = substitute("Poisson process sample paths"~(italic(N)[t])~
                           "with intensity"~lambda==l, list(l = lambda)))
    for(k in 1:p)  # include all sample paths (note: can't easily work with the faster matlines() here)
        lines(t[[k]], N.t[[k]], type = "s",
              col = adjustcolor("black", alpha.f = 1/(1+p/50)))

    ## Plot of the histogram and true pmf of all p-many values at T
    N.T <- sapply(N.t, function(N.) tail(N., n = 1)) # grab out last values = value of N_t at t = T
    hist <- hist(N.T, breaks = -0.5 + yran[1]:(yran[2]+1), plot = FALSE) # pmf estimate based on all paths at t = T
    true <- dpois(yran[1]:yran[2], lambda = lambda * T) # true pmf evaluated at the same points
    xran <- if(include.normal) {
                c(0, max(hist$density, true, dnorm(lambda * T)))
            } else c(0, max(hist$density, true))
    par(mar = c(4.5, 0.5, 1, 4)) # reduce space around the second plot, especially to the left
    plot(hist$density, yran[1]:yran[2], type = "l", yaxt = "n", xlim = xran,
         ylim = yran, # forces the two plots to have aligned y-axes
         xlab = "Pmf at T") # plot pmf estimate (as a piecewise linear function to see more)
    lines(true, yran[1]:yran[2], lty = 2) # plot true pmf (as a piecewise linear function to see more)
    if(include.normal) {
        lines(dnorm(yran[1]:yran[2], mean = lambda * T, sd = sqrt(lambda * T)),
              yran[1]:yran[2], lty = 3)
        legend("topright", bty = "n", lty = c(1, 2, 3), cex = 0.6,
               legend = c("Estimate", "True pmf", expression(N(lambda*T, lambda*T))))
    } else legend("topright", bty = "n", lty = c(1, 2), cex = 0.6,
                  legend = c("Estimate", "True pmf"))
    axis(4) # secondary y-axis
    mtext(substitute("Based on"~p.~"sample paths", list(p. = p)),
          side = 4, line = 3) # secondary y-axis label

    ## Restore original plot parameters and layout
    par(opar) # restore plot parameters
    layout(1) # restore layout
    invisible()
}


### 1 Simulating a homogeneous Poisson process #################################

## Simulation
T <- 2 # time horizon
lambda <- 1 # intensity
n.paths <- 200 # number of paths to be sampled
set.seed(271) # for reproducibility
N.t <- replicate(n.paths, expr = HPP(T, lambda = lambda))
str(N.t)

## Plot paths with estimated and true pmf
plot_with_pmf(N.t, T = T, lambda = lambda)

## With other algorithm
N.t. <- replicate(n.paths, expr = HPP(T, lambda = lambda, method = "backward"))
plot_with_pmf(N.t., T = T, lambda = lambda)

## With larger lambda
lambda <- 10
N.t <- replicate(n.paths, expr = HPP(T, lambda = lambda))
plot_with_pmf(N.t, T = T, lambda = lambda)

## One can show that Poi(lambda * T) ~= N(lambda * T, lambda * T) for large lambda.
plot_with_pmf(N.t, T = T, lambda = lambda, include.normal = TRUE)
