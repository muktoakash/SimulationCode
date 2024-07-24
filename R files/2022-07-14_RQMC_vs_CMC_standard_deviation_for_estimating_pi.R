## By Marius Hofert

## Studying the variance-reduction effect of using RQMC for estimating pi.


### Setup ######################################################################

library(qrng) # for QRNGs


### 0 Auxiliary function #######################################################

##' @title Estimating Pi with CMC and RQMC
##' @param n vector of sample sizes (for computing the estimates)
##' @param N number of replications of estimating the standard deviation for
##'        each sample size considered
##' @param method character string indicating the method to be used (crude MC
##'        or randomized quasi-MC)
##' @return length(n)-vector containing the estimated standard deviations of
##'         the estimator for each sample size considered.
##' @author Marius Hofert
##' @note We could also return the computed estimators and not only their
##'       standard deviations (but we only use the latter here).
pi_std_dev_simulation <- function(n, N, method = c("CMC", "RQMC"))
{
    ## Basic checks
    stopifnot(n >= 1, N >= 2)
    method <- match.arg(method)

    ## Generate pseudo- or randomized quasi-random numbers
    n.max <- max(n) # generate as many as *maximally* required (recycle otherwise)
    U <- matrix(if(method == "CMC") {
                    runif(n.max * N)
                } else {
                    replicate(N, expr = sobol(n.max, randomize = "digital.shift"))
                    ## Note: One is tempted to do the same as in the pseudo-random
                    ##       case, namely...
                    ##       sobol(n.max * N, randomize = "digital.shift")
                    ##       ... but that would not be correct. We really need
                    ##       N replications of Sobol' sequences of length n.max
                    ##       (which is not equivalent to one Sobol sequence of
                    ##       length n.max * N split up in N parts)
                }, ncol = N) # (n, N)-matrix (each row contains N replications for a specific sample size)

    ## Evaluate the integrand
    X <- 4 * sqrt(1-U^2)

    ## For each sample size and each replication, estimate pi
    pi.hat <- sapply(n, function(n.) colMeans(X[1:n., , drop = FALSE])) # (N, length(n))-matrix
    ## Note: 'drop = FALSE' prevents one-row matrices to be interpreted as
    ##       vectors. Without drop = FALSE and if the smallest sample size
    ##       provided is 1, then this line would fail.

    ## Estimate for each sample size the standard deviation of the estimator based
    ## on the N replications
    apply(pi.hat, 2, sd) # length(n)-vector
}


### 1 Estimated standard deviations against sample size ########################

## Compute standard deviations of the two estimators for various sample sizes
n <- 2^(3:22) # sample sizes (powers will be equidistant in log-scale)
N <- 25 # number of replications considered
set.seed(271) # for reproducibility
system.time(sd.CMC  <- pi_std_dev_simulation(n, N = N)) # ~= 1.7s
system.time(sd.RQMC <- pi_std_dev_simulation(n, N = N, method = "RQMC")) # ~= 1.8s
## Note: One could normally use the same seed for both methods ("common random
##       variates") to obtain more comparable results, but the randomization
##       of Sobol' sequences works differently anyways, so there is no point.

## Plot
plot(n, sd.CMC, type = "l", log = "xy", ylim = range(sd.CMC, sd.RQMC),
     col = "maroon3", xlab = "Sample size n",
     ylab = substitute("Sample standard deviation computed from"~N==N.~"replications",
                       list(N. = N)))
lines(n, sd.RQMC, col = "royalblue3")
legend("bottomleft", bty = "n", lty = c(1, 1), col = c("maroon3", "royalblue3"),
       legend = c("CMC estimator", "RQMC estimator"))


### 2 With regression lines ####################################################

## In log-log scale (log w.r.t. base 10), the lines are roughly linear which
## indicates that the y-value is a power of the x-value since y = x^alpha
## leads to log(y) = alpha * log(x) (in any base), so a linear function. Let's
## estimate the slope alpha of the two lines.
regr.CMC  <- lm(log10(sd.CMC)  ~ log10(n)) # fit a regression line
regr.RQMC <- lm(log10(sd.RQMC) ~ log10(n))
slope.CMC  <- coefficients(regr.CMC)[["log10(n)"]]
slope.RQMC <- coefficients(regr.RQMC)[["log10(n)"]]

## Plot
plot(n, sd.CMC, type = "l", log = "xy", ylim = range(sd.CMC, sd.RQMC),
     col = "maroon3", xlab = "Sample size n",
     ylab = substitute("Sample standard deviation computed from"~N==N.~"replications",
                       list(N. = N)))
lines(n, sd.RQMC, col = "royalblue3")
abline(regr.CMC,  lty = 2)
abline(regr.RQMC, lty = 2)
legend("bottomleft", bty = "n", lty = c(1, 1, 2), col = c("maroon3", "royalblue3", "black"),
       legend = as.expression(c(substitute("CMC estimator (slope"~a*")",
                                           list(a = round(slope.CMC,  2))),
                                substitute("RQMC estimator (slope"~a*")",
                                           list(a = round(slope.RQMC, 2))),
                                "Regression lines")))
## Note:
## 1) The variance of RQMC estimators is known to converge quadratically, so
##    the standard deviation converges to 0 as a function of order 1/n
##    => slope approximately -1.
## 2) For CMC we know by the CLT that the variance is of order 1/n and thus
##    the standard deviation converges to 0 as a function of order 1/sqrt(n)
##    => slope approximately -1/2.
