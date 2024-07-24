## By Marius Hofert

## Studying the variance-reduction effect of using RQMC for estimating
## the exceedance probability mu = P(X_1 > x_1, ..., X_d > x_d) for independent
## X_j ~ N(0,1).


### Setup ######################################################################

library(qrng) # for QRNGs
n <- 1000 # sample size considered throughout


### 0 Auxiliary functions ######################################################

##' @title Side-by-side Plot
##' @param ... 2-column matrices of data to be plotted
##' @param base.label base label used for both axes
##' @param cex character expansion
##' @return invisible()
##' @author Marius Hofert
plot_matrix <- function(..., base.label, cex = 0.5)
{
    ## Basic checks
    args <- list(...)
    nplots <- length(args) # number of plots
    stopifnot(nplots >= 1, sapply(args, function(x) is.matrix(x) && (ncol(x) == 2)),
              is.character(base.label), 0 <= cex, cex <= 0.5)

    ## Setup square-like plot layout (filled with plots in row-major order, so 'by row')
    layout(matrix(1:nplots, nrow = 1, ncol = nplots, byrow = TRUE))

    ## Plots
    opar <- par(pty = "s", mar = par("mar") - c(1, 1, 3, 1)) # square plots and reduced margins
    for(i in 1:nplots)
        plot(args[[i]], cex = cex,
             xlab = substitute(italic(l)[1], list(l = base.label)),
             ylab = substitute(italic(l)[2], list(l = base.label)))
    par(opar) # reset plot parameters

    ## Reset layout
    layout(1)
}

##' @title Estimating mu = P(X_1 > x_1,..., X_d > x_d) for independent X_j ~ N(0,1)
##' @param n sample size
##' @param x evaluation point x = (x_1,...,x_d)
##' @return 2-list containing the estimated mu and corresponding run time for
##'         each of the two methods (MC, RQMC).
##' @author Marius Hofert
indep_N01_surv_prob <- function(n, x)
{
    d <- length(x)
    stopifnot(n >= 1, d >= 1)

    ## Pseudo-random sampling
    clock <- proc.time() # start stop watch
    X <- matrix(rnorm(n * d), ncol = d)
    prob <- mean(rowSums(X > rep(x, each = n)) == d) # = sum(<rows with all entries TRUE>) / n
    rt <- 1000 * (proc.time() - clock)[["elapsed"]] # run time in ms

    ## Quasi-random sampling
    clock <- proc.time()
    X. <- qnorm(sobol(n, d = d, randomize = "digital.shift"))
    prob. <- mean(rowSums(X. > rep(x, each = n)) == d)
    rt. <- 1000 * (proc.time() - clock)[["elapsed"]]

    ## Return
    list(MC   = c(prob = prob,  time = rt), # (pseudo-random) Monte Carlo results
         RQMC = c(prob = prob., time = rt.)) # randomized quasi-Monte Carlo results
}


### 1 Visual comparison ########################################################

## U(0,1)^2 case

## Sample
seed <- 271
set.seed(seed) # for reproducibility
U <- matrix(runif(n * 2), ncol = 2) # pseudo-random numbers
U.none <- sobol(n, d = 2, seed = seed) # non-randomized Sobol' sequence
U.digi <- sobol(n, d = 2, seed = seed, randomize = "digital.shift") # a fast randomization

## Plot
plot_matrix(U, U.none, U.digi, base.label = "U") # between the 2nd and 3rd plot not a drastic visible difference here

## Remark: Generating N samples of size n from a randomized quasi-random number
##         generator and computing an estimator based on each sample will lead
##         to N estimates (realizations of the estimator) which, as a sample
##         of size N, has a smaller variance than when this exercise is repeated
##         for a PRNG. This especially applies to estimating tail probabilities,
##         for example.


## Independent N(0,1)

## Map to N(0,1) margins
X <- qnorm(U)
X.none <- qnorm(U.none)
X.digi <- qnorm(U.digi)

## Plot
plot_matrix(X, X.none, X.digi, base.label = "X")


### 2 Simulation study to assess the variance reduction effect #################

## Setup
N <- 1000 # number of replications of estimates
n <- 5000 # sample size for each estimate
d <- 10 # dimension
x. <- 0 # evaluation point (x,..,x) of P(X_1 > x,.., X_d > x) (here: same in each dimension)
(mu <- pnorm(-x.)^d) # true mu = P(X_1 > x,.., X_d > x) = P(X > x)^d = (1-Phi(x))^d = Phi(-x)^d
## Note: 1) mu ~= 0.0009765625 (rare event) => only 0.0009765625 * 5000 ~= 5
##          observations are expected to *jointly* be positive.
##       2) For larger d, mu will be even smaller and thus our basic tail estimator
##          becomes unreliable (either way); in other words, n needs to grow with
##          d for this simulation to be meaningful.

## Simulation
set.seed(271) # for reproducibility
system.time(raw <- lapply(1:N, function(N.) indep_N01_surv_prob(n, x = rep(x., d))))

## Convert simulation object into an array
res <- array(unlist(raw), dim = c(2, 2, N), # (*): (<type of result>, <method>, <replication>)
             dimnames = list("type" = c("prob", "time"),
                             "method" = c("MC", "RQMC"),
                             "Replication" = 1:N))
## Note for (*): 'raw' is a nested list, nested according to the replication
##               (outer nesting), the estimation method and the type of result
##               (innermost nesting). The unlist() unlists 'raw' in reverse order
##               and fills the array, this is how we know which dimension
##               in the array contains which computed result (first dimension
##               contains the innermost results, last dimension the outermost).
##               The same applies if we had returned a (2, 2)-matrix in
##               indep_N01_surv_prob() (in which case the *rows* would have been
##               considered first, and columns second dimension etc. as usual).

## Build vectors (for convenience)
prob.MC   <- res["prob", "MC",]
prob.RQMC <- res["prob", "RQMC",]
time.MC   <- res["time", "MC",]
time.RQMC <- res["time", "RQMC",]


### 3 Variance reduction effect ################################################

## Boxplot of the N estimated probabilities
boxplot(list(MC = prob.MC, RQMC = prob.RQMC),
        main = "Simulated exceedance probability")
mtext(text = substitute(N==N.*","~n==n.*","~d==d.*","~mu==mu.,
                        list(N. = N, n. = n, d. = d, mu. = mu)),
      side = 4, line = 1, adj = 0)
## Remark
## - We see that the estimated exceedance probabilities vary less for the RQMC
##   method, so the RQMC estimator has a smaller variance.
## - Again note that both of these results become unreliable for larger d
##   if n is kept fixed.

## Variance reduction in numbers
(VRF <- var(prob.MC) / var(prob.RQMC)) # estimated VRF
(var(prob.MC)-var(prob.RQMC))/var(prob.MC) * 100 # % improvement


### 4 Run time consideration ###################################################

## Boxplot of run times in simulation
boxplot(list(MC = time.MC, RQMC = time.RQMC), outline = FALSE,
        main = "Elapsed time in milliseconds")
mtext(text = substitute(N==N.*","~n==n.*","~d==d.*","~mu==mu.,
                        list(N. = N, n. = n, d. = d, mu. = mu)),
      side = 4, line = 1, adj = 0)
## Note that every once in a while, larger outliers can appear, we omit
## them here (outline = FALSE) in order to better see what happens in the typical
## case.

## Run-time reduction factor
(RRF <- mean(time.MC) / mean(time.RQMC)) # < 1 MC is faster, > 1 RQMC is faster
## < 1 => MC is faster (this used to be different, but rnorm() is maintained
## and has improved)


### 5 Variance per unit of time consideration ##################################

## To be fair need to take run time into consideration. Because if a method
## takes twice as long for the same sample size, the other method could
## effectively use a larger sample size and thus obtain a reduced variance.

## A fairer factor to consider is VRF * RRF
VRF * RRF # < 1 MC is better, > 1 RQMC is better
