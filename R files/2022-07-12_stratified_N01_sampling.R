## By Marius Hofert

## Stratified sampling for N(0,1) data


### Setup ######################################################################

n <- 5000 # sample size
set.seed(271) # for reproducibility
U <- runif(n) # set of U(0,1)
X <- qnorm(U) # corresponding N(0,1) sample


### 0 Auxiliary function #######################################################

##' @title A Nicer Histogram Plot
##' @param x data
##' @param text text for 4th axis
##' @param ... additional arguments passed to the underlying hist() (unused here)
##' @return invisible()
##' @author Marius Hofert
myhist <- function(x, text = NULL, ...) {
    hist(x, breaks = 25, probability = TRUE, xlab = "Data", main = "", ...)
    if(!is.null(text))
        mtext(text = text, side = 4, line = 1, adj = 0)
    box()
}


### 1 Comparing non-stratified and proportionally stratified samples ###########

## Stratify U proportionally (n_k = n * p_k, k = 1,...,K)
K <- 100 # number of strata; note: if we choose n here, each sample lies in its own stratum
n.k <- n/K # number of samples per strata according to 'proportional allocation' (equal for all strata)
stopifnot(n.k %% 1 == 0, K * n.k == n) # sanity checks
U.SS <- unlist(lapply(1:K, function(k) ((U[n.k * (k-1) + 1:n.k] + k - 1) / K))) # for each k, draw n.k samples from stratum k
X.SS <- qnorm(U.SS) # map to stratified N(0,1) sample via inversion method

## Compare histograms to see the effect of (here: proportional allocation) stratification
myhist(U,    text = substitute("Random U(0,1) sample of size"~n.,     list(n. = n)))
myhist(U.SS, text = substitute("Stratified U(0,1) sample of size"~n., list(n. = n)))
myhist(X,    text = substitute("Random N(0,1) sample of size"~n.,     list(n. = n)))
myhist(X.SS, text = substitute("Stratified N(0,1) sample of size"~n., list(n. = n)))


### 2 (Optimal allocation) stratification for N(0,1) ###########################

## Build strata and compute their probabilities
strata <- c(-Inf, seq(-3, 3, by = 0.25), Inf) # interesting: for the range (-2, 2), the result doesn't look that good
K <- length(strata)-1 # number of strata
p <- diff(pnorm(strata)) # probabilities of falling in the strata
plot(1:K, p, type = "b", xlab = "Stratum k",
     ylab = "Probability of falling in stratum k")
## => To see the shape of the density makes sense as the density around a small
##    interval (here: a stratum) indicates the probability of falling in this interval.

## Pilot study for simulating the standard deviations per stratum
rnorm_strata <- function(n, a, b) # sample N(0,1) conditional on (a,b] (via inversion)
    qnorm(pnorm(a) + (pnorm(b) - pnorm(a)) * runif(n)) # see slides for the formula
N <- 1000 # pilot study sample size to compute standard deviations per stratum
SD <- sapply(1:K, function(k) sd(rnorm_strata(N, a = strata[k], b = strata[k+1]))) # strata (sample) standard deviations
plot(1:K, SD, type = "b", xlab = "Stratum", ylab = "Sample standard deviation")
## => sd is larger in the extreme strata as they are unbounded

## Determine the optimal strata sample sizes
n.k <- sapply(1:K, function(k) round(n * p[k] * SD[k] / sum(p * SD))) # optimal strata sample sizes
sum(n.k) # should be close to n
plot(1:K, n.k, type = "b", xlab = "Stratum", ylab = "Stratum sample size") # plot optimal sample sizes per stratum
## => This makes sense as the formula implies that the larger the larger the
##    probability of falling in a stratum or the larger its standard deviation,
##    the larger the stratum sample size must be (and that's what we see).

## Now sample from each stratum (by recycling the above pseudo-random U's)
stopifnot(sum(n.k) <= n) # as we want to recycle the above U's
n.k.cum <- cumsum(c(0, n.k)) # add up strata sample sizes (with dummy 0 in the beginning)
X.SS. <- unlist(lapply(1:K, function(k) {
    U. <- U[(n.k.cum[k] + 1) : n.k.cum[k+1]] # map the (recycled) U's to the conditional distribution in stratum k
    qnorm( pnorm(strata[k]) + p[k] * U. ) # inversion method for stratum k
    ## Note: F("left endpoint of the stratum") + "stratum probability" * "uniform in that stratum"
    ##       is uniformly distributed in stratum k (and then mapped to N(0,1) by inversion)
}))

## Plot
myhist(X.SS., text = substitute("Optimally stratified N(0,1) sample of size"~n.~"with"~K.~"strata",
                                list(n. = n, K. = K)))

## Comparison with non-stratified sample
myhist(X, text = substitute("Random N(0,1) sample of size"~n., list(n. = n)))
