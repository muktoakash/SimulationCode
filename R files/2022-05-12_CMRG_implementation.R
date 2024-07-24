## By Marius Hofert

## Implementation of L'Ecuyer's Combined Multiple Recursive Generator (CMRG)
## in R and comparison with R's implementation.


##' @title L'Ecuyer's Combined Multiple-Recursive Generator
##' @param n sample size
##' @param seed seed
##' @return numeric vector of generated random numbers
##' @author Marius Hofert
##' @note Not optimized for efficiency
CMRG <- function(n, seed)
{
    ## Input checks
    stopifnot(n >= 1, length(seed) == 6, seed >= 0,
              any(seed[1:3] > 0), any(seed[4:6] > 0))

    ## Initialization for the recursion
    y <- x <- integer(n+3)
    x[1:3] <- seed[1:3]
    y[1:3] <- seed[4:6]
    m1 <- 4294967087 # = 2^32 - 209
    m2 <- 4294944443 # = 2^32 - 22853

    ## Recursion
    for(i in 1:n) {
        x[i+3] <- (1403580 * x[i+1] -  810728 * x[i]) %% m1
        y[i+3] <- ( 527612 * y[i+2] - 1370589 * y[i]) %% m2
    }
    z <- (x - y) %% m1

    ## Standardize the result and return
    res <- z / (m1 + 1)
    res[z == 0] <- m1 / (m1 + 1)
    tail(res, n = n) # omit the seed
}

## Our own implementation for seed 1:6
seed <- 1:6 # also checked sample(0:1e6, size = 6) for some runs
n <- 1e5
U <- CMRG(n, seed = seed) # in particular, .Random.seed exists after this call

## Switch R's internal PRNG to L'Ecuyer's CMRG
RNGkind("L'Ecuyer-CMRG")

## R's implementation for seed 1:6
.Random.seed[2:7] <- seed
U. <- runif(n) # generate random numbers from R's implementation

## Compare
stopifnot(all.equal(U, U.)) # => fine!

## Run-time comparison (same seed not really necessary)
n. <- 1e7 # 10M
(rt.mine <- system.time(CMRG(n., seed = seed))) # our implementation
.Random.seed[2:7] <- seed
(rt.theirs <- system.time(runif(n.))) # R's implementation
rt.mine[["elapsed"]] / rt.theirs[["elapsed"]]
## => We are about a factor of 40 slower (not surprising, we use a pure
##    R implementation); used to be around 20 two years ago, so R got
##    even faster.

## Reset PRNG
RNGkind("Mersenne-Twister")

## Technical note: R-3.6.2's implementation can be found in:
## ./src/main/RNG.c:146 (main algorithm),
## ./src/main/RNG.c:244 (seed validity check) and
## ./src/main/RNG.c:296 (initialization)
