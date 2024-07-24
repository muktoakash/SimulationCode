## By Marius Hofert

## Implementation of a rejection idea for sampling from the discrete uniform
## distribution

library(sfsmisc) # for polyn.eval()


##' @title PRNG from U({1,..,max})
##' @param n number of samples to draw
##' @param max maximum of the support of the discrete uniform distribution
##' @return n-vector containing realizations from U({1,..,max})
##' @author Marius Hofert
rDiscrUnif <- function(n, max)
{
    stopifnot(n >= 1, max %% 1 == 0, max > 0) # input checks (max must be an integer > 0)
    mm1 <- max - 1 # idea: we draw on {0,...,m-1} and then add 1
    nbits.mm1 <- ceiling(log(mm1, base = 2)) # minimal number of bits to represent mm1
    sapply(1:n, function(i) { # construct the ith draw from U({1,...,max})
        repeat {
            ## Generate nbits.mm1-many random bits
            rbits <- runif(nbits.mm1) > 0.5 # nbits.mm1-many U(0,1) here and check for > 0.5 (TRUE/FALSE => 1/0)
            ## Convert the nbits.mm1-many 0s and 1s to a number in base 10
            ## (note: (101)_2 = 1 * 2^0 + 0 * 2^1 + 1 * 2^2 = (5)_10 = 5)
            x.base10 <- polyn.eval(coef = rbits, x = 2) # evaluate a polynomial at 2 => conversion to base 10
            ## If the resulting base-10 number is <= max-1, return it, otherwise reject it
            if(x.base10 <= mm1) return(x.base10 + 1) # if in {0,...,max-1} return 1 more
        }
    })
}

## Run and check
n <- 1e6 # sample size
max <- 7 # for U({1,..,max})
set.seed(271) # for reproducibility
system.time(x <- rDiscrUnif(n, max = max)) # draw random variates; ~= 6s
stopifnot(all.equal(as.numeric(table(x)/n), rep(1/max, max), # check numerical equality
                    check.attributes = FALSE, tol = 0.002))
## This method would be much faster in C than with our pure R implementation here.

## Note: Of course ceiling(max * runif(n)) is mathematically the simplest way
system.time(y <- ceiling(max * runif(n))) # ~= 0.03s
stopifnot(all.equal(as.numeric(table(y)/n), rep(1/max, max),
                    check.attributes = FALSE, tol = 0.002))
