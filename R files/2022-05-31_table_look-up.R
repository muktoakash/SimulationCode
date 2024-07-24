## By Marius Hofert

## Demonstration of the basic table look-up method

##' @title Basic Table Look-Up Method
##' @param n sample size to draw
##' @param probs probabilities based on which to draw the 'values'
##' @param values outcomes with the given probabilities 'probs'
##' @return n random variates from 'values' with given probabilities 'probs'
##' @author Marius Hofert
rtable_lookup <- function(n, probs, values = seq_along(probs))
{
    stopifnot(n >= 1, 0 <= probs, probs <= 1, all.equal(sum(probs), 1),
              length(probs) == length(values))
    probs.as.chars <- format(probs, scientific = FALSE) # convert probabilities to characters ("0.<digits>")
    digits <- nchar(probs.as.chars) - 2 # number of digits after decimal point for all probabilities
    M <- 10^max(digits) # 10^{<maximal number of digits of probabilities>}
    T <- rep(values, times = probs * M) # set up basic table
    T[ceiling(length(T) * runif(n))] # randomly index the table
}

## Example call
probs <- dbinom(0:5, size = 5, prob = 0.4) # true B(5, 0.4) probabilities we consider
n <- 1e5
set.seed(271)
x <- rtable_lookup(n, probs = probs, values = 0:5)
tab <- table(x)/n # empirical probabilities computed with the table look-up method
stopifnot(0 <= tab, tab <= 1, sum(tab) == 1,
          all.equal(as.vector(tab), probs, tolerance = 5e-3))
