## By Marius Hofert


## Computing the expected payoff of a basket option under dependence


### Setup ######################################################################

library(copula) # for generating dependent data specified via a copula; install via install.packages("copula")


### 0 Auxiliary function #######################################################

##' @title Expected Payoff of a Basket Call Option Via Monte Carlo Integration
##' @param n Monte Carlo sample size
##' @param rho correlation between the d stock prices at maturity T (for
##'        simplicity assumed to be the same for all pairs of stocks)
##' @param nu degrees of freedom parameter of the t copula of (S_{T,1}, ..., S_{T,d})
##' @param S.t d-vector of stock prices at time t (typically t = 0 for 'today')
##' @param w d-vector of weights of the d stocks (w = (w_1, ..., w_d))
##' @param K strike of the option
##' @param r risk-free annual interest rate
##' @param sig constant annual volatilities for all stocks
##' @param t time in years (typically t = 0 for 'today')
##' @param T maturity (in years)
##' @param alpha significance level for the asymptotic 95% CIs
##' @return 2-list containing the Monte Carlo estimate of V_t and asymptotic
##'         (1-alpha)-confidence intervals of the expected payoffd of the basket
##'         call option
##' @author Marius Hofert
##' @note We discuss the construction of the confidence intervals (CIs) later
expected_payoff <- function(n, rho, nu, S.t, w, K, r, sig, t = 0, T = 1, alpha = 0.05)
{
    ## Basic checks
    d <- length(S.t) # number of components in the portfolio
    stopifnot(n >= 1, 0 <= rho, rho <= 1, S.t > 0, w >= 0, d >= 2, length(w) == d,
              K > 0, r >= 0, sig > 0, t >= 0, T > 0, 0 < alpha, alpha < 1)

    ## Construct dependent realizations of S_T (so at maturity T). Each margin
    ## follows a LN as in the Black--Scholes model but the stock prices are
    ## dependent according to a t copula.
    cop <- tCopula(rho, dim = d, df = nu) # define the t_nu copula
    U.T <- rCopula(n, cop = cop) # sample U from the t_nu copula
    S.T <- sapply(1:d, function(j) qlnorm(U.T[,j], # apply marginal quantile transformations
                                          meanlog = log(S.t[j]) + (r-sig[j]^2/2) * (T-t),
                                          sdlog = sig[j]*sqrt(T-t))) # see Black--Scholes model
    ## Note: S.T is an (n, d)-matrix

    ## Compute the realizations of the integrand of V_t
    integrand <- exp(-r*(T-t)) * pmax(0, (S.T %*% w) - K) # n-vector
    ## Note: We need to use pmax() instead of max() here so that we get the
    ##       maximum for *each* of the n replications.

    ## Compute the MC estimator of V_t
    mu.hat <- mean(integrand) # estimated the expected payoff V.t

    ## Compute asymptotic confidence intervals (based on CLT; see later for the 'how')
    z <- qnorm(1-alpha/2) # (1-alpha/2)-quantile of N(0,1)
    SE <- sd(integrand) / sqrt(n) # standard error
    CI <- c(low = mu.hat - z * SE, up = mu.hat + z * SE) # lower and upper confidence interval bound

    ## Return results
    list(mu.hat = mu.hat, CI = CI)
}


### 1 Specify model parameters #################################################

## Setup
rho <- seq(0, 1, length.out = 51) # homogeneous correlation we let vary
nu <- 3.5 # degrees of freedom of the t copula
d <- 100 # number of components in the portfolio
S.t <- rep(10, d) # current stock prices S_{t,j}
w <- rep(1, d) # weights (here: equal)
K <- 12 * d # strike
r <- 0.01 # risk-free annual interest rate
sig <- rep(0.2, d) # (constant) annual volatilities of all stocks
t <- 0 # time today
T <- 0.25 # maturity of a quarter year


### 2 Estimate the expected payoff as a function of the correlation parameter ##

## Computing the result object
n <- 1e4 # MC sample size
set.seed(271) # for reproducibility
pb <- txtProgressBar(max = length(rho), style = 3) # setup progress bar
res <- lapply(1:length(rho), # iterate over all rho's
              function(k) {
                  setTxtProgressBar(pb, k) # update progress bar
                  expected_payoff(n, rho = rho[k], nu = nu, S.t = S.t, w = w,
                                  K = K, r = r, sig = sig, t = t, T = T)
              })

## Grab out the results we use
V.t <- sapply(res, function(x) x[["mu.hat"]]) # extract estimated payoffs
V.t.CI.low <- sapply(res, function(x) x[["CI"]][["low"]]) # lower CI
V.t.CI.up  <- sapply(res, function(x) x[["CI"]][["up"]])  # upper CI


### 3 Plot the expected payoff as a function of the correlation parameter ######

## Plot
plot(rho, V.t, type = "l", ylim = range(V.t.CI.low, V.t.CI.up),
     xlab = expression("Correlation"~rho~"between all stocks"),
     ylab = substitute("Expected payoff"~V[t]~"of the basket call option ("*
                           italic(t)[nu.]*" copula, log-normal margins)",
                       list(nu. = nu)))
lines(rho, V.t.CI.low, lty = 2)
lines(rho, V.t.CI.up,  lty = 2)
legend("bottomright", bty = "n", lty = c(1, 2),
       legend = c("MC estimate", "95% asymptotic CIs"))
mtext(substitute("MC sample size"~n==n.*", time today"~t==t.*", maturity"~T==T.,
                 list(n. = n, t. = t, T. = T)), side = 4, line = 0.5, adj = 0)
