## By Marius Hofert

## Understanding confidence intervals: We simulate B samples from
## an Exp(lambda) distribution, compute (for each sample) the corresponding (1-alpha)-
## confidence interval (CI) for the mean mu = 1/lambda and empirically check
## how many of the CIs contain (the true mean) mu. This should roughly be (1-alpha)*100%
## many.


##' @title Asymptotic (1-alpha)-Confidence Interval
##' @param x n-vector containing a sample of size n
##' @param alpha the significance level
##' @return 2-vector containing the lower (first component) and upper (second
##'         component) endpoint of the computed CI.
##' @author Marius Hofert
CI <- function(x, alpha = 0.05)
{
    stopifnot(length(x) > 1, 0 < alpha, alpha < 1)
    z <- qnorm(1-alpha/2) # compute normal quantile
    mu.hat <- mean(x) # estimate mean
    SE <- sd(x)/sqrt(length(x)) # estimate standard error
    cbind(mu.hat - z * SE, mu.hat + z * SE) # CLT-based CI
}

## Generate B samples of size n from an Exp(lambda) distribution
B <- 1000 # number of replications
n <- 100 # sample size (should be sufficiently large for the normal approximation of the CLT to hold)
lam <- 2 # lambda
set.seed(271) # for reproducibility
X <- matrix(rexp(n * B, rate = lam), ncol = B) # (n, B)-matrix

## Compute corresponding B asymptotic CIs
CIs <- t(apply(X, 2, CI)) # compute B asymptotic CIs
str(CIs) # (B, 2)-matrix
colnames(CIs) <- c("low", "up")
head(CIs)

## Now let's check how many CIs contain the true mean
true.mean <- 1/lam # true mean of an Exp(lam) distribution
contains.mean <- apply(CIs, 1, function(ci) (ci[1] <= true.mean) && true.mean <= ci[2]) # samples for which CI contains mu
(percent.coverage <- 100 * mean(contains.mean)) # => 94.5% of the 1000 simulated (1-alpha)-CIs contain the true mean (should be ~= (1-alpha)*100%)

## Plot the CIs (by vertical lines). For better visibility, we sort them according
## to their midpoints (estimator of the mean)
CI.midpts <- rowMeans(CIs) # estimated means
ord <- order(CI.midpts) # determine their order
CIs.sort <- CIs[ord, ] # (B, 2)-matrix of CIs sort according to the estimated means
contains.mean.sort <- contains.mean[ord] # ... and sort the indicators whether the true mean is contained in the CI accordingly
plot(NA, type = "n", xlim = c(1, B), ylim = range(CIs),
     xlab = "Sample number (sorted according to CI-midpoints, i.e. the estimated means)",
     ylab = "", main = substitute(B.~"simulated asymptotic 95%-CIs for an Exp("*lam.*") distribution",
                                  list(B. = B, lam. = lam)))
matlines(matrix(rep(1:B, each = 2), ncol = B), # bth column contains two times b
         t(CIs.sort), # (2, B)-matrix
         lty = 1, col = c("maroon3", "royalblue3")[1 + contains.mean.sort]) # note: much quicker than a for loop over B-many individual vertical lines
abline(h = true.mean) # add the true mean
legend("bottomright", bty = "n", lty = rep(1,3), col = c("black", "royalblue3", "maroon3"),
       legend = c(expression("True mean"~mu==1/lambda), expression("95%-CIs covering"~mu),
                  expression("95%-CIs not containing"~mu)))
mtext(substitute("Estimated coverage probability:"~cov*"%", list(cov = round(percent.coverage, 1))),
      side = 4, line = 0.5, adj = 0)
