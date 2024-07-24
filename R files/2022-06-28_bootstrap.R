## By Marius Hofert

## We estimate the mean E(X) and the 99%-quantile F_X^-(0.99) of the distribution
## of a sum X of dependent random variables. We then bootstrap these two
## quantities and assess how precise the respective estimators are.


### Setup ######################################################################

library(copula) # for generating dependent data specified via a copula


### 1 Generate the dependent data ##############################################

## Sample from the sum of three dependent random variables, so
## X = X_1 + X_2 + X_3, where (X_1, X_2, X_3) have a t_{3.5} copula (with
## parameter such that Kendall's tau is 0.5, so a mid-range dependence) and
## t_2, t_3 and t_4 margins.
d <- 3 # copula dimension
nu.cop <- 3.5 # copula degrees of freedom
cop <- tCopula(iTau(tCopula(), tau = 0.5), dim = d, df = nu.cop) # specifying the copula
nu.mar <- 1 + 1:d # marginal degrees of freedom
n <- 100 # relatively small sample size
set.seed(271)
U <- rCopula(n, copula = cop) # sample from the copula
Y <- sapply(1:d, function(j) qt(U[,j], df = nu.mar[j])) # transform margins to t (second part of Sklar's Theorem)
X <- rowSums(Y) # build dependent sum
## Note:
## - X could be realizations of the total loss of d business lines in a company
## - Note that the model used here is more flexible than a 3-dimensional t
##   distribution as we can have different degrees of freedom for the copula and
##   all the margins (in a joint t distribution, all would need to be the same)


### 2 Estimating E(X) and F_X^-(0.99) ##########################################

## From now on we do as if we don't know the df F_X the data came from (which is
## the case in a practical applications anyway).

## Say we want estimate the mean E(X) and F_X^-(0.99).
## This can be done like this:
(mu <- mean(X))
q99 <- function(p) quantile(p, probs = 0.99, names = FALSE) # helper function
(q <- q99(X))
## ... But these are only numbers (the estimates), we don't know how precise
## these estimates (or rather, their corresponding estimators) are.


### 3 Bootstrap the two estimates ##############################################

## We generate bootstrap samples and compute the estimators of E(X) and
## F_X^-(0.99) for each bootstrap sample.
B <- 1000 # number of bootstrap replications
X.boot <- matrix(sample(X, size = B * n, replace = TRUE), nrow = B) # (n, B)-matrix of resampled X
boot.mu <- colMeans(X.boot) # B-vector of estimated means
boot.q  <- apply(X.boot, 2, q99) # B-vector of estimated 99%-quantiles
## Now we have replications of the estimates of E(X) and F_X^-(0.99) and we
## can investigate their properties. Although these realizations are obtained
## from data from hat{F}_n (and not from F; this is due to the resampling), we
## do as if they (approximately) come from F.


### 4 Analyze the two bootstrap estimates ######################################

## Compute the bootstrap standard deviation for each estimator
## (so the sample standard deviation of the bootstrap estimates)
sd(boot.mu)
sd(boot.q) # ... significantly larger (which is not a surprise as F_X^-(0.99) looks further into the tail)
## Note: We could now report these numbers with the estimates computed in
##       Section 2 above. The bootstrap allows us to get (approximate) standard
##       deviations even though we only have one original dataset. An alternative
##       would be to split up the 100 original data points we have in equally
##       sized buckets of data points and compute estimates for each bucket,
##       but this leads to a much smaller sample size per bucket.

## Compute the bootstrap 95% confidence interval for each estimator
## (so the sample 2.5%- and 97.5%-quantiles of the bootstrap estimates)
quantile(boot.mu, probs = c(0.025, 0.975))
quantile(boot.q,  probs = c(0.025, 0.975)) # ... much wider (same reason)

## How about the two bootstrap distributions? We can compare them e.g. with a boxplot
boxplot(boot.mu, boot.q, names = c("Bootstrap estimates for E(X)",
                                   expression("Bootstrap estimates for"~{F^"-"}(0.99))))
mtext(substitute("Based on"~B==B.~"bootstrap replications of size"~n==n.~"each",
                 list(B. = B, n. = n)), side = 4, line = 0.5, adj = 0)
## This confirms what we have seen before: It is much more difficult to accurately
## estimate a high quantile than the mean.
