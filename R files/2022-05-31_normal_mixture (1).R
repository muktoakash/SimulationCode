## By Marius Hofert

## Playing with univariate finite normal mixtures


### 1 Sampling #################################################################

## Consider F(x) = (1/K) * \Phi((x - 5*1) / 1) + ... + (1/K) * \Phi((x - 5*K) / K)
## which is an equiprobable mixture (each involved normal distribution has equal
## probability 1/K to be sampled from) of normals.

## We use the composition method for sampling F
K <- 3 # number of components
n <- 5000 # sample size
set.seed(271) # for reproducibility
Z <- sample(1:K, size = n, replace = TRUE) # sample n times uniformly from 1:K
X <- numeric(n) # vector to store the generated samples from X ~ F
for(k in 1:K) { # we iterate over K here instead of n as typically K << n
    ii <- Z == k # pick all Z's that are 'k' (for sampling from the k-th distribution)
    X[ii] <- rnorm(sum(ii), mean = 5 * k, sd = k) # sample from F_{X|Z=k}=\Phi((x-k)/k) as many samples as lie in the k-th bucket
}

## Plot the samples
plot(X, xlab = "Index i", ylab = expression(X[i]), pch = 20)

## Including colors depending on which component distribution was sampled from
cols <- c("black", "royalblue3", "darkorange2")
plot(X, xlab = "Index i", ylab = expression(X[i]), col = cols[Z], pch = 20)


### 2 Plot of the distribution function ########################################

## Define the df
F <- function(x) rowSums(outer(x, 1:K, function(x, k) pnorm((x - 5*k) / k) / K))

## Plot the df
x <- seq(0, 5 * K + 2 * 3, length.out = 257) # plot from 0 to maximal mean plus two standard deviations
opar <- par(mar = par("mar") + c(0, 1.5, 0, 0)) # create more space for the y-axis label
plot(x, F(x), type = "l", ylab = expression(F(x)==sum(Phi((x-5*k)/k)/K, k=k==1,K)))
par(opar) # restore saved plot parameters


### 3 Plot of the density ######################################################

## Define the density
f <- function(x) rowMeans(outer(x, 1:K, function(x, k) dnorm((x - 5*k) / k) / (k*K)))

## Plot the density
x <- seq(0, 5 * K + 2 * 3, length.out = 257)
opar <- par(mar = par("mar") + c(0, 1.5, 0, 0))
plot(x, f(x), type = "l", ylab = expression(f(x)==sum(phi((x-5*k)/k)/(k*K), k=k==1,K)))
par(opar)
## Note:
## - The factor 1/K comes from the mixture distribution, the 1/k is
##   1/<standard deviation of the respective mixture components>
##   since G((x-mu)/sigma) has density g((x-mu)/sigma)/sigma if G is a df
##   with density g.
## - With the three components being centered around 5, 10, 15 and each
##   being weighted with probability 1/3, one might be tempted to assume
##   that the density has a local maximum at 10. But that's not the case, as
##   the density of the second component is affected by the one of the other
##   components (here: especially the third component pulls the probability
##   mass further right).
