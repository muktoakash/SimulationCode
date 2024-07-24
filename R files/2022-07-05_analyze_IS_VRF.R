## By Marius Hofert

## Investigating the VRF when estimating mu = P(X > q) for X ~ N(0,1) via IS
## with Exp(q) IS density


## Var, VRF and % improvement as a function of mu
VRF <- function(mu)
{
    ## Numerator of the VRF
    Var.MC <- mu * (1-mu)

    ## Denominator of the VRF (= Var of the IS estimator with Exp(q) IS density for n = 1),
    ## where we express q in terms of mu, too.
    q <- qnorm(1-mu) # since mu = P(X > q) = 1-pnorm(q), we have q = qnorm(1-mu)
    Var.IS <- (exp(-(3/4)*(q^2)) / (2*sqrt(pi)*q)) * pnorm(q/sqrt(2), lower.tail = FALSE) - mu^2

    ## Return variances, VRF and % improvement of the IS over the Var estimator
    c(Var.MC = Var.MC, # variance of the crude MC estimator
      Var.IS = Var.IS, # variance of the importance sampling estimator
      VRF = Var.MC/Var.IS, # variance reduction factor
      Percent = (Var.MC-Var.IS)/Var.MC * 100) # percentage variance improvement
}

## For a fixed mu
mu <- 0.001
VRF(mu) # => VRF ~= 45248, improvment ~= 99.9978%

## Var(IS) as a function of mu
mu <- seq(0, 0.05, length.out = 129)
res <- sapply(mu, VRF) # (4, 129)-matrix
plot(mu, res["Var.IS",],  type = "l", xlab = expression("Probability"~mu),
     ylab = "Variance of the IS estimator for n = 1")
## => The smaller mu, the smaller even the variance of the IS estimators (good)

## VRF
plot(mu, res["VRF",], type = "l", log = "y",
     xlab = expression("Probability"~mu), ylab = "VRF of IS w.r.t. crude MC")
## => Decreasing (good; the smaller mu, the larger the VRF)

## % improvement as a function of mu
plot(mu, res["Percent",], type = "l", xlab = expression("Probability"~mu),
     ylab = "Percentage improvement of variance of IS over crude MC")
## => Large variance reduction overall by using IS, but even more so for small mu.
##    Note that this quantity is independent of 'n'.

## Var(IS) is so small that a single draw from Exp(q) already leads to a good estimate
mu <- 0.001 # consider this true mu as an example
q <- qnorm(1-mu) # corresponding q
set.seed(271) # for reproducibility
Y <- rexp(1, rate = q) # single draw from the IS density
mu.hat.IS <- (exp(-(q^2)/2) / (sqrt(2*pi)*q)) * mean(exp(-(Y^2)/2)) # IS estimate
abs(mu.hat.IS - mu) # absolute error (~ 6e-05)
abs((mu.hat.IS - mu)/mu) # relative error (~ 6%)
