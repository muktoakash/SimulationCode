## By Marius Hofert

## Visually checking the Glivenko-Cantelli Theorem by simulation for an Exp(1)
## distribution


## Sample exponential random variates for different sample sizes
lambda <- 1 # we consider standard exponentials here
n <- 2^(1:16) # sample sizes for the different edfs we consider
set.seed(271)
X <- rexp(max(n), rate = lambda) # sample Exp(1) (for the maximal n; for smaller sample sizes, we recycle)

## Plot (with pausing in-between)
xran <- range(X) # for determining the evaluation points of the edfs (wherever we have data)
x <- seq(xran[1], xran[2], length.out = 501) # evaluation points
for(i in seq_along(n)) # iterate over all n
{
    plot(x, pexp(x, rate = lambda), type = "l", col = "royalblue3", ylab = "") # true df
    lines(x, ecdf(X[1:n[i]])(x)) # edf based on the first n[i] random variates
    legend("bottomright", bty = "n", lty = c(1, 1), col = c("royalblue3", "black"),
           legend = as.expression(c(substitute(Exp(l)~"df", list(l = lambda)),
                                    substitute(hat(F)[n](x)~"(with vertical lines)", list(n = n[i])))))
    Sys.sleep(0.5) # put the system to sleep for 0.5s
}
