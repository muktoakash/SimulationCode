## By Marius Hofert

## Sampling algorithms used by R

## Note: - ./R-4.2.0/src/library/stats/R/distn.R provides the R interfaces
##         to the underlying C code
##       - Especially important is ./R-4.2.0/src/main/names.c which contains
##         the names of the C functions called.
##       - The line numbers below can change per R version


### 1 Discrete distributions ###################################################

## Uniform U({1,..n}) (e.g., for n = 5: sample(1:5, size = 100, replace = TRUE))
sample # => sample.int()
sample.int # => .Internal(sample)
## (grep) => ./R-4.2.0/src/main/names.c:488 => do_sample()
## - for unequal probabilities:
##   ./R-4.2.0/src/main/random.c:499 => walker_ProbSampleReplace()
##   => ./R-4.2.0/src/main/random.c:346 => alias method (not discussed)
## - for equal probabilities: ./R-4.2.0/src/main/random.c:545
##   => calls (int)(R_unif_index(n))
##   => ./R-4.2.0/src/main/RNG.c:870 => random bits & rejection; see RNGkind()

## Binomial B(n, p) (note: For this distribution it's trickier to see what's used)
rbinom # => .Call(C_rbinom, n, size, prob)
## => ./R-4.2.0/src/main/names.c:472 => do_random2()
## => ./R-4.2.0/src/main/random.c:187 => C's rbinom() (via RAND2())
## => Not so easy to spot, but:
##    - for np >= 30: ./R-4.2.0/src/nmath/rbinom.c:117
##      Composition method with:
##      + Left and right tail: Rejection method with exponential envelope
##      + Body: Rejection method with hat envelope and triangle for squeezing
##    - for np < 30: ./R-4.2.0/src/nmath/rbinom.c:185 => inversion method

## Geometric Geo(p) on IN_0
rgeom # => .Call(C_rgeom, n, prob)
## => ./R-4.2.0/src/main/names.c:466: => do_random1()
## => ./R-4.2.0/src/main/random.c:104 => C's rgeom (via RAND1())
## => ./R-4.2.0/src/nmath/rgeom.c:49
## => Uses the stochastic representation Poi(Exp(p/(1-p))) (and not floor(Exp(-log(1-p))))

## Negative binomial NB(r, p)
rnbinom # => .Call(C_rnbinom, n, size, prob)
## => ./R-4.2.0/src/main/names.c:478: => do_random2()
## => ./R-4.2.0/src/main/random.c:193 => C's rnbinom (via RAND2())
## => ./R-4.2.0/src/nmath/rnbinom.c:53
## => Uses the stochastic representation Poi(Gamma(r, scale = (1-p)/p))

## Poisson Poi(lambda) (note: For this distribution it's trickier to see what's used)
rpois # => .Call(C_rpois, n, lambda)
## => ./R-4.2.0/src/main/names.c:467 => do_random1()
## => ./R-4.2.0/src/main/random.c:105 => C's rpois (via RAND1())
## => ./R-4.2.0/src/nmath/rpois.c:54
## => Not so easy to spot, but:
##    - lambda (= mu) < 10: Inversion method (based on table of precomputed probabilities)
##    - lambda >= 10:       Rejection method with floor(N(lambda, lambda)) as envelope
##                          (incl. squeezing)


### 2 Continuous distributions #################################################

## Uniform U(a,b)
runif # => .Call(C_runif, n, min, max)
## => ./R-4.2.0/src/main/names.c:482 => do_random2()
## => ./R-4.2.0/src/main/random.c:195 => C's runif (via RAND2())
## => ./R-4.2.0/src/nmath/runif.c:37 => Uses a + (b - a) * unif_rand()
## => ./R-4.2.0/src/main/RNG.c:136 => MT_genrand
## => ./R-4.2.0/src/main/RNG.c:670

## Normal N(mu, sig2)
rnorm # => .Call(C_rnorm, n, mean, sd)
## => ./R-4.2.0/src/main/names.c:481 => do_random2()
## => ./R-4.2.0/src/main/random.c:194 => C's rnorm (via RAND2())
## => ./R-4.2.0/src/nmath/rnorm.c:40 => Uses mu + sigma * norm_rand()
## => ./R-4.2.0/src/nmath/snorm.c:270 => uses inversion via qnorm5()
## => ./R-4.2.0/src/nmath/qnorm.c:52

## Log-normal LN(mu, sig2)
rlnorm # => .Call(C_rlnorm, n, meanlog, sdlog)
## => ./R-4.2.0/src/main/names.c:476 => do_random2()
## => ./R-4.2.0/src/main/random.c:191 => C's rlnorm (via RAND2())
## => ./R-4.2.0/src/nmath/rlnorm.c:37 => Uses e^rnorm()

## Student t_nu
rt # => .Call(C_rt, n, df)
## => ./R-4.2.0/src/main/names.c:468 => do_random1()
## => ./R-4.2.0/src/main/random.c:106 => C's rt (via RAND1())
## => ./R-4.2.0/src/nmath/rt.c:41 => Uses norm_rand()/ sqrt(rchisq(df) / df)

## Exponential Exp(lambda)
rexp # => .Call(C_rexp, n, 1/rate)
## => ./R-4.2.0/src/main/names.c:465 => do_random1()
## => ./R-4.2.0/src/main/random.c:103 => C's rexp (via RAND1())
## => ./R-4.2.0/src/nmath/rexp.c:40 => Uses scale * exp_rand()
## => ./R-4.2.0/src/nmath/sexp.c:39 (more involved and not just via inversion):
##    composition + table look-up + rejection with polynomial envelopes

## Gamma Gamma(alpha, beta)
rgamma # => .Call(C_rgamma, n, shape, scale)
## => ./R-4.2.0/src/main/names.c:475 => do_random2()
## => ./R-4.2.0/src/main/random.c:190 => C's rgamma (via RAND2())
## => ./R-4.2.0/src/nmath/rgamma.c:97
## => Uses:
##    alpha <  1: Rejection with envelope h(x) <= x^{alpha-1}/Gamma(alpha)
##                for x in [0,1] and h(x) <= exp(-x)/Gamma(alpha) for x > 1.
##    alpha >= 1: Rejection method based on normal envelope with squeezing

## Chi-squared chi^2_nu
rchisq # => .Call(C_rchisq, n, df)
## => ./R-4.2.0/src/main/names.c:464 => do_random1()
## => ./R-4.2.0/src/main/random.c:102 => C's rchisq (via RAND1())
## => ./R-4.2.0/src/nmath/rchisq.c:40 => Uses rgamma(df / 2.0, 2.0) (scale = 2)
