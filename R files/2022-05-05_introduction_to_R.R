## By Marius Hofert

## Introductory R script and playground for learning R. See also Appendix A of
## the manual "An Introduction to R" on http://cran.r-project.org/manuals.html


### Comments ###################################################################

## Q: What is R?
## A: - R is a *free* software environment for *statistical* computing and *graphics*
##    - R was created by *R*obert Gentleman and *R*oss Ihaka in 1993.
##      Since mid-1997, R is developed by the *R Development Core Team* (and
##      contributors)
##    - The sources or R (.tar.gz) are on https://cran.r-project.org/ and
##      consist of base and recommended packages (involving C and Fortran code,
##      too); see installed.packages()[,"Priority"]

## Q: Why R?
## A: - Contributed packages (both the available ones, see
##      https://cran.r-project.org/web/packages/available_packages_by_name.html,
##      and the possibility to write your own)
##    - Ability to write *readable* code (focus is on main aspects of a problem)
##    - High-level programming language (plotting, optimization, run time
##      measurement, debugging, parallel computing etc.). Helps when exploring a
##      problem to gain understanding and insight.

## Q: How to install R (for daily use)?
## A: - Install R from https://cran.r-project.org/
##    - Install an integrated development environment (IDE) such as RStudio.

## Q: How to work with R?
## A: - Create a script myscript.R containing the R source code.
##    - Run the script interactively: Execute it line-by-line (Ctrl + RET)
##    - If required, run the script in batch mode with:
##      R CMD BATCH myscript.R

## Q: How to install R (contributed) packages?
## A: - From CRAN (release version) with install.packages("mypkg").
##    - From a development server (development version), e.g.
##      + R-Forge: install.packages("mypkg", repos = "https://R-Forge.R-project.org")
##      + GitHub: devtools::install_github("maintainer/mypkg")
##    - If required, install from source (.tar.gz):
##      install.packages("mypkg.tar.gz", repos = NULL, lib = "/mydirectory")

## Q: How to find help on R?
## A: - Comprehensive R Archive Network (CRAN):
##      + General manual: https://cran.r-project.org/ -> Manuals -> "An Introduction to R"
##      + Packages: https://cran.r-project.org/ -> Packages -> Table of available packages,
##        sorted by name -> (find the package you are looking for and click it)
##        -> Reference manual (or even 'Package source' or 'Vignettes')
##    - From within an R session, manual pages of functions: '?' (e.g., ?uniroot) or
##      'help("[[")' (for specific functions). Study the examples on the help files.
##    - Google ('r-help')


### Simple manipulations; numbers and vectors ##################################

## Simple manipulations
1/2
1/0 # in R, Inf and -Inf exist and R can often deal with them correctly
1/-0
0/0 # ... also NaN = 'not a number' is available; 0/0, 0*Inf, Inf-Inf lead to NaN
x <- 0/0 # store the result in 'x'
class(x) # the class/type of 'x'; => NaN is still of mode 'numeric'
class(Inf) # Inf is of mode 'numeric' (although mathematically not a number); helpful in optimizations

## Vectors (data structure which contains objects of the same mode)
x <- c(1, 2, 3, 4, 5) # numeric vector
length(x) # its length
x # print method
(y <- 1:5) # another way of creating such a vector (and *printing* the output via '()')
(z <- seq_len(5)) # and another one (see below for the 'why')
z[6] <- 6 # append to a vector (better than z <- c(z, 6)); (much) more comfortable than in C/C++
z

## Note: We can check whether the R objects are the same
x == y # component wise numerically equal
identical(x, y) # identical as objects? why not?
class(x) # => x is a *numeric* vector
class(y) # => y is an *integer* vector
all.equal(x, y) # numerical equality; see argument 'tolerance'
identical(x, as.numeric(y)) # => also fine

## Numerically not exactly the same
x <- var(1:4)
y <- sd(1:4)^2
all.equal(x, y) # numerical equality
x == y # ... but not exactly
x - y # numerically not 0
## See also https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f

## Watch out
n <- 0
1:n # not the empty sequence but c(1, 0); caution in 'for loops': for(i in 1:n) ...!
seq_len(n) # better: => empty sequence
seq_along(c(3, 4, 2)) # 1:3; helpful to 'go along' objects

## Watch out
1:3-1 # ':' has higher priority; note also: the '-1' is recycled to the length of 1:3
1:(3-1)

## Some functions
(x <- c(3, 4, 2))
length(x) # as seen above
rev(x) # change order
sort(x) # sort in increasing order
sort(x, decreasing = TRUE) # sort in decreasing order
ii <- order(x) # create the indices which sort x; geek zone: x[order(x)][order(order(x))] == x
x[ii] # => sorted
log(x) # component-wise logarithms
x^2 # component-wise squares
sum(x) # sum all numbers
cumsum(x) # compute the *cumulative* sum
prod(x) # multiply all numbers
seq(1, 7, by = 2) # 1, 3, 5, 7
rep(1:3, each = 3, times = 2) # 1 1 1 2 2 2 3 3 3  1 1 1 2 2 2 3 3 3
tail(x, n = 1) # get the last element of a vector
head(x, n = -1) # get all but the last element

## Logical vectors
logical(0) # the empty logical vector
(ii <- x >= 3) # logical vector indicating whether each element of x is >= 3
x[ii] # use that vector to index x => pick out all values of x >= 3
!ii # negate the logical vector
all(ii) # check whether all indices are TRUE (whether all x >= 3)
any(ii) # check whether any indices are TRUE (whether any x >= 3)
ii |  !ii # vectorized logical OR (is, componentwise, any entry TRUE?)
ii &  !ii # vectorized logical AND (are, componentwise, both entries TRUE?)
ii || !ii # logical OR applied to all values (is entry any TRUE?)
ii && !ii # logical AND applied to all values (are all entries TRUE?)
3 * c(TRUE, FALSE) # TRUE is coerced to 1, FALSE to 0
class(NA) # NA = 'not available' is 'logical' as well (used for missing data)
z <- 1:3; z[5] <- 4 # two statements in one line (';'-separated)
z # => 4th element 'not available' (NA)
(z <- c(z, NaN, Inf)) # append NaN and Inf
class(z) # still numeric (although is.numeric(NA) is FALSE)
is.na(z) # check for NA or NaN
is.nan(z) # check for just NaN
is.infinite(z) # check for +/-Inf
z[(!is.na(z)) & is.finite(z) & z >= 2] # subsetting; pick out all finite numbers >= 2

## Character vectors
character(0) # the empty character vector
x <- "apple"
y <- "orange"
(z <- paste(x, y)) # paste together; use sep = "" or paste0() to paste without space
paste(1:3, c(x, y), sep = " - ") # recycling ("apple" appears again)

## Named vectors
(x <- c("a" = 3, "b" = 2)) # named vector of class 'numeric'
x["b"] # indexing elements by name (useful!)
x[["b"]] # drop the name


### Arrays and matrices ########################################################

## Matrices
(A  <- matrix(1:12, ncol = 4)) # watch out, R operates on/fills by *columns*
(A. <- matrix(1:12, ncol = 4, byrow = TRUE)) # fills matrix row-wise
(B <- rbind(1:4, 5:8, 9:12)) # row bind
(C <- cbind(1:3, 4:6, 7:9, 10:12)) # column bind
stopifnot(identical(A, C), identical(A., B)) # check whether the constructions are identical
cbind(1:3, 5) # recycling
(A <- outer(1:4, 1:5, FUN = pmin)) # (4,5)-matrix with (i,j)th element min{i, j}
## => Lower triangular matrix contains column number, upper triangular matrix contains row number

## Some functions
nrow(A) # number of rows
ncol(A) # number of columns
dim(A) # dimension
diag(A) # diagonal of A
diag(3) # identity (3, 3)-matrix
(D <- diag(1:3)) # diagonal matrix with elements 1, 2, 3
D %*% B # matrix multiplication
B * B # Hadamard product, i.e., element-wise product
rowSums(A) # row sums
apply(A, 1, sum) # the same
colSums(A) # column sums
apply(A, 2, sum) # the same

## Array (data structure which contains objects of the same mode)
## Special cases: vectors (1d-arrays) and matrices (2d-arrays)
arr <- array(1:24, dim = c(2,3,4),
             dimnames = list(x = c("x1", "x2"),
                             y = c("y1", "y2", "y3"),
                             z = paste("z", 1:4, sep = ""))) # (2,3,4)-array with dimensions (x,y,z)
arr # => also filled in the first dimension first, then the second, then the third
str(arr) # use str() to the *str*ucture of the object arr
arr[1,2,2] # pick out a value
arr. <- aperm(arr, perm = c(3,1,2)) # permute the array to dimensions (z,x,y)
str(arr.)
(mat <- apply(arr, 1:2, FUN = sum)) # for each combination of fixed first and second variables, sum over all other dimensions


### Lists (including data frames) ##############################################

## Data frames are rectangular objects containing objects of possibly different
## type of the same length
(df <- data.frame(Year = as.factor(c(2000, 2000, 2000, 2001, 2003, 2003, 2003)), # loss year
                  Line = c("A", "A", "B", "A", "B", "B", "B"), # business line
                  Loss = c(1.2, 1.1, 0.6, 0.8, 0.4, 0.2, 0.3))) # loss in M USD, say
str(df) # => first two columns are factors
is.matrix(df) # => indeed no matrix
as.matrix(df) # coercion to a character matrix
data.matrix(df) # coercion to a numeric matrix

## Computing maximal losses per group for two different groupings
## Omits NAs:
aggregate(Loss ~ Year, data = df, FUN = max)
aggregate(Loss ~ Year * Line, data = df, FUN = max)
## Another version (results in a table structure with all combinations of variables):
## tapply(df[,"Loss"], df[,"Year"], max) # maximal loss per Year
## tapply(df[,"Loss"], df[,c("Year", "Line")], max) # maximal loss per Year-Line combination


## Lists
is.list(df) # => data frames are indeed just lists

## Lists are the most general data structures in R in the sense that they
## can contain pretty much everything, e.g., lists themselves or functions
## or both... (and of different lengths)
(L <- list(group = LETTERS[1:4], value = 1:2, sublist = list(10, function(x) x+1)))

## Extract elements from a list
## Version 1:
L[[1]] # get first element of the list
L[[3]][[1]] # get first element of the sub-list
## Version 2: use '$'
L$group
L$sublist[[1]]
## Version 3 (most readable and fail-safe): use the provided names
L[["group"]]
L[["sublist"]][[1]]

## Change a name
names(L)
names(L)[3] <- "sub.list"
str(L)

## Watch out
L[[1]] # the first component
L[1] # the sub-list containing the first component of L
class(L[[1]]) # character
class(L[1]) # list


### Using implemented distributions ############################################

## Probability distributions (d/p/q/r*)
dexp(1.4, rate = 2) # density f(x) = 2*exp(-2*x)
pexp(1.4, rate = 2) # distribution function F(x) = 1-exp(-2*x)
qexp(0.3, rate = 2) # quantile function F^-(y) = -log(1-y)/2
rexp(4,   rate = 2) # draw random variates from Exp(2)


### Random number generation ###################################################

## Generate from N(0,1)
(X <- rnorm(2)) # generate two N(0,1) random variates
str(.Random.seed) # encodes types of random number generators (RNGs) and the seed
## Note (see ?.Random.seed):
## - The first integer in .Random.seed encodes the ...
##   1) U(0,1) RNG (lowest two decimals; '03' = 'Mersenne Twister')
##   2) N(0,1) RNG (the hundreds; '4' = 'Inversion')
##   3) U{1,...,n} RNG (the ten thousands; '10' = 'Rejection')
##   Note:
##   + 3) was newly added in R 3.6.0
##   + 3) is used in sample() -> sample.int() -> C functions sample and sample2
##     -> ./src/main/names.c: do_sample and do_sample2 -> ./src/main/random.c
##     (for do_sample => uses the alias method in the non-uniform case and
##     R_unif_index in the uniform case) and ./src/main/unique.c (for do_sample2
##     => uses R_unif_index -> ./src/main/RNG.c which generates (a vector of)
##     random bits until a number < n appears (rejection algorithm); 'dn' most
##     likely stands for 'n as a double'.
## - The remaining integers denote the actual seed.
## - The default kind is the "Mersenne Twister" (which needs an integer(624)
##   as seed and the current position in this sequence, so 625 numbers).
## - If no random numbers were generated yet in an R session, .Random.seed
##   will not exist.
RNGkind() # => Mersenne Twister, with inversion for N(0,1) and rejection for U{1,..,n}

## How can we make sure to obtain the same results (for *reproducibility*?)
(Y <- rnorm(2)) # => another two N(0,1) random variates
all.equal(X, Y) # obviously not equal (here: with probability 1)

## Set a 'seed' so that computations are reproducible
set.seed(271) # with set.seed() we can set the seed
X <- rnorm(2) # draw two N(0,1) random variates
set.seed(271) # set the same seed again
Y <- rnorm(2) # draw another two N(0,1) random variates
all.equal(X, Y) # => TRUE
## Note:
## - If you just start R without calling set.seed(), a seed is constructed from
##   system time and the R process number. You can also do rm(.Random.seed)
##   and call rnorm() thereafter to convince yourself that .Random.seed is
##   newly generated.
## - In the above code, if Y <- rnorm(3), then all.equal(X, Y[1:2]) is also TRUE.

## A pseudo-random number generator which allows for easily advancing the
## seed is L'Ecuyer's combined multiple-recursive generator (CMRG); see
## MRG32k3a.c and MRG32k3a.h on http://simul.iro.umontreal.ca/rng (R's version
## only makes minor modifications to this). Let's see how we can call it from R.
RNGkind() # => Mersenne Twister, inversion is used for generating N(0,1)
RNGkind("L'Ecuyer-CMRG")
RNGkind() # => L'Ecuyer's CMRG, inversion is used for generating N(0,1)
.Random.seed # => now of length 7 (first number similarly as above and seed of length 6)
Z <- rnorm(2) # use L'Ecuyer's CMRG for generating random numbers
library(parallel) # for nextRNGStream() for advancing the seed
.Random.seed <- nextRNGStream(.Random.seed) # advance seed by 2^127
Z. <- rnorm(2) # generate from next stream => will be 'sufficiently apart' from Z
RNGkind("Mersenne-Twister") # switch back to Mersenne-Twister
RNGkind()


### Control statements #########################################################

## R has if() else, ifelse() (a vectorized version of 'if'), for loops (avoid or
## only use if they don't take much run time), repeat and while (with 'break' to
## exit and 'next' to advance to the next loop iteration)

## ... without going into details, note that even 'if()' is a function, so
## instead of:
x <- 4
if(x < 5) y <- 1 else y <- 0 # y is the indicator whether x < 5
## ... write (the much more readable)
y <- if(x < 5) 1 else 0
## ... or even better
(y <- x < 5) # ... as a logical
y + 2 # ... which is internally again converted to {0,1} in calculations

## Also, loops of the type...
x <- integer(5)
for(i in 1:5) x[i] <- i * i
## ... can typically be avoided by something like
x. <- sapply(1:5, function(i) i * i) # of course we know that this is simply (1:5)^2 which is even faster
stopifnot(identical(x, x.))

## For efficient R programming, the following functions are useful (geek zone):
lapply(1:5, function(i) i * i) # returns a list
sapply(1:5, function(i) i * i) # returns a *s*implified version (here: a vector)
sapply # => calls lapply()
unlist(lapply(1:5, function(i) i * i)) # a bit faster than sapply()
vapply(1:5, function(i) i * i, NA_real_) # even faster but we have to know the return value of the function


### Writing functions ##########################################################

## Let's say you don't like R's default plot symbol (a circle) and color (black)
plot(1:10, 10:1)

## You could always write out all arguments
plot(1:10, 10:1, pch = 20, col = "royalblue3")

## Or define your own function
myplot <- function(...) # note: no curly braces {} needed when only one line
    plot(..., pch = 20, col = "royalblue3")
myplot(1:10, 10:1)

## But what happens if you want to change the color for a single plot
myplot(1:10, 10:1, col = "maroon3") # fails, as we hard-coded the color

## Better version of myplot
myplot <- function(..., pch = 20, col = "royalblue3")
    plot(..., pch = pch, col = col)
myplot(1:10, 10:1)
myplot(1:10, 10:1, col = "maroon3")

## Now let's plot to a pdf
file <- "myfilename.pdf"
pdf(file, width = 6, height = 6) # open plot device
myplot(1:10, 10:1, col = "maroon3")
dev.off() # close plot device
if(file.exists(file)) file.remove(file) # clean-up


## Let's write a function for computing the mean of an Exp(lambda) analytically or
## via Monte Carlo

##' @title Function for Computing the Mean of an Exp(lambda) Distribution
##' @param lambda parameter (vector of) lambda > 0
##' @param n Monte Carlo sample size
##' @param method character string indicating the method to be used:
##'        "analytical": analytical formula 1/lambda
##'        "MC": Monte Carlo based on the sample size 'n'
##' @return computed mean(s)
##' @author Marius Hofert
##' @note vectorized in lambda
exp_mean <- function(lambda, n, method = c("analytical", "MC"))
{
    stopifnot(lambda > 0) # input check(s)
    switch(match.arg(method), # distinguish (switch between) the two methods
           "analytical" = {
               1/lambda # vectorized in lambda (i.e., works for a vector of lambda's)
               ## Note: We did not use 'n' in this case, so although it is a
               ##       formal argument, we do not need to provide it.
           },
           "MC" = {
               if(missing(n)) # checks the formal argument 'n'
                   stop("You need to provide the Monte Carlo sample size 'n'")
               E <- rexp(n)
               sapply(lambda, function(l) mean(E/l)) # vectorized in lambda
           },
           stop("Wrong 'method'"))
}
## Note: We could have also omitted 'n' and used '...'. We would then need
##       to check in the "MC" case whether 'n' was provided. Checking can
##       be done with hasArg(n) in this case and 'n' can be obtained with
##       list(...)$n.

## Calls
(true <- exp_mean(1:4))
set.seed(271)
sim <- exp_mean(1:4, n = 1e7, method = "MC")
stopifnot(all.equal(true, sim, tol = 0.001))


## A word concerning efficiency

## Some function mimicking a longer computation
f <- function() {
    ## Longer computation to get 'x'
    Sys.sleep(1) # mimics a longer computation
    x <- 1
    ## Longer computation to get 'y'
    Sys.sleep(1) # mimics a longer computation
    y <- 2
    ## Return
    list(x = x, y = y)
}

## If you need both values 'x' and 'y', then do *not* do...
x <- f()$x
y <- f()$y
## ... as this calls 'f' twice. Do this instead:
res <- f() # only one function call
x <- res$x
y <- res$y


### Misc #######################################################################

## Not discussed here:
## - How to read/write data from/to a file.
##   This can be done with read.table()/write.table(), for example.
##   For .csv files, there are the convenience wrappers
##   read.csv()/write.csv().
## - How to load/save R objects from/to a file.
##   This can be done using load()/save()

q() # quit the R session
