"""./CMRG_implementation.py"""
# Original R code by Dr. Marius Hofert
# 2022-05-12

# Trascribed to python by Mukto Akash
# Convention: Comments by Marius are indicated using ##,
#             while comments by Mukto have single #.

##' @title L'Ecuyer's Combined Multiple-Recursive Generator
##' @param n sample size
##' @param seed seed
##' @return numeric vector of generated random numbers
##' @author Marius Hofert
##' @note Not optimized for efficiency

def CMRG(n, seed):
    ## Input checks
    assert((n >= 1) &&
        (seed.length == 6) &&
        all(x > 0 for x in seed[0:3]) && # using all() has O(n) time and O(1) space
        all (x > 0 for x in seed[3: 6])  # Note that python is 0-indexed while R is 1-indexed
     )

     ## Initialization for the recursion
    y = x = int(n+3)
    x[1:3] = seed[0:3]
    y[1:3] = seed[3:6]
     m1 = 4294967087 ## = 2^32 - 209
     m2 = 4294944443 ## = 2^32 - 22853

     ## Recursion
    for(i in range(n)) {
        x[i+3] = (1403580 * x[i+1] -  810728 * x[i]) % m1
        y[i+3] = ( 527612 * y[i+2] - 1370589 * y[i]) % m2
    }
    z = (x - y) % m1

    ## Standardize the result and return
    res = z / (m1 + 1)
    res[z == 0] = m1 / (m1 + 1) # inspect this line of code closely
    res[1:n] ## omit the seed

if __name__ == '__main__':
    ## Our own implementation for seed 1:6
    seed = range(6) ## also checked sample(0:1e6, size = 6) for some runs
    n = 1e5
    U = CMRG(n, seed = seed) ## in particular, .Random.seed exists after this call

    