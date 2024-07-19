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

def CMRG(n, seed) {
    ## Input checks
    assert((n >= 1) &&
        (seed.length == 6) &&
        all(x > 0 for x in seed[0:3]) && # using all() has O(n) time and O(1) space
        all (x > 0 for x in seed[3: 6])  # Note that python is 0-indexed while R is 1-indexed
     )
}