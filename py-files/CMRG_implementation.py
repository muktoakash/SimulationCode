"""./CMRG_implementation.py"""
# Original R code by Dr. Marius Hofert
# 2022-05-12

# Trascribed to python by Mukto Akash

##' @title L'Ecuyer's Combined Multiple-Recursive Generator
##' @param n sample size
##' @param seed seed
##' @return numeric vector of generated random numbers
##' @author Marius Hofert
##' @note Not optimized for efficiency