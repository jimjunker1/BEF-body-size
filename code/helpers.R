# load packages 
library(here)
library(isdbayes)
library(junkR)
library(neonstore)
library(tidyverse)
i_am("code/helpers.R")
### --- helper functions ---###
#'
#'
#'
pareto_expectation = function(lambda, xmin, xmax){
  if(lambda == -1) {
    # Special case λ = -1
    return((xmax - xmin) / log(xmax / xmin))
  } else if(lambda == -2) {
    # Special case λ = -2
    return((xmin * xmax / (xmin - xmax)) * log(xmax / xmin))
  } else {
    # General case
    numerator <- (lambda + 1) * (xmax^(lambda + 2) - xmin^(lambda + 2))
    denominator <- (lambda + 2) * (xmax^(lambda + 1) - xmin^(lambda + 1))
    return(numerator / denominator)
  }
}

#'
#'
#'
estimate_pareto_N = function(n, lambda, xmin, xmin2, xmax){
  lambdaPlus = lambda + 1
  n * (xmax^(lambdaPlus) - xmin2^(lambdaPlus)) /
    (xmax^(lambdaPlus) - xmin^(lambdaPlus))
}

