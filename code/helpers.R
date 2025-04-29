# load packages 
library(here)
library(isdbayes)
library(junkR)
library(neonstore)
library(tidyverse)
library(hillR)
library(lubridate)
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

#'
#'
#'
merge_macrofish_dates = function(mDf = NULL, fDf = NULL, limit = 30,...){
  mDf = get(mDf, envir = .GlobalEnv)
  fDf = get(fDf, envir = .GlobalEnv)
  
  mList = mDf %>% named_group_split(siteID)
  fList = fDf %>% named_group_split(siteID)
  
  mList = mList[names(fList)]
  fList = fList[names(mList)]
  
  fDateList = map2(mList, fList, ~.x$collectDate %>%  
                     map2(., list(.y$collectDate), \(x,y){
                       if(min(abs(y - x)) >= limit){
                         return(NA)
                       } else{
                         d = which(abs(y - x) == min(abs(y - x)))
                         return(unlist(d))
                       }
                     }) %>% unlist)
  
  mfList = pmap(list(mList,
                     fDateList,
                     fList), \(x,y,z){
                       df = x %>%  
                         bind_cols(fDate = z$collectDate[y]) %>% 
                         bind_cols(fishID = z$fishID[y])
                       
                       
                       return(df)
                     })
  
  return(mfList)
}
