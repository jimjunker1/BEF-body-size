# load packages 
library(here)
library(isdbayes)
library(junkR)
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(hillR)
library(janitor)
library(lubridate)
library(tidybayes)
library(brms)
library(ubms)
i_am("R/helpers.R")

# directory
NEON_db_dir = sprintf("C:/Users/%s/OneDrive - UNT System/Projects/database-files",
                      Sys.info()[['user']])

# stream sites
streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU",
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

if(!update){
  date_updated = readRDS(here("data/date_updated.rds"))
  cat(paste0("Data was last updated on ",date_updated))
} else{
  # macroinvertebrates
  neonstore::neon_download(
    product = "DP1.20120.001",
    site = streamsites,
    type ="basic",
    dir = NEON_db_dir,
    .token = Sys.getenv("NEON_TOKEN")
  )
  # fish 
  neonstore::neon_download(
    product = "DP1.20107.001",
    site = streamsites,
    type = "basic",
    dir = NEON_db_dir ,
    .token = Sys.getenv("NEON_TOKEN")
  )
  # stream widths
  neonstore::neon_download(
    product = "DP1.20190.001",
    site = streamsites,
    type = "basic",
    dir = NEON_db_dir ,
    .token = Sys.getenv("NEON_TOKEN")
  )
  # variables
  neonstore::neon_download(
    product="DP1.20190.001",
    start_date="2021-01-01", 
    end_date="2022-01-01",
    table = "variables",
    type="basic",
    site= "ARIK",
    dir = NEON_db_dir ,
    .token = Sys.getenv("NEON_TOKEN")
  )
  
  saveRDS(Sys.Date(), here("data/date_updated.rds"))
  date_updated = readRDS(here("data/date_updated.rds"))
  cat(paste0("Data was last updated on ",date_updated))
}
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
