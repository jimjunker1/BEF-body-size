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
library(vegan)
'%ni%' <- Negate('%in%')
i_am("R/helpers.R")
update = FALSE
rerun = FALSE
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
#'  @title pareto_expectation
#'  @description
#'  This function estimates the mean body size, or expectation of a community/sample 
#'  that is assumed to follow a Pareto distribution with a given rate exponent, lambda,
#'  and designated minimum and maximum body sizes, xmin and xmax respectively. This function
#'  is useful for estimating the mean body size from sampled data with known sampling biases, 
#'  in this case, known undersampling of small body sizes.
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

#'  @title estimate_pareto_N
#'  @description
#'  This function extrapolates the community abundance, N, from a Pareto distributed 
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

#'
#'
#'
compute_size_spectra <- function(posterior, M_range = NULL, n_points = 100) {
  # posterior: data frame or matrix with columns mu_S, sigma_S, lambda
  # M_range: vector of min/max body sizes (if NULL, inferred from mu_S ± 4*sigma_S)
  
  mu_post <- posterior$mu_S
  sigma_post <- posterior$sigma_S
  lambda_post <- posterior$lambda
  
  # define range of M if not provided
  if(is.null(M_range)) {
    M_min <- exp(min(mu_post - 4*sigma_post))
    M_max <- exp(max(mu_post + 4*sigma_post))
  } else {
    M_min <- M_range[1]
    M_max <- M_range[2]
  }
  
  M_seq <- exp(seq(log(M_min), log(M_max), length.out = n_points))
  
  # Initialize matrices to store slopes
  beta_mat <- matrix(NA, nrow = nrow(posterior), ncol = n_points)
  gamma_mat <- matrix(NA, nrow = nrow(posterior), ncol = n_points)
  
  for(i in 1:nrow(posterior)) {
    beta_mat[i, ] <- - (log(M_seq) - mu_post[i]) / sigma_post[i]^2
    gamma_mat[i, ] <- beta_mat[i, ] + lambda_post[i]
  }
  
  # Compute posterior means and credible intervals
  beta_mean <- apply(beta_mat, 2, mean)
  beta_ci <- t(apply(beta_mat, 2, quantile, probs = c(0.025,0.975)))
  
  gamma_mean <- apply(gamma_mat, 2, mean)
  gamma_ci <- t(apply(gamma_mat, 2, quantile, probs = c(0.025,0.975)))
  
  # Return as data frame
  df <- data.frame(
    M = M_seq,
    beta_mean = beta_mean,
    beta_lower = beta_ci[,1],
    beta_upper = beta_ci[,2],
    gamma_mean = gamma_mean,
    gamma_lower = gamma_ci[,1],
    gamma_upper = gamma_ci[,2]
  )
  
  return(df)
}

#' @title intensity_at_m0
#' @description
#' This function estimates the 'intensity' or abundance at a reference mass
#' given a set lambda, x_min, and x_max. Useful for comparing across communities.
#' 
#'
#'
intensity_at_m0 <- function(m, x_min, x_max, lambda, m0, w = NULL) {
  if (lambda == -1) stop("lambda = -1 not supported")
  if (x_min <= 0 || x_max <= x_min) stop("Invalid bounds")
  if (m0 < x_min || m0 > x_max) warning("m0 outside truncation range")
  
  # total (possibly weighted) abundance
  if (is.null(w)) {
    N <- length(m)
  } else {
    if (length(w) != length(m)) stop("w must match length of m")
    N <- sum(w)
  }
  
  # scaling constant
  c <- N * (lambda + 1) / (x_max^(lambda + 1) - x_min^(lambda + 1))
  
  # intensity at reference mass
  n_m0 <- c * m0^lambda
  return(n_m0)
}