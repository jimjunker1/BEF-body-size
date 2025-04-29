#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param update
#' @return
#' @author Jim Junker
#' @export
update_data <- function(update) {
if(!update){
  date_updated = readRDS(here("data/date_updated.rds"))
  return(paste0("Data was last updated on ",date_updated))
} else{
  # macroinvertebrates
  neonstore::neon_download(
    product = "DP1.20120.001",
    dir = NEON_db_dir,
    .token = Sys.getenv("NEON_TOKEN")
  )
  # fish 
  neonstore::neon_download(
    product = "DP1.20107.001",
    site = streamsites,
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
  return(paste0("Data was last updated on ",date_updated))
}
}
