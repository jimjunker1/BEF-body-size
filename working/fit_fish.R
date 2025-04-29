#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param update
#' @return
#' @author Jim Junker
#' @export
fit_fish <- function(fish_pass_file) {

three_pass_data_wide_total_fish = readRDS(fish_pass_file)
## Model fitting -------------
# Fits a multinomial Poisson depletion model to estimate fish density per collection
# The result is combined in the next script with fish sizes to generate fish size density so that fish can be combined with macroinvertebrates.

# load data (filter out zeros...no fish collected also won't have body sizes)
three_pass_data_wide = three_pass_data_wide_total_fish  %>% 
  mutate(total_fish = `1` + `2` + `3`) %>% 
  filter(total_fish > 0) %>% 
  filter(site_id %in% streamsites)

# fit multinomial poisson (three pass depletion model) -----------------------------------------------

# put passes in a matrix (for ubms)
three_pass_matrix = three_pass_data_wide %>% 
  select(`1`,`2`,`3`) %>% 
  as.matrix()

# assign covariates (for ubms)
three_pass_frame <- unmarkedFrameMPois(three_pass_matrix,
                                       siteCovs=as.data.frame(three_pass_data_wide %>% select(site_int)),
                                       type = "removal")

three_pass_model = stan_multinomPois(
  formula = ~1 + (1|site_int) ~ 1 + (1|site_int),
  data = three_pass_frame,
  iter = 2000,
  chains = 4, cores = 4,
  thin = 1, seed = 1312
)

saveRDS(three_pass_model, file = here("data/models/three_pass_model.rds"))
return(here("data/models/three_pass_model.rds"))

}
