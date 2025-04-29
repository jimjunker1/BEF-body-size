#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fish_fit
#' @return
#' @author Jim Junker
#' @export
merge_fish <- function(fish_fit, fish_pass_file) {
  
  fish_fit = readRDS(fish_fit)
  three_pass_df = fish_pass_file

  three_pass_population = as_draws_df(fish_fit@stanfit) %>% 
    select(contains("_state")) %>% 
    pivot_longer(cols = contains("site_int")) %>% 
    clean_names() %>% 
    mutate(value = beta_state_intercept + value) %>% 
    # bind_rows(sample_1) %>% 
    select(name, value) %>% 
    mutate(site_int = as.factor(parse_number(name))) %>% 
    group_by(site_int) %>% # group and summarize
    median_qi(pop_threepass = exp(value)) %>% # summarize on the probability scale (via link function)
    select(-.width, -.point, -.interval) %>% 
    rename(.lower_threepass = .lower,
           .upper_threepass = .upper) %>% #get original group names
    left_join(three_pass_df %>% ungroup %>% 
                mutate(site_int = as.factor(site_int))) %>% 
    mutate(area_m2 = measured_reach_length * mean_wetted_width_m,
           no_fish_per_m2 = pop_threepass/area_m2,
           no_fish_per_m2_lower = .lower_threepass/area_m2,
           no_fish_per_m2_upper = .upper_threepass/area_m2,
           raw_total_per_m2 = total_fish/area_m2)
  
  
## get the relative abundance for extrapolating counts
  # wrangle individual fish counts
  fish_ind_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_perFish-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  fish_measures = fish_ind_load %>%
    select(eventID, taxonID, passStartTime, siteID, namedLocation) %>%
    collect() %>% 
    clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time))) %>% 
    summarise()
  
  # wrangle bulk counts
  fish_count_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_bulkCount-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  fish_bulk = fish_count_load %>%
    select(eventID, taxonID, bulkFishCount, passStartTime, passNumber, siteID, namedLocation)  %>%
    collect() %>% 
    clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time)))

}
