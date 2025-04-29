#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param update
#' @return
#' @author Jim Junker
#' @export
wrangle_fish <- function(update) {

  # load the stores
  ## fish
  neonstore::neon_store(
    product = "DP1.20107.001",
    dir = NEON_db_dir,
    db = neon_db(NEON_db_dir, read_only = FALSE)
  )
  ## widths
  neon_store(
    product = "DP1.20190.001",
    table = "rea_widthFieldData-basic",
    dir = NEON_db_dir,
    db = neon_db(NEON_db_dir, read_only = FALSE)
  ) 
  # lazy load the needed tables ----
  # lazy load the widths
  widths_load = neon_table(
    product = "DP1.20190.001",
    table = "rea_widthFieldData-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  # wrangle and summarize the wetted widths measures
  mean_wetted_width = widths_load %>% 
    select(siteID, collectDate, wettedWidth) %>% 
    collect() %>% 
    clean_names() %>% 
    mutate(year = year(collect_date),
           month = month(collect_date),
           year_month = paste(year,month, sep = "_")) %>%
    group_by(site_id) %>%
    summarize(mean_wetted_width_m = mean(wetted_width, na.rm = T),
              sd_wetted_width_m = sd(wetted_width, na.rm = T))
  #####
  # wrangle the events level data
  
  fish_pass_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_perPass-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  reach_event = fish_pass_load %>% 
    select(namedLocation, eventID) %>% 
    collect() %>% 
    distinct() %>% 
    clean_names()
  
  # get reachids, eventids, and targets (i.e., passes that returned zero fish will have "N")
  true_zeros = fish_pass_load %>%
    select(siteID, namedLocation, passNumber, eventID, targetTaxaPresent, passStartTime) %>%
    collect() %>% 
    clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time))) %>% 
    select(-pass_start_time) %>% 
    distinct()  %>% # removes duplicates. JSW confirmed that these were true duplicates on 2023-03-01
    filter(target_taxa_present == "N")
  
  # wrangle the field data
  fish_field_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_fieldData-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  # get info on whether reaches are fixed or random. Only fixed reaches have 3 
  # pass removal. Random reaches are all single pass
  fixed_random_reach = fish_field_load %>% 
    select(namedLocation, fixedRandomReach) %>% 
    collect() %>% 
    distinct(namedLocation, fixedRandomReach) %>% 
    clean_names() %>% 
    filter(fixed_random_reach == 'fixed')
  
  # wrangle reach lengths
  fish_reach_length = fish_field_load %>% 
    collect() %>% 
    clean_names() %>% 
    mutate(collect_date = start_date) %>% 
    distinct(named_location, event_id, measured_reach_length, collect_date, site_id) %>%
    group_by(collect_date, site_id, named_location) %>%
    add_tally() %>%
    filter(n == 1)
  
  # wrangle individual fish counts
  fish_ind_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_perFish-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  # The next two steps combine two datasets. These are needed to obtain the number
  # of fish per pass. Either alone would be an inaccurate number.
  # Get fish abundance from fsh_perFish. The first 50 fish are measured for length. 
  fish_measures = fish_ind_load %>%
    select(eventID, taxonID, passStartTime, passNumber, siteID, namedLocation) %>%
    collect() %>% 
    clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time)))%>% 
    left_join(reach_event) %>%
    rename(pass = pass_number) %>% 
    group_by(site_id, collect_date, pass, event_id, named_location) %>%
    tally()
  
  # wrangle bulk counts
  fish_count_load = neon_table(
    product = "DP1.20107.001",
    table = "fsh_bulkCount-basic",
    db = neon_db(NEON_db_dir),
    lazy = TRUE
  )
  # all fish after the first 50 are bulk counted. Those values are here.
  fish_bulk = fish_count_load %>%
    select(eventID, taxonID, bulkFishCount, passStartTime, passNumber, siteID, namedLocation)  %>%
    collect() %>% 
    clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time))) %>% 
    left_join(reach_event) %>%
    rename(pass = pass_number) %>%
    mutate(n = as.integer(bulk_fish_count)) %>% 
    select(-pass_start_time)
  
  # bring it all together
  # combine the 1st 50 fish with the bulk counts to get a total number of fish per pass.
  # Then add identifying information
  three_pass_data = bind_rows(fish_bulk, fish_measures) %>%
    as_tibble() %>% glimpse() %>% 
    group_by(pass, event_id, collect_date, named_location, site_id) %>%
    summarize(total_fish = sum(n, na.rm = T)) %>%
    # separate(reach_id, into = c("site_id", "date", "reach"), remove = F) %>%
    mutate(month = month(collect_date),
           year = year(collect_date),
           year_month = paste(year, month, sep = "_")) %>%
    ungroup %>%
    left_join(fish_reach_length) %>%
    left_join(mean_wetted_width) %>%
    left_join(fixed_random_reach)
  
  # make wide format. Replace 0's using information in fsh_perPass$target_taxa_present
  three_pass_data_wide_total_fish = three_pass_data %>%   # restrict to fixed reaches only
    filter(fixed_random_reach == "fixed") %>%
    # select(-event_id) %>%
    group_by(named_location, site_id, collect_date, pass, month, year, year_month, measured_reach_length, n, 
             mean_wetted_width_m, sd_wetted_width_m, fixed_random_reach
    ) %>% 
    reframe(total_fish = sum(total_fish)) %>% 
    pivot_wider(names_from = pass, values_from = total_fish) %>% # four events have the 3rd pass entered in two numbers. This totals them so we can pivot.
    ungroup() %>%
    replace_na(list(`1` = 0, # replace NA with zeros (assumes zero fish if there were no values entered)
                    `2` = 0,
                    `3` = 0)) %>%
    left_join(true_zeros)  %>%
    mutate(`1` = case_when(`1` == 0 & is.na(target_taxa_present) ~ 1e9,   # create silly number to filter out false zeros
                           TRUE ~ `1`),
           `2` = case_when(`2` == 0 & is.na(target_taxa_present) ~ 1e9,
                           TRUE ~ `2`),
           `3` = case_when(`3` == 0 & is.na(target_taxa_present) ~ 1e9,
                           TRUE ~ `3`)) %>%
    filter(`1` < 1e9) %>% # filter out false zeros
    filter(`2` < 1e9) %>%
    filter(`3` < 1e9) %>%
    mutate(increased = case_when(`3` > `1` ~ "no depletion",
                                 TRUE ~ "depletion")) %>% 
    mutate(site_int = as.factor(row_number())) #sample identifier
  
  write_csv(three_pass_data_wide_total_fish, file = here("data/three_pass_data_wide_total_fish.csv"))
  return(here("data/three_pass_data_wide_total_fish.csv"))
}
