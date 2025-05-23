
# download NEON data for three-pass fish eclectroshocking, wrangle it. The resulting data
# is used to estimate population size with three-pass depletion models.
# Modeling occurs in a different script.

# stack data
if(update){
  neonstore::neon_store(
    product = "DP1.20107.001",
    dir = NEON_db_dir 
  )
  neon_store(
    product = "DP1.20190.001",
    table = "rea_widthFieldData",
    dir = NEON_db_dir,
    db = neon_db(NEON_db_dir, read_only = FALSE)
  )  


fish_stacked = stackFromStore(filepaths=NEON_db_dir,
                      dpID="DP1.20107.001",
                      package="basic",
                      site = streamsites)

stream_widths_stacked = stackFromStore(filepaths=NEON_db_dir,
                               dpID="DP1.20190.001",
                               package="basic",
                               site = streamsites)

saveRDS(fish_stacked, file = here("data/fish_stacked.rds"))
saveRDS(stream_widths_stacked, file = here("data/stream_widths_stacked.rds"))
}

# get reach lengths and widths --------------------------------------------
stream_widths_stacked = readRDS(here("data/stream_widths_stacked.rds"))

# mean wetted width. Combine with reach lengths to get sampling area later.
mean_wetted_width = stream_widths_stacked$rea_widthFieldData %>%
  clean_names() %>%
  select(site_id, collect_date, wetted_width) %>%
  mutate(year = year(collect_date),
         month = month(collect_date),
         year_month = paste(year,month, sep = "_")) %>%
  group_by(site_id) %>%
  summarize(mean_wetted_width_m = mean(wetted_width, na.rm = T),
            sd_wetted_width_m = sd(wetted_width, na.rm = T))

saveRDS(mean_wetted_width, file = here("data/mean_wetted_width.rds"))

# TOTAL POPULATION wrangle data ------------------
fish <- readRDS(here("data/fish_stacked.rds"))

# 1) get reachids, eventids
reach_event = fish$fsh_perPass %>%
  select(namedLocation, eventID) %>%
  distinct()  %>% # removes duplicates. JSW confirmed that these were true duplicates on 2023-03-01
  clean_names()

# 2) get info on whether reaches are fixed or random. Only fixed reaches have 3 pass removal. Random reaches are all single pass
fixed_random_reach = fish$fsh_fieldData %>%
  distinct(namedLocation, fixedRandomReach) %>%
  filter(fixedRandomReach == "fixed") %>% # fix this typo. Confirmed by email with NEON on 2023-03-01
  clean_names()

# 3) get reach lengths
fish_reach_length = fish$fsh_fieldData %>%
  clean_names() %>% glimpse() %>% 
  mutate(collect_date = start_date) %>% 
  distinct(named_location, event_id, measured_reach_length, collect_date, site_id) %>%
  group_by(collect_date, site_id, named_location) %>%
  add_tally() %>%
  filter(n == 1)   # filters duplicate reach lengths

# The next two steps combine two datasets. These are needed to obtain the number of fish per pass. Either alone would be
# an inaccurate number.
# 4) Get fish abundance from fsh_perFish. The first 50 fish are measured for length. They are here in fsh_perFish
fish_measures = fish$fsh_perFish %>%
  select(eventID, taxonID, passStartTime, passNumber, siteID, namedLocation) %>%
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time)))%>% 
  left_join(reach_event) %>%
  rename(pass = pass_number) %>% 
  group_by(site_id, collect_date, pass, event_id, named_location) %>%
  tally()

# 5) all fish after the first 50 are bulk counted. Those values are here.
fish_bulk = fish$fsh_bulkCount %>%
  select(eventID, taxonID, bulkFishCount, passStartTime, passNumber, siteID, namedLocation)  %>%
  clean_names() %>%
    mutate(collect_date = ymd(as.Date(pass_start_time))) %>% 
    left_join(reach_event) %>%
    rename(pass = pass_number) %>%
  mutate(n = as.integer(bulk_fish_count)) %>% 
  select(-pass_start_time)

# 6) combine the 1st 50 fish with the bulk counts to get a total number of fish per pass.
#    Then add identifying information

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

# 7) get reachids, eventids, and targets (i.e., passes that returned zero fish will have "N")
true_zeros = fish$fsh_perPass %>%
  select(siteID, namedLocation, passNumber, eventID, targetTaxaPresent, passStartTime) %>%
  mutate(collect_date = ymd(as.Date(passStartTime))) %>% 
  select(-passStartTime) %>% 
  distinct()  %>% # removes duplicates. JSW confirmed that these were true duplicates on 2023-03-01
  clean_names() %>%
  filter(target_taxa_present == "N")

# 8) make wide format. Replace 0's using information in fsh_perPass$target_taxa_present
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
  mutate(site_int = as.factor(row_number())) #%>%  #sample identifier
  # filter(!is.na(collect_date))

write_csv(three_pass_data_wide_total_fish, file = here("data/three_pass_data_wide_total_fish.csv"))
