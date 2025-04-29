
here::i_am("R/01_wrangle-data.R")
## The last time the data stores where updated
cat("Data stores were updated",as.character(readRDS(here("data/date_updated.rds"))))
## should we update all of the data stores?
update = FALSE
source(here::here("R/helpers.R"))
 
# get sample and site level biodiversity data
#### Start with fish to adjust abundances
if(update){
# 1) load the stores
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

# 2) wrangle and summarize the wetted widths measures
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
# 3) wrangle the events level data

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

# 4) wrangle the field data
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
  select(eventID, passStartTime, passNumber, siteID, namedLocation) %>%
  collect() %>% 
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time)))%>% 
  left_join(reach_event) %>%
  rename(pass = pass_number) %>% 
  group_by(site_id, collect_date, pass, event_id, named_location) %>%
  tally()

fish_measure_taxa = fish_ind_load %>%
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
# all fish after the first 50 are bulk counted. Those values are here.
fish_bulk = fish_count_load %>%
  select(eventID, bulkFishCount, passStartTime, passNumber, siteID, namedLocation)  %>%
  collect() %>% 
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time))) %>% 
  left_join(reach_event) %>%
  rename(pass = pass_number) %>%
  mutate(n = as.integer(bulk_fish_count)) %>% 
  select(-pass_start_time)

fish_bulk_taxa = fish_count_load %>%
  select(eventID, taxonID, bulkFishCount, passStartTime, passNumber, siteID, namedLocation)  %>%
  collect() %>% 
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time)))

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
}


### Macroinvertebrates ----
# load the macro data
neonstore::neon_store(
  product = "DP1.20120.001",
  table = "inv_taxonomyProcessed-basic",
  dir = NEON_db_dir,
  db = neon_db(NEON_db_dir, read_only = FALSE)
)
macro_load <- neon_table(
  product = "DP1.20120.001",
  db = neon_db(NEON_db_dir),
  table = "inv_taxonomyProcessed-basic",
  lazy = TRUE
)

macro <- macro_load %>%
  select(siteID, collectDate = collectDate, scientificName, taxonID = acceptedTaxonID, estimatedTotalCount) %>%
  collect() %>% 
  clean_names() %>% 
  mutate(type = 'inverts',
         collect_date = as.Date(collect_date)) %>% 
  summarise(estimated_total_count = sum(estimated_total_count),
            .by = c('site_id','collect_date','type','taxon_id')
            )
  
##### Fish -----
# load the fish data
neonstore::neon_store(
  product = "DP1.20107.001",
  dir = NEON_db_dir,
  db = neon_db(NEON_db_dir, read_only = FALSE)
)

fish_load = neon_table(
  product = "DP1.20107.001",
  table = "fsh_bulkCount-basic",
  db = neon_db(NEON_db_dir),
  lazy = TRUE
  )

fish = fish_load %>% 
  select(siteID, namedLocation, eventID, collectDate = passStartTime, passNumber, taxonID, bulkFishCount, actualOrEstimated) %>%
  collect() %>% 
  mutate(type = 'fish',
         collectDate = as.Date(gsub("(^\\d{4}-\\d{2}-\\d{2}) .*", "\\1", collectDate))) %>% 
  summarise(estimatedTotalCount = sum(bulkFishCount), .by = c('siteID','collectDate','type','taxonID'))

## stream widths load



field_load = neon_table(
  product = "DP1.20107.001",
  table = "fsh_fieldData-basic",
  db = neon_db(NEON_db_dir, read_only = FALSE),
  lazy = TRUE
)
reach_load = neon_table(
  product = "DP1.20107.001",
  table = "fsh_perPass-basic",
  db = neon_db(NEON_db_dir, read_only = FALSE),
  lazy = TRUE
  )

widths_load = neon_table(
  product = "DP1.20190.001",
  table = "rea_widthFieldData",
  db = neon_db(NEON_db_dir),
  lazy = TRUE
)

## 
reach_event = reach_load %>% 
  select(siteID, eventID, namedLocation) %>%
  distinct() %>%
  collect()

field_event = field_load %>% 
  select(siteID, eventID, namedLocation, fixedRandomReach, measuredReachLength) %>% 
  collect()

widths = widths_load %>% 
  # select(siteID, collectDate, wettedWidth) %>% 
  collect() #%>% 
  mutate(year = year(collectDate),
         month = month(collectDate),
         year_month = paste(year,month, sep = "_")) %>% 
  group_by(siteID) %>%
  summarize(mean_wetted_width_m = mean(wettedWidth, na.rm = T),
            sd_wetted_width_m = sd(wettedWidth, na.rm = T))
#### Combine fish and macros ----
# unique sampling date IDs for macros
macro_id = macro %>%
  distinct(siteID, collectDate) %>% 
  arrange(siteID, collectDate) %>% 
  mutate(macroID = 1:n())
# unique sampling date IDs for fish
fish_id = fish %>% 
  distinct(siteID, collectDate) %>% 
  arrange(siteID, collectDate) %>% 
  mutate(fishID = 1:n())

#

macro_merge_id = merge_macrofish_dates(mDf = "macro_id", fDf = "fish_id", limit = 30) %>% 
  bind_rows() %>% filter(!is.na(fishID)) %>% 
  select(siteID, mDate = collectDate, fDate) %>% 
  mutate(id = 1:n()) %>% 
  left_join(.,macro %>% 
              select(siteID, mDate = collectDate, type, taxonID, estimatedTotalCount),
            by = c('siteID','mDate'))
  
fish_merge_id = merge_macrofish_dates(mDf = "macro_id", fDf = "fish_id", limit = 30) %>% 
  bind_rows() %>% filter(!is.na(fishID)) %>% 
  select(siteID, mDate = collectDate, fDate) %>% 
  mutate(id = 1:n()) %>%  
  left_join(.,fish %>%
              select(siteID, fDate = collectDate, type, taxonID, estimatedTotalCount),
            by = c('siteID','fDate'))

macro_fish_n = macro_merge_id %>% 
  bind_rows(fish_merge_id)

### create sample and site level biodiversity measures



  
  
  H_dat$hill_0 = hill_taxa(H_dat %>% select(-c(site_id, collectDate)), q = 0)


###

sampleParams <- readRDS(here("data/dat_clauset_xmins.rds")) %>% ungroup %>% 
  select(site_id, sample_id, year, xmin, xmin_c = xmin_clauset, xmax, gpp, gpp_sd, mean_om, sd_om, mat = mean, dw, no_m2) %>% 
  mutate(b = dw * no_m2) %>% 
  mutate(dw_mean = weighted.mean(dw, no_m2), .by = c(site_id, sample_id)) %>% 
  summarise(no_m2 = sum(no_m2),
            b = sum(b), .by = c(site_id, sample_id, year, dw_mean, xmin, xmin_c, xmax, gpp, gpp_sd, mean_om, sd_om, mat))

lambdas <- readRDS(here("data/lambdas.rds"))

ARIKLambdas = lambdas %>% 
  filter(site_id == 'ARIK',
         year == 2016) %>% 
  .[c(2,1),]

ARIKParams = sampleParams %>% 
  filter(site_id == 'ARIK',
         year == 2016)

dat = bind_cols(ARIKLambdas, ARIKParams) %>% 
  select(site_id = site_id...6, year = year...5,
         .epred, .lower, .upper, .width, .point, .interval,
         dw_mean, xmin, xmin_c, xmax, no_m2, b) %>% 
  mutate(m_old = pmap_dbl(.,~pareto_expectation(lambda = ..3,
                                                xmin = ..10,
                                                xmax = ..12)),
         m_est = pmap_dbl(.,~pareto_expectation(lambda = ..3,
                                                xmin = 0.0006,
                                                xmax = ..12)),
         m_est_l = pmap_dbl(.,~pareto_expectation(lambda = ..4,
                                                  xmin = 0.0006,
                                                  xmax = ..12)),
         m_est_u = pmap_dbl(.,~pareto_expectation(lambda = ..5,
                                                  xmin = 0.0006,
                                                  xmax = ..12)),
         n_est = pmap_dbl(.,~estimate_pareto_N(n = ..13,
                                         lambda = ..3,
                                         xmin = ..10,
                                         xmin2 = 0.0006,
                                         xmax = ..12)),
         n_est_l = pmap_dbl(.,~estimate_pareto_N(n = ..13,
                                                 lambda = ..4,
                                                 xmin = ..10,
                                                 xmin2 = 0.0006,
                                                 xmax = ..12)),
         n_est_u = pmap_dbl(.,~estimate_pareto_N(n = ..13,
                                                 lambda = ..5,
                                                 xmin = ..10,
                                                 xmin2 = 0.0006,
                                                 xmax = ..12)),
         b_old = m_old * no_m2,
         b_est = m_est * n_est,
         b_est_l = m_est_l * n_est_l,
         b_est_u = m_est_u * n_est_u,
         b_diff = b_est - b,
         b_diff_l = b_est_l - b,
         b_diff_u = b_est_u -b)

sum(isdbayes::rparetocounts(96633, lambda = -2.01, xmin = 0.0006, xmax = 30840))

# calculate the 
# debugonce(pareto_expectation)
pareto_expectation(lambda = dat$.epred[1],
                   xmin = dat$xmin[1],
                   xmax = dat$xmax[1])

m_est = pareto_expectation(lambda = dat$.epred[1],
                   xmin = 0.0006,
                   xmax = dat$xmax[1])

n_est = estimate_pareto_N(n = dat$no_m2[1],
                  lambda = dat$.epred[1],
                  xmin = dat$xmin[1],
                  xmin2 = 0.0006,
                  xmax = dat$xmax[1])
dat$no_m2[1]
dat$b[1]

n_est * m_est
