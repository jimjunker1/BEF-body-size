
here::i_am("R/01_wrangle-data.R")
## The last time the data stores where updated
cat("Data stores were updated",as.character(readRDS(here::here("data/date_updated.rds"))))
## should we update all of the data stores?
update = FALSE
rerun = TRUE
source(here::here("R/helpers.R"))
 
# get sample and site level biodiversity data
#### Start with fish to adjust abundances
if(rerun){
gert::git_pull()
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
  select(siteID, eventID, passStartTime, namedLocation, taxonID) %>%
  collect() %>% 
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time)),
         month = month(collect_date),
         year = year(collect_date),
         year_month = paste(year, month, sep = "_")) %>% 
  select(-pass_start_time) %>% 
  summarise(count = n(),.by = c('site_id','event_id','taxon_id','year_month'))

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
  select(siteID, namedLocation, eventID, taxonID, bulkFishCount, passStartTime)  %>%
  collect() %>% 
  clean_names() %>%
  mutate(collect_date = ymd(as.Date(pass_start_time)),
         month = month(collect_date),
         year = year(collect_date),
         year_month = paste(year, month, sep = "_")) %>% 
  summarise(count = sum(bulk_fish_count), .by = c('site_id','event_id','taxon_id','year_month'))

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

three_pass_population = as_draws_df(three_pass_model@stanfit) %>% 
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
  left_join(three_pass_data_wide %>% ungroup %>% 
              mutate(site_int = as.factor(site_int))) %>% 
  mutate(area_m2 = measured_reach_length * mean_wetted_width_m,
         no_fish_per_m2 = pop_threepass/area_m2,
         no_fish_per_m2_lower = .lower_threepass/area_m2,
         no_fish_per_m2_upper = .upper_threepass/area_m2,
         raw_total_per_m2 = total_fish/area_m2) 

fish_count_taxa = fish_measure_taxa %>% 
  left_join(fish_bulk_taxa, by = c('site_id','event_id','taxon_id','year_month')) %>% 
  mutate(count = sum(across(matches('count.')), na.rm = TRUE),.by = c('site_id','event_id','taxon_id','year_month')) %>% 
  select(-count.x, -count.y) %>% 
  mutate(rel_n = count/sum(count), .by = c('site_id','event_id','year_month')) %>% 
  left_join(three_pass_population %>% 
              filter(!is.na(area_m2)) %>% 
              select(site_id, year_month, no_fish_per_m2, no_fish_per_m2_lower,
                     no_fish_per_m2_upper, raw_total_per_m2, area_m2) %>% 
              summarise(no_fish_per_m2 = mean(no_fish_per_m2),
                        no_fish_per_m2_lower = mean(no_fish_per_m2_lower),
                        no_fish_per_m2_upper = mean(no_fish_per_m2_upper),
                        raw_no_fish_per_m2 = mean(raw_total_per_m2),
                        area_m2 = sum(area_m2),
                        .by = c('site_id','year_month')),
            by = c('site_id','year_month')) %>% 
  mutate(no_fish_per_m2 = no_fish_per_m2 * rel_n,
         no_fish_per_m2_lower = no_fish_per_m2_lower * rel_n,
         no_fish_per_m2_upper = no_fish_per_m2_upper * rel_n)

saveRDS(fish_count_taxa, here('data/fish_count_taxa.rds'))

} else{
  fish_count_taxa = readRDS(here('data/fish_count_taxa.rds'))
}

### Macroinvertebrates ----
# load the macro data
if(rerun){
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

macro_count_taxa <- macro_load %>%
  select(siteID, collectDate = collectDate, scientificName, taxonID = acceptedTaxonID, estimatedTotalCount) %>%
  collect() %>% 
  clean_names() %>% 
  filter(!is.na(estimated_total_count),
         !is.na(taxon_id)) %>% 
  mutate(collect_date = as.Date(collect_date),
         month = month(collect_date),
         year = year(collect_date),
         year_month = paste(year, month, sep= "_")) %>% 
  summarise(count = sum(estimated_total_count, na.rm = TRUE),
            .by = c('site_id','year_month','taxon_id')
            )

# vector of samples for lambdas
dat_2022_clauset = readRDS(here('data/dat_2022_clauset.rds')) 
sample_site_year_vec = dat_2022_clauset %>% 
  ungroup %>% 
  select(site_id, sample_id, collect_date) %>% 
  mutate(collect_date = as.Date(collect_date),
         year = year(collect_date),
         month = month(collect_date),
         year_month = paste(year, month, sep = "_"),
         site_year = paste(site_id, year_month, sep = '_')) %>% 
  select(site_year) %>% unlist %>% unique

# df to merge samples for lambdas
sample_site_year_df = dat_2022_clauset %>% 
  ungroup %>% 
  select(site_id, sample_id, collect_date) %>% 
  mutate(collect_date = as.Date(collect_date),
         year = year(collect_date),
         month = month(collect_date),
         year_month = paste(year, month, sep = "_"),
         site_year = paste(site_id, year_month, sep = '_')) %>% 
  select(site_id, sample_id, site_year) %>% 
  distinct()

macro_site_year_vec = macro_count_taxa %>%
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  select(site_year) %>% unlist %>% unique

macro_samples = intersect(macro_site_year_vec, sample_site_year_vec)

fish_site_year_vec = fish_count_taxa %>% 
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  select(site_year) %>% unlist %>% unique

fish_samples = intersect(fish_site_year_vec, sample_site_year_vec)

fish_count_taxa_samples =fish_count_taxa %>% 
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  filter(site_year %in% fish_samples) %>% 
  left_join(sample_site_year_df, by = c('site_id','site_year')) %>% 
  select(-site_year) %>% 
  filter(!is.na(no_fish_per_m2))

macro_count_taxa_samples = macro_count_taxa %>% 
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  filter(site_year %in% macro_samples) %>% 
  left_join(sample_site_year_df, by = c('site_id','site_year')) %>% 
  select(-site_year) %>% 
  filter(!is.na(count))

macro_fish_count_taxa_samples = macro_count_taxa_samples %>% 
  rename(count_m2 = count) %>% 
  bind_rows(fish_count_taxa_samples %>%
              select(site_id, year_month, taxon_id, count_m2 = no_fish_per_m2))

common_site_year = intersect(macro_site_year_vec, fish_site_year_vec)
fish_count_taxa =fish_count_taxa %>% 
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  filter(site_year %in% common_site_year) %>% 
  select(-site_year) %>% 
  filter(!is.na(no_fish_per_m2))

macro_count_taxa = macro_count_taxa %>% 
  mutate(site_year = paste(site_id, year_month, sep = "_")) %>% 
  filter(site_year %in% common_site_year) %>% 
  select(-site_year) %>% 
  filter(!is.na(count))

macro_fish_count_taxa = macro_count_taxa %>% 
  rename(count_m2 = count) %>% 
  bind_rows(fish_count_taxa %>%
              select(site_id, year_month, taxon_id, count_m2 = no_fish_per_m2))


saveRDS(macro_fish_count_taxa, here("data/macro_fish_count_taxa.rds"))
saveRDS(macro_fish_count_taxa_samples, here('data/macro_fish_count_taxa_samples.rds'))
} else{
  macro_fish_count_taxa = readRDS(here("data/macro_fish_count_taxa.rds"))
}

## create sample and site level biodiversity measures
if(rerun){
# create a species list to isolate wide format actions
taxa_list = macro_fish_count_taxa_samples %>% 
  select(taxon_id) %>% 
  unlist %>% unique
  
macro_fish_count_wide = macro_fish_count_taxa_samples %>% 
  ungroup %>% 
  pivot_wider(id_cols = c(site_id, sample_id),
              names_from = taxon_id, values_from = count_m2,
              values_fn = sum, values_fill = 0)

macro_fish_density_wide = macro_fish_count_taxa_samples %>% 
  ungroup %>% 
  mutate(den = round(count_m2 * (1/min(count_m2))),
         .by = c('site_id','sample_id')) %>% 
  pivot_wider(id_cols = c(site_id, sample_id),
              names_from = taxon_id, values_from = den,
              values_fn = sum, values_fill = 0)

# Species biodiversity data sets
H_dat = macro_fish_density_wide
H_dat = H_dat %>% left_join(
  H_dat %>% distinct(site_id) %>% bind_cols(
    specpool(H_dat %>%
               select(all_of(taxa_list)),
             pool = H_dat %>% select(site_id) %>% unlist,
             smallsample = TRUE)
    ), by = c('site_id')) %>% 
  left_join(H_dat %>% select( site_id, sample_id) %>% bind_cols(
    macro_fish_density_wide %>% select(all_of(taxa_list)) %>% 
      apply(., 1, estimateR) %>% t
  ), by = c('site_id','sample_id')
  ) 
H_dat$hill_0 = hill_taxa(H_dat %>% select(all_of(taxa_list)), q = 0)
H_dat$hill_1 = hill_taxa(H_dat %>% select(all_of(taxa_list)), q = 1)
H_dat$hill_2 = hill_taxa(H_dat %>% select(all_of(taxa_list)), q = 2)

saveRDS(H_dat, here('data/macro_fish_diversity.rds'))
saveRDS(taxa_list, here('data/taxa_list.rds'))
# x = readRDS(here('data/x.rds'))+1
# system(
#   paste0(
#     'git add -A && git commit -m "rerun #',x,'" && git push'
#   )
# )
# commit_mess = paste0("rerun commit #",x)
# map(gert::git_status()[gert::git_status()$status == 'new', 'file'] %>% unlist, ~gert::git_add(.x))
# gert::git_commit_all(commit_mess)
# gert::git_push()
} else{
  H_dat = readRDS(here('data/macro_fish_diversity.rds'))

  H_summ = H_dat %>% 
    summarise(S_obs_inc = mean(Species),
              S_chao_inc = mean(chao),
              S_jack1_inc = mean(jack1),
              S_jack2_inc = mean(jack2),
              S_boot_inc = mean(boot),
              S_obs_rar = mean(S.obs),
              S_chao1_rar = mean(S.chao1),
              S_ACE_rar = mean(S.ACE, na.rm = TRUE),
              hill_0 = mean(hill_0),
              hill_1 = mean(hill_1),
              hill_2 = mean(hill_2),
              .by = c('site_id')
              ) %>% 
    mutate(across(where(is.numeric), round))

}

###
pareto_variance <- function(lambda, x_min, x_max, tol = 1e-6) {
  
  if (abs(lambda + 1) < tol || abs(lambda + 2) < tol || abs(lambda + 3) < tol) {
    stop("lambda is too close to a special case (-1, -2, -3). Explicit special-case handling needed.")
  }
  
  E_M <- ((lambda + 1) / (lambda + 2)) * 
    (x_max^(lambda + 2) - x_min^(lambda + 2)) /
    (x_max^(lambda + 1) - x_min^(lambda + 1))
  
  E_M2 <- ((lambda + 1) / (lambda + 3)) * 
    (x_max^(lambda + 3) - x_min^(lambda + 3)) /
    (x_max^(lambda + 1) - x_min^(lambda + 1))
  
  var_M <- E_M2 - E_M^2
  
  return(var_M)
}

# Example explicitly tested
lambda_values <- c(-1.01, -1.0001, -2.001, -2.000001, -3.1)
sapply(lambda_values, function(lam) pareto_variance(lam, 1, 10))


lambda <- -1.5
x_min <- 1
x_max <- 10
N_total <- 1000

# Calculate normalization constant c explicitly
c <- N_total * (lambda + 1) / (x_max^(lambda + 1) - x_min^(lambda + 1))

# Explicit analytical function to calculate Var(N)
calc_abundance_variance <- function(lambda, c, x_min, x_max) {
  term1 <- (lambda + 1) / (2 * lambda + 1)
  numerator <- x_max^(2 * lambda + 1) - x_min^(2 * lambda + 1)
  denominator <- x_max^(lambda + 1) - x_min^(lambda + 1)
  varN <- c^2 * (term1 * numerator / denominator - 1)
  return(varN)
}

# Compute Var(N) explicitly
variance_N <- calc_abundance_variance(lambda, c, x_min, x_max)

cat("Explicitly calculated variance in abundances (N):", variance_N, "\n")



## extrapolate the es
sampleParams <- readRDS(here("data/dat_clauset_xmins.rds")) %>% ungroup %>%
  select(site_id, sample_id, year, xmin, xmin_c = xmin_clauset, xmax, gpp, gpp_sd, mean_om, sd_om, mat = mean, dw, no_m2) %>%
  mutate(b = dw * no_m2) %>%
  mutate(dw_mean = weighted.mean(dw, no_m2), .by = c(site_id, sample_id)) %>%
  summarise(no_m2 = sum(no_m2),
            b = sum(b), .by = c(site_id, sample_id, year, dw_mean, xmin, xmin_c, xmax, gpp, gpp_sd, mean_om, sd_om, mat))

lambdas <- readRDS(here("data/lambdas.rds"))

# ARIKLambdas = lambdas %>% 
#   filter(site_id == 'ARIK',
#          year == 2016) %>% 
#   .[c(2,1),]
# 
# ARIKParams = sampleParams %>% 
#   filter(site_id == 'ARIK',
#          year == 2016)
# 
# dat = bind_cols(ARIKLambdas, ARIKParams) %>% 
#   select(site_id = site_id...6, year = year...5,
#          .epred, .lower, .upper, .width, .point, .interval,
#          dw_mean, xmin, xmin_c, xmax, no_m2, b) %>% 
#   mutate(m_old = pmap_dbl(.,~pareto_expectation(lambda = ..3,
#                                                 xmin = ..10,
#                                                 xmax = ..12)),
#          m_est = pmap_dbl(.,~pareto_expectation(lambda = ..3,
#                                                 xmin = 0.0006,
#                                                 xmax = ..12)),
#          m_est_l = pmap_dbl(.,~pareto_expectation(lambda = ..4,
#                                                   xmin = 0.0006,
#                                                   xmax = ..12)),
#          m_est_u = pmap_dbl(.,~pareto_expectation(lambda = ..5,
#                                                   xmin = 0.0006,
#                                                   xmax = ..12)),
#          n_est = pmap_dbl(.,~estimate_pareto_N(n = ..13,
#                                          lambda = ..3,
#                                          xmin = ..10,
#                                          xmin2 = 0.0006,
#                                          xmax = ..12)),
#          n_est_l = pmap_dbl(.,~estimate_pareto_N(n = ..13,
#                                                  lambda = ..4,
#                                                  xmin = ..10,
#                                                  xmin2 = 0.0006,
#                                                  xmax = ..12)),
#          n_est_u = pmap_dbl(.,~estimate_pareto_N(n = ..13,
#                                                  lambda = ..5,
#                                                  xmin = ..10,
#                                                  xmin2 = 0.0006,
#                                                  xmax = ..12)),
#          b_old = m_old * no_m2,
#          b_est = m_est * n_est,
#          b_est_l = m_est_l * n_est_l,
#          b_est_u = m_est_u * n_est_u,
#          b_diff = b_est - b,
#          b_diff_l = b_est_l - b,
#          b_diff_u = b_est_u -b)
# 
# sum(isdbayes::rparetocounts(96633, lambda = -2.01, xmin = 0.0006, xmax = 30840))
# 
# # calculate the 
# # debugonce(pareto_expectation)
# pareto_expectation(lambda = dat$.epred[1],
#                    xmin = dat$xmin[1],
#                    xmax = dat$xmax[1])
# 
# m_est = pareto_expectation(lambda = dat$.epred[1],
#                    xmin = 0.0006,
#                    xmax = dat$xmax[1])
# 
# n_est = estimate_pareto_N(n = dat$no_m2[1],
#                   lambda = dat$.epred[1],
#                   xmin = dat$xmin[1],
#                   xmin2 = 0.0006,
#                   xmax = dat$xmax[1])
# dat$no_m2[1]
# dat$b[1]
# 
# n_est * m_est
