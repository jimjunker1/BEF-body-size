here::i_am("code/01_wrangle-data.R")
source(here::here("code/helpers.R"))
 
# get sample and site level biodiversity data
# local directory location
NEON_db_dir = sprintf("C:/Users/%s/OneDrive - UNT System/Projects/database-files",
                      Sys.info()[['user']])

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
  mutate(type = 'inverts',
         collectDate = as.Date(collectDate)) %>% 
  summarise(estimatedTotalCount = sum(estimatedTotalCount),.by = c('siteID','collectDate','type','taxonID'))
  
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
  collect() #%>% 
  mutate(type = 'fish',
         collectDate = as.Date(gsub("(^\\d{4}-\\d{2}-\\d{2}) .*", "\\1", collectDate))) %>% 
  summarise(estimatedTotalCount = sum(bulkFishCount), .by = c('siteID','collectDate','type','taxonID'))

## stream widths load

neon_store(
  product = "DP1.20190.001",
  table = "rea_widthFieldData",
  dir = NEON_db_dir,
  db = neon_db(NEON_db_dir, read_only = FALSE)
)

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
