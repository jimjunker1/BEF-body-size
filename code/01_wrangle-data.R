
i_am("code/01_wrangle-data.R")
source(here("code/helpers.R"))

sampleParams <- readRDS(here("data/dat_clauset_xmins.rds")) %>% ungroup %>% 
  select(site_id, sample_id, year, xmin, xmin_c = xmin_clauset, xmax, gpp, gpp_sd, mean_om, sd_om, mat = mean, dw, no_m2) %>% 
  mutate(b = dw * no_m2) %>% 
  mutate(dw_mean = weighted.mean(dw, no_m2), .by = c(site_id, sample_id)) %>% 
  summarise(no_m2 = sum(no_m2),
            b = sum(b), .by = c(site_id, sample_id, year, dw_mean, xmin, xmin_c, xmax, gpp, gpp_sd, mean_om, sd_om, mat))

lambdas <- readRDS(here("data/lambdas.rds"))

dat = left_join(lambdas, sampleParams, by = "sample_id")

# calculate the 