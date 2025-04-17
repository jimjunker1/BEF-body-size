
here::i_am("code/01_wrangle-data.R")
source(here::here("code/helpers.R"))

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
