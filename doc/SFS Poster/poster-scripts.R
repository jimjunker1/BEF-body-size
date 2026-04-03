library(tidyverse)
library(gridExtra)
library(ggpubr)
# code for SFS poster

fig1_plot_a = readRDS(file = here('doc/plots/b_s_pred_plot.rds'))
fig1_plot_b = readRDS(file = here('doc/plots/b_m_pred_plot.rds'))

fig1_a = fig1_plot_a 
fig1_b = fig1_plot_b 

# fig1_agg = ggpubr::ggarrange(fig1_a, fig1_b, nrow = 1, align = 'hv')


ggsave(here('doc/plots/figure1a.svg'),
       fig1_a,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
       )

ggsave(here('doc/plots/figure1b.svg'),
       fig1_b,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

fig2 = readRDS(here('doc/plots/n_m_theory_plot.rds'))

ggsave(here('doc/plots/figure2.svg'),
       fig2,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

fig3 = readRDS(here('doc/plots/b_lambda_pred_plot.rds'))
ggsave(here('doc/plots/figure3.svg'),
       fig3,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


box1 = readRDS(here("doc/plots/nm_extrapolate_plot.rds"))
ggsave(here('doc/plots/boxfigure.svg'),
       box1,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


## University of Alabama talk
folder_path = "C:/Users/jj0895/OneDrive - UNT System/Office/Talks/University of Alabama/"
b_s_plot = readRDS(here('doc/plots/b_s_pred_plot.rds'))

ggsave(plot = b_s_plot,
       filename = 'b_s_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


b_m_plot = readRDS(here('doc/plots/b_m_pred_plot.rds'))

ggsave(plot = b_m_plot,
       filename = 'b_m_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

b_m_n_plot = readRDS(here('doc/plots/b_m_n_pred_plot.rds'))

ggsave(plot = b_m_n_plot,
       filename = 'b_m_n_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

nm_theory_plot = readRDS(here('doc/plots/n_m_theory_plot.rds'))

ggsave(plot = nm_theory_plot,
       filename = 'nm_theory_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


nm_plot = readRDS(here('doc/plots/nm_plot.rds'))
ggsave(plot = nm_plot,
       filename = 'nm_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

m_sigma_s_plot = readRDS(here('doc/plots/m_sigma_s_plot.rds'))
ggsave(plot = m_sigma_s_plot,
       filename = 'm_sigma_s_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

lambda_m_sigma_plot = readRDS(here('doc/plots/lambda_m_sigma_plot.rds'))
ggsave(plot = lambda_m_sigma_plot,
       filename = 'lambda_m_sigma_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


s_lambda_plot = readRDS(here('doc/plots/s_lambda_plot.rds'))
ggsave(plot = s_lambda_plot,
       filename = 's_lambda_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

resid_s_plot = readRDS(here('doc/plots/resid_s_plot.rds'))
ggsave(plot = resid_s_plot,
       filename = 'resid_s_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


resid_lambda_plot = readRDS(here('doc/plots/resid_lambda_plot.rds'))
ggsave(plot = resid_lambda_plot,
       filename = 'resid_lambda_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)


lambda_n_plot = 
  dat %>%
  ggplot()+
  geom_point(aes(x = lambda_med, y = ln_tot, fill = site_id), size = 1, shape = 21, alpha = 0.5)+
  geom_point(data = dat_summ, aes(x = lambda_med, y = ln_tot, fill = site_id), size = 3, shape = 21)+
  scale_fill_viridis(discrete = TRUE)+
  scale_x_continuous(name = "Food web efficiency ( \u03BB )",
                     limits = c(-3, -1))+
  scale_y_continuous(name = expression('Abundance (individuals'~m^-2~")"))+
  theme(legend.position = 'none')

ggsave(plot = lambda_n_plot,
       filename = 'lambda_n_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)

lambda_m_plot = 
  dat %>%
  ggplot()+
  geom_point(aes(x = lambda_med, y = lmbar_b_est, fill = site_id), size = 1, shape = 21, alpha = 0.5)+
  geom_point(data = dat_summ, aes(x = lambda_med, y = lmbar_b_est, fill = site_id), size = 3, shape = 21)+
  scale_fill_viridis(discrete = TRUE)+
  scale_x_continuous(name = "Food web efficiency ( \u03BB )",
                     limits = c(-3, -1))+
  scale_y_continuous(name = expression(bar(M)[" "]*"(mg "*ind.^-1*")"))+
  theme(legend.position = 'none')

ggsave(plot = lambda_m_plot,
       filename = 'lambda_m_plot.svg',
       path = folder_path,
       device = 'svg',
       width = 8,
       height = 8, units = 'in',
       scaling = 2
)
