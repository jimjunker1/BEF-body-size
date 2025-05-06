library(tidyverse)
library(gridExtra)
library(ggpubr)
# code for SFS poster

fig1_plot_a = readRDS(file = here('doc/plots/b_s_pred_plot.rds'))
fig1_plot_b = readRDS(file = here('doc/plots/b_m_pred_plot.rds'))

fig1_a = fig1_plot_a +
  theme(plot.margin = unit(c(0.2,0,0.2,0), 'lines'))
fig1_b = fig1_plot_b +
  theme(plot.margin = unit(c(0.2,0,0.2,0), 'lines'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

fig1_agg = ggpubr::ggarrange(fig1_a, fig1_b, nrow = 1, align = 'hv')


ggsave(here('doc/plots/figure1.svg'),
       fig1_agg,
       device = 'svg',
       width = 14,
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
