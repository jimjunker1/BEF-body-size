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

