# libraries & functions ---------------------------------------------------

# libraries
library(here)
library(Cairo)
library(ggplot2)
library(extrafont)
library(readxl)
library(data.table)
library(parallel) # for parallel computing (speed-up)
library(mgcv) # for bam()
library(AICcmodavg) # for aictabCustom()
library(patchwork) # for wrap_plots()
library(gratia) # for appraise() & draw()
library(plotrix)
library(emmeans)
# library(itsadug) # complex interaction plots for gam using plot_smooth()
# # gammSlice - https://cran.r-project.org/web/packages/gammSlice/index.html

# functions

# ggplot style
sgg <- function(fs = 16) {
  theme(text = element_text(family = 'Times New Roman', size = fs),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black'),
        strip.text.x = element_text(size = fs),
        axis.text = element_text(size = fs),
        axis.title = element_text(size = fs),
        legend.title = element_text(size = fs),
        legend.text = element_text(size = fs),
        complete = FALSE)
}

# remove space between panels
squeeze_panels <- theme(panel.spacing = grid::unit(0, 'lines')) 

