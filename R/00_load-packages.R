# load packages -----------------------------------------------------------

# be sure to check for packages conflicts!
# import
library(readxl) # read excel files
library(janitor) # simple tools to clean dirty data
library(here) # a simpler way to find your files
library(SWMPr) # working with SWMP data from the NERRS
library(SWMPrExtension) # expanded functions for SWMPr package

# tidy and wrangle
library(tidyverse) # because...tidyverse (ggplot2, tidyr, dplyr)
library(lubridate) # dates and times

# stats
library(broom) # convert statistical analysis objects into tidy tibbles
library(psych) # has expanded statistical functions

# markdown
library(knitr)
library(rmarkdown)
library(kableExtra)

# expanded data visualization
library(cowplot) # additional themes for ggplot2
library(gridExtra) # grid graphics
library(patchwork) # grid graphics (better)
library(scales) # scale functions for visualization
library(gganimate) # make animated plots
library(plotly) # create interactive web graphics - use for html output files
library(ggcorrplot) # visualization of correlation matrix using ggplot2
# or install the latest ggcorrplot from GitHub
# Install
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggcorrplot")
