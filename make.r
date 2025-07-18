library(targets)
library(ggplot2)
library(tidyr)
library(mgcv)
library(ggtext)
library(formula.tools)
library(fmsb)
library(scam)
library(rmarkdown)
library(lubridate)
library(dplyr)
library(latex2exp)
library(rms)
library(tls)
library(stargazer)
library(graphics)
library(minpack.lm)
library(onls)
library(scales)
library(patchwork)
library(splines)
library(viridis)
library(gratia)
library(knitr)
library(emmeans)
library(marginaleffects)
library(modelsummary)
library(kableExtra)
library(tinytable)
library(gridtext)
library(stringr)
library(ggh4x)
library(readxl)


lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)

data_irn_individuals <-
  load_individual_data(path = here::here("1_data", "data_irn_individuals.csv"))

data_irn_food_control <-
  load_food_control_data(path = here::here("1_data", "data_irn_food_controls.csv"))

data_irn_groups <-
  load_group_data(path = here::here("1_data", "data_irn_groups.csv"))

data_irn_individuals_controls <-
  load_individual_control_data(path = here::here("1_data", "data_irn_individuals_controls.csv"))

data_irn_individuals_controls_d1 <-
  load_individual_control_d1_data(path = here::here("1_data", "data_irn_individuals_controls_d1.csv"))

data_irn_individuals_combined <-
  combine_individual_data(
    data_fc = data_irn_food_control,
    data_ic = data_irn_individuals_controls,
    data_i = data_irn_individuals
  )

data_irn_group_combined <-
  combine_group_data(
    data_i = data_irn_individuals_combined,
    data_g = data_irn_groups,
    data_fc = data_irn_food_control,
    data_icc = data_irn_individuals_controls_d1
  )

model <-
  model_irn(data_i = data_irn_individuals_combined, data_g = data_irn_group_combined)

plot_irn(
  data_i = data_irn_individuals_combined,
  data_g = data_irn_group_combined,
  data_model = model,
  data_fc = data_irn_food_control,
  data_ic = data_irn_individuals_controls
)

