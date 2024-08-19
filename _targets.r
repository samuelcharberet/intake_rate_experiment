#################################################



#################################################
# _targets.R file


library(targets)
library(here)

packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "mgcv",
  "ggtext",
  "formula.tools",
  "fmsb",
  "ggsci",
  "tls",
  "scam",
  "rmarkdown",
  "lubridate",
  "latex2exp",
  "stargazer",
  "graphics",
  "minpack.lm",
  "onls",
  "scales",
  "patchwork",
  "viridis",
  "splines",
  "readr",
  "ggpubr",
  "ggsci",
  "gratia",
  "knitr"
)
packages_to_install <- packages[!(packages %in% installed.packages())]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
} else {
  message("No packages to install.")
}


tar_option_set(
  packages = packages
)

# We source all functions contained in all files in the R directory
lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)


list(
  # define individual data file
  tar_target(
    data_irn_individuals_file,
    here::here("1_data", "data_irn_individuals.csv"),
    format = "file"
  ),
  # define group data file
  tar_target(
    data_irn_groups_file,
    here::here("1_data", "data_irn_groups.csv"),
    format = "file"
  ),
  # define food control data file
  tar_target(
    data_irn_food_controls_file,
    here::here("1_data", "data_irn_food_controls.csv"),
    format = "file"
  ),
  # define individual control data file
  tar_target(
    data_irn_individuals_controls_file,
    here::here("1_data", "data_irn_individuals_controls.csv"),
    format = "file"
  ),
  # load individual data file
  tar_target(
    data_irn_individuals,
    load_individual_data(path = data_irn_individuals_file)
  ),
  # load group data file
  tar_target(data_irn_groups, load_group_data(path = data_irn_groups_file)),
  # load food control data file
  tar_target(
    data_irn_food_controls,
    load_food_control_data(path = data_irn_food_controls_file)
  ),
  # load individual control data file
  tar_target(
    data_irn_individuals_controls,
    load_individual_control_data(path = data_irn_individuals_controls_file)
  ),
  # Combine individual data
  tar_target(
    data_irn_indivuals_combined,
    combine_individual_data(
      data_fc = data_irn_food_controls,
      data_ic = data_irn_individuals_controls,
      data_i = data_irn_individuals
    )
  ),
  # Combine groups data
  tar_target(
    data_irn_groups_combined,
    combine_group_data(
      data_i = data_irn_indivuals_combined,
      data_fc = data_irn_food_controls,
      data_g = data_irn_groups
    )
  ),
  # Model the data
  tar_target(
    models_irn,
    model_irn(data_i = data_irn_indivuals_combined, data_g = data_irn_groups_combined)
  ),

  # Plot the data
  tar_target(
    plots_irn,
    plot_irn(
      data_i = data_irn_indivuals_combined,
      data_g = data_irn_groups_combined,
      data_model = models_irn,
      data_ic = data_irn_individuals_controls
    ),
    format = "file"
  ),

  # Fit a non linear model to the growth efficiency data
  tar_target(
    theoretical_models_irn,
    theoretical_model_irn(data_i = data_irn_indivuals_combined),
    format = "file"
  )

  # Generate report Rmd
  # archetypes::tar_render(rmd_report, "3_manuscript/irn.rmd")
)
