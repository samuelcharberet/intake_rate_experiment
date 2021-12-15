#################################################



#################################################
# _targets.R file

library(targets)
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
    load_individual_data(data_irn_individuals_file)
  ),
  # load group data file
  tar_target(
    data_irn_groups,
    load_group_data(data_irn_groups_file)
  ),
  # load food control data file
  tar_target(
    data_irn_food_controls,
  load_food_controle(data_irn_food_controls_file)
  ),
  # define individual control data file
  tar_target(
    data_irn_individuals_controls,
    load_individual_control_data(data_irn_individuals_controls_file)
  ),
  # merge individual data
  tar_target(
    data_irn_inidivuals_combined,
    combine_individual_data(
      load_food_control_data,
      load_individual_control_data,
      load_individual_data
    )
  ),
  # merge individual data
  tar_target(
    data_irn_groups_combined,
    combine_groups_data(
      data_irn_inidivuals_combined,
      load_gro
      load_food_control_data,
      load_individual_control_data,
      load_individual_data
    )
  ),
  # model the data
  tar_target(models_irn, model(data_irn)),
  # plot the data
  tar_target(plots_irn, plot(data_irn)),
  # generate report Rmd
  tarchetypes::tar_render(rmd_report, "3_manuscript/irn.rmd")
)