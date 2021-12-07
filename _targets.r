#################################################



#################################################
# _targets.R file

library(targets)
# We source all functions contained in all files in the R directory
lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)


list(
  # define individual data file
  tar_target(
    load_individual_data,
    "data/SAU_LME_20_v48_0_1/SAU_LME_20_v48_0.csv",
    format = "file"
  ),
  # define group data file
  tar_target(
    load_group_data,
    "data/ghs/Global_heterotroph_stoichio_v5.csv",
    format = "file"
  ),
  # define food control data file
  tar_target(
    load_food_control_data,
    "data/ghs/Global_heterotroph_stoichio_v5.csv",
    format = "file"
  ),
  # define individual control data file
  tar_target(
    load_individual_control_data,
    "data/ghs/Global_heterotroph_stoichio_v5.csv",
    format = "file"
  ),
  # merge data
  tar_target(
    data_irn,
    merge_data(
      load_individual_data,
      load_group_data,
      load_food_control_data,
      load_individual_control_data
    )
  ),
  # model the data
  tar_target(models_irn, model(data_irn)),
  # plot the data
  tar_target(plots_irn, plot(data_irn)),
  # generate report Rmd
  tarchetypes::tar_render(rmd_report, "3_manuscript/irn.rmd")
)