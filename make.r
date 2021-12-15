


lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)

data_irn_individuals <-
  load_individual_data(path = here::here("1_data", "data_irn_individuals.csv"))

data_irn_food_control <-
  load_food_control_data(path = here::here("1_data", "data_irn_food_controls.csv"))

data_irn_groups <-
  load_group_data(path = here::here("1_data", "data_irn_groups.csv"))

data_irn_individuals_controls <-
  load_individual_control_data(path = here::here("1_data", "data_irn_individuals_controls.csv"))

data_irn_inidivuals_combined <-
  combine_individual_data(data_fc = data_irn_food_control,
                          data_ic = data_irn_individuals_controls,
                          data_i = data_irn_individuals)

data_irn_group_combined <- combine_groups_data(data_irn_inidivuals_combined,
                                               data_irn_groups)

model <- model_irn(data_irn_inidivuals_combined, data_irn_group_combined)

plot_irn(data_irn_inidivuals_combined, data_irn_group_combined)