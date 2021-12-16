#' load_individual_data
#'
#' @return a clean data tibble containing information on individuals from the intake rate experiment
#'
load_individual_data = function(path) {
  ##########  0. Load data  ##########
  
  data_irn_individuals <- readr::read_delim(path)
  
  ##########  1. Structuration  ##########
  
  # Decide column classes
  
  character_columns = c(
    "food_provided_unit",
    "bodymass_unit",
    "tube_egestion_mass_unit",
    "egestion_mass_unit",
    "tube_food_mass_unit",
    "food_mass_unit",
    "reason"
  )
  date_columns = c(
    "seventh_instar_date",
    "pre_pupa_date",
    "pupa_date",
    "first_collection_date",
    "last_collection_date"
  )
  
  factor_columns = c(
    "individual_ID",
    "group_ID",
    "treatment_ID",
    "sex",
    "tube_egestion_ID",
    "tube_food_ID",
    "body_analysis",
    "emergence",
    "individual_removed"
  )
  numeric_columns = c(
    "food_provided_ww",
    "bodymass_7th_instar_j0_ww",
    "bodymass_7th_instar_j1_ww",
    "bodymass_7th_instar_j2_ww",
    "bodymass_7th_instar_j3_ww",
    "bodymass_7th_instar_j3_dw",
    "bodymass_7th_instar_j16_ww",
    "bodymass_imago_ww",
    "bodymass_imago_dw",
    "empty_tube_egestion_mass",
    "filled_tube_egestion_mass",
    "egestion_mass",
    "empty_tube_food_mass",
    "filled_tube_food_mass",
    "food_mass",
    "number_collection_days"
  )
  
  # We define the type of each column
  
  data_irn_individuals <- data_irn_individuals |>
    dplyr::mutate(across(tidyselect::all_of(character_columns), as.character)) |>
    dplyr::mutate(across(tidyselect::all_of(date_columns), ~ as.POSIXct(.x, format = "%d/%m/%Y"))) |>
    dplyr::mutate(across(tidyselect::all_of(factor_columns), as.factor)) |>
    dplyr::mutate(across(tidyselect::all_of(numeric_columns), as.numeric))
  
  # Removing individuals that underwent experimental errors
  
  # Individual 27 has a very high egestion/ingestion ratio (0.8), which seems impossible
  # Possibly due to tube weighing error
  data_irn_individuals = data_irn_individuals[-which(data_irn_individuals$individual_ID == "27"),]
  
  # Individual 38 was believed to undergo pre pupation too soon
  data_irn_individuals = data_irn_individuals[-which(data_irn_individuals$individual_ID == "38"),]
  
  # Individual 94 was a L6 instead of a L7 on the first day of the experiment
  data_irn_individuals = data_irn_individuals[-which(data_irn_individuals$individual_ID == "94"),]
  
  ##########  2. Filling the table  ##########
  
  ##### Last collection day #####
  
  # We automatically define the last collection day based on whether or not the pre pupation occurred during the week
  
  for (i in 1:nrow(data_irn_individuals)) {
    # If no last collection date is specified, meaning that the individual was the object of three collection
    if (is.na(data_irn_individuals$last_collection_date[i]) == T) {
      # And if no prepupation date is specified
      if (is.na(data_irn_individuals$pre_pupa_date[i]) == T) {
        # Then the last collection date was two days after the first one
        data_irn_individuals$last_collection_date[i] = data_irn_individuals$first_collection_date[i] + lubridate::days(2)
      } else {
        # Else it means that the last collection date was one day before the prepupation
        data_irn_individuals$last_collection_date[i] = data_irn_individuals$pre_pupa_date[i] - lubridate::days(1)
      }
    }
  }
  
  ##### Number of collection day #####
  
  data_irn_individuals$number_collection_days = as.numeric(
    data_irn_individuals$last_collection_date - data_irn_individuals$first_collection_date + 1
  )
  
  ##### Bodymass at the last collection date #####
  
  # We create a column corresponding to the last bodymass measured before pre pupation, that is the bodymass at the last collection date
  data_irn_individuals$bodymass_last_collection_date = NA
  
  for (i in 1:nrow(data_irn_individuals)) {
    # We define for each individuals the day number of last collection
    if (is.na(data_irn_individuals$first_collection_date[i]) == F) {
      day_last_collection = as.numeric(
        data_irn_individuals$last_collection_date[i] - data_irn_individuals$first_collection_date[i]
      ) + 1
      # Define the bodymass column corresponding to this day number
      col_name = colnames(data_irn_individuals)[grepl("bodymass", colnames(data_irn_individuals))][day_last_collection +
                                                                                                     1]
      # We add the corresponding mass in the new column
      data_irn_individuals$bodymass_last_collection_date[i] = as.numeric(data_irn_individuals[i, col_name])
    }
  }
  
  ##### Growth rate #####
  
  # We compute the growth rate of the 7th instar before prepupation
  
  
  data_irn_individuals$growth_rate = (
    data_irn_individuals$bodymass_last_collection_date - data_irn_individuals$bodymass_7th_instar_j0_ww
  ) / data_irn_individuals$number_collection_days
  data_irn_individuals$growth_rate_unit = "mg_ww/day"
  
  ##### Egested mass complete period #####
  
  data_irn_individuals$egestion_mass = data_irn_individuals$filled_tube_egestion_mass - data_irn_individuals$empty_tube_egestion_mass
  
  return(data_irn_individuals)
}
