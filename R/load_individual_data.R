#' load_individual_data
#'
#' @return a clean data tibble containing information on individuals from the intake rate experiment
#'
load_individual_data = function(path) {
  ##########  0. Load data  ##########
  
  data_irn_individuals <- readxl::read_xlsx(path)
  
  str(data_irn_individuals)
  
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
  
  data_irn_individuals <- mutate(data_irn_individuals, across(character_columns, as.character))
  data_irn_individuals <- mutate(data_irn_individuals, across(date_columns, as.POSIXct))
  data_irn_individuals <- mutate(data_irn_individuals, across(factor_columns, as.factor))
  data_irn_individuals <- mutate(data_irn_individuals, across(numeric_columns, as.numeric))

  dat <- data_irn_individuals |>
  mutate(across(character_columns, as.character)) |>
    mutate(across(factor_columns, as.factor)) |>
    mutate(across(numeric_columns, as.numeric))
    
   str(data_irn_individuals)
  
  
  # Removing individuals that underwent experimental errors
  
  # Individual 38 was believed to undergo pre pupation too soon
  data_intake = data_intake[-which(data_intake$individual_ID == "38"), ]
  
  # Individual 94 was a L6
  data_intake = data_intake[-which(data_intake$individual_ID == "94"), ]
  
  ##########  2. Filling the table  ##########
  
  ##### Last collection day #####
  
  # We automatically define the last collection day based on whether or not the pre pupation occurred during the week
  
  for (i in 1:nrow(data_intake)) {
    if (is.na(data_intake$last_collection_date[i] == T)) {
      if (is.na(data_intake$pre_pupa_date[i] == T)) {
        data_intake$last_collection_date[i] = data_intake$first_collection_date[i] + lubridate::days(2)
      } else {
        if (is.na(data_intake$last_collection_date[i] == T)) {
          data_intake$last_collection_date[i] = data_intake$pre_pupa_date[i] - lubridate::days(1)
        }
      }
    }
  }
  
  ##### Number of collection day #####
  
  data_intake$number_collection_days = as.numeric(data_intake$last_collection_date - data_intake$first_collection_date + 1)
  
  ##### Bodymass at the last collection date #####
  
  # We create a column corresponding to the last bodymass measured before pre pupation, that is the bodymass at the last collection date
  data_intake$bodymass_before_last_collection_date = NA
  
  for (i in 1:nrow(data_intake)) {
    day_last_collection = as.numeric(data_intake$last_collection_date[i] - data_intake$first_collection_date[i]) +
      2
    data_intake$bodymass_before_last_collection_date[i] = as.numeric(data_intake[i, colnames(data_intake)[grepl("bodymass", colnames(data_intake))][day_last_collection]])
  }
  
  ##### Growth rate #####
  
  # We compute the growth rate on the 7th instar without prepupation
  data_intake$growth_rate = (
    data_intake$bodymass_before_last_collection_date - data_intake$bodymass_7th_instar_j0_ww
  ) / data_intake$number_collection_days
  data_intake$growth_rate_unit = "mg_ww/day"
}
