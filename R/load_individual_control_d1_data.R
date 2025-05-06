#' load_individual_control_d1_data
#'
#' @return a clean data tibble containing information on control individuals from the intake rate experiment
#'
load_individual_control_d1_data <- function(path) {
  ##########  0. Load data  ##########
  
  data_irn_individuals_controls_d1 <- readr::read_delim(path)
  
  ##########  1. Structuration  ##########
  
  # Decide column classes
  
  factor_columns <- c("group_ID")
  
  numeric_columns <- c(
    "C_larvae",
    "N_larvae",
    "P_larvae",
    "Ca_larvae",
    "K_larvae",
    "Mg_larvae",
    "Na_larvae",
    "S_larvae"
  )
  
  text_columns <- c(
    "C_larvae_unit",
    "N_larvae_unit",
    "P_larvae_unit",
    "Ca_larvae_unit",
    "K_larvae_unit",
    "Mg_larvae_unit",
    "Na_larvae_unit",
    "S_larvae_unit"
  )

# We define the type of each column

data_irn_individuals_controls_d1 <- data_irn_individuals_controls_d1 |>
  dplyr::mutate(across(tidyselect::all_of(factor_columns), as.factor)) |>
  dplyr::mutate(across(tidyselect::all_of(numeric_columns), as.numeric)) |>
  dplyr::mutate(across(tidyselect::all_of(text_columns), as.character)) 


return(data_irn_individuals_controls_d1)
}
