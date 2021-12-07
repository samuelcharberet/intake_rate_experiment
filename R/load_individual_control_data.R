#' load_individual_control_data
#' 
#' @return a clean data tibble containing information on control individuals from the intake rate experiment
#'
load_individual_control_data = function(path){
  data_irn_control_individuals <- readxl::read_xlsx(path)
}