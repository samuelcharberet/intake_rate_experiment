#' load_individual_data
#' 
#' @return a clean data tibble containing information on individuals from the intake rate experiment
#'
load_individual_data = function(path){
  data_irn_individuals <- readxl::read_xlsx(path)
}