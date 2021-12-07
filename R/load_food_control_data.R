#' load_food_control_data
#' 
#' @return a clean data tibble containing information on food controls chemical analysis from the intake rate experiment
#'
load_food_control_data = function(path){
  data_irn_food_control <- readxl::read_xlsx(path)
}