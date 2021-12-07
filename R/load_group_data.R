#' load_group_data
#' 
#' @return a clean data tibble containing information on group chemical analysis from the intake rate experiment
#'
load_group_data = function(path){
  data_irn_groups <- readxl::read_xlsx(path)
}