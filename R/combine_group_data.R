#' combine_group_data
#'
#' @return a datatable with new variables at the level of the groups
#' @export
#'
#' @examples
combine_group_data <- function(data_i, data_ic, data_g) {
  # We compute a predictive group egestion mass by adding individual egestion masses
  group_IDs = unique(data_i$group_ID)
  
  for (i in 1:length(group_IDs)) {
    individual_group_rows = which(data_i$group_ID == group_IDs[i])
    group_row = which(data_g$group_ID == group_IDs[i])
    
    data_g$egestion_group_mass[group_row] = sum(data_i$egestion_mass[individual_group_rows])
  }
  return(data_g)
}