#' combine_group_data
#'
#' @return a datatable with new variables at the level of the groups
#' @export
#'
#' @examples
combine_group_data <- function(data_i, data_g, data_fc) {
  # We compute a group egestion mass by adding individual egestion masses
  group_IDs = unique(data_i$group_ID)
  data_g$food_consumed_collection_days_dw = NA
  data_g$food_consumed_collection_days_fw = NA
  data_g$groupmass_7th_instar_j0_fw = NA
  data_g$groupmass_7th_instar_j3_fw = NA
  data_g$groupmass_7th_instar_j3_fw = NA
  data_g$seventh_instar_date = "2021-10-26 CEST"
  data_g$seventh_instar_date = as.POSIXct(data_g$seventh_instar_date)
  data_g$number_collection_days = NA
  

  # We compute group-level measures based on the sum of the individual data
  
  for (i in 1:length(group_IDs)) {
    individual_group_rows = which(data_i$group_ID == group_IDs[i])
    group_row = which(data_g$group_ID == group_IDs[i])
    
    data_g$egestion_group_mass_dw[group_row] = sum(data_i$egestion_mass_dw[individual_group_rows])
    data_g$food_consumed_collection_days_dw[group_row] = sum(data_i$food_consumed_collection_days_dw[individual_group_rows])
    data_g$food_consumed_collection_days_fw[group_row] = sum(data_i$food_consumed_collection_days_fw[individual_group_rows])
    data_g$groupmass_7th_instar_j0_fw[group_row] = sum(data_i$bodymass_7th_instar_j0_fw[individual_group_rows])
    data_g$groupmass_7th_instar_j3_fw[group_row] = sum(data_i$bodymass_7th_instar_j3_fw[individual_group_rows])
    data_g$seventh_instar_date[group_row] = data_i$seventh_instar_date[individual_group_rows[1]]
    data_g$seventh_instar_date[group_row] = data_i$seventh_instar_date[individual_group_rows[1]]
    data_g$number_collection_days[group_row] = sum(data_i$number_collection_days[individual_group_rows])
  }
  
  # We compute the group-level element-specific intake rate at the level of the group
  # by combining food chemical analysis and group-level intake rate
  
  data_g$food_C = NA
  data_g$food_N = NA 
  data_g$food_P = NA
  data_g$food_S = NA 
  data_g$food_Na = NA 
  data_g$food_Mg = NA 
  data_g$food_K = NA 
  data_g$food_Ca = NA 
  
  
  for (i in 1:nrow(data_g)) {
    # Define the date of seventh instar
    seventh_instar_date = data_g$seventh_instar_date[i]
    # Define the week dates corresponding to the seventh instar
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    # We search for these dates in the food control dataset
    week_indexes = which(data_fc$date == week_dates)
    
    # We report the elemental content of food
    data_g$food_C[i] = data_fc$food_C[week_indexes[1]]
    data_g$food_N[i] = data_fc$food_N[week_indexes[1]]
    data_g$food_P[i] = data_fc$food_P[week_indexes[1]]
    data_g$food_S[i] = data_fc$food_S[week_indexes[1]]
    data_g$food_Na[i] = data_fc$food_Na[week_indexes[1]]
    data_g$food_Mg[i] = data_fc$food_Mg[week_indexes[1]]
    data_g$food_K[i] = data_fc$food_K[week_indexes[1]]
    data_g$food_Ca[i] = data_fc$food_Ca[week_indexes[1]]
    
    
  }
  
  
  # Simulate some data while waiting for the chemical analysis to be done
  data_g$egestion_C = rnorm(nrow(data_g), 0.4, 0.04)
  data_g$egestion_N = rnorm(nrow(data_g), 0.07, 0.007)
  data_g$egestion_P = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$egestion_S = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$egestion_Na = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$egestion_Mg = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$egestion_K = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$egestion_Ca = rnorm(nrow(data_g), 0.03, 0.003)
  
  # We compute the group intake rate
  
  data_g$group_mass_specific_intake_rate_fw = data_g$food_consumed_collection_days_fw/(data_g$number_collection_days*((data_g$groupmass_7th_instar_j0_fw+data_g$groupmass_7th_instar_j3_fw)/2))
  
  # Computes the element absorption efficiency
  
  data_g$C_absorption_efficiency_dw = 1-((data_g$egestion_C*data_g$egestion_group_mass_dw)/(data_g$food_C*data_g$food_consumed_collection_days_dw))
  data_g$N_absorption_efficiency_dw = 1-((data_g$egestion_N*data_g$egestion_group_mass_dw)/(data_g$food_N*data_g$food_consumed_collection_days_dw))
  data_g$P_absorption_efficiency_dw = 1-((data_g$egestion_P*data_g$egestion_group_mass_dw)/(data_g$food_P*data_g$food_consumed_collection_days_dw))
  data_g$S_absorption_efficiency_dw = 1-((data_g$egestion_S*data_g$egestion_group_mass_dw)/(data_g$food_S*data_g$food_consumed_collection_days_dw))
  data_g$Na_absorption_efficiency_dw = 1-((data_g$egestion_Na*data_g$egestion_group_mass_dw)/(data_g$food_Na*data_g$food_consumed_collection_days_dw))
  data_g$Mg_absorption_efficiency_dw = 1-((data_g$egestion_Mg*data_g$egestion_group_mass_dw)/(data_g$food_Mg*data_g$food_consumed_collection_days_dw))
  data_g$K_absorption_efficiency_dw = 1-((data_g$egestion_K*data_g$egestion_group_mass_dw)/(data_g$food_K*data_g$food_consumed_collection_days_dw))
  data_g$Ca_absorption_efficiency_dw = 1-((data_g$egestion_Ca*data_g$egestion_group_mass_dw)/(data_g$food_Ca*data_g$food_consumed_collection_days_dw))
  
  

  return(data_g)
}