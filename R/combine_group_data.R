#' combine_group_data
#'
#' @return a datatable with new variables at the level of the groups
#' @export
#'
#' @examples
combine_group_data <- function(data_i, data_g, data_fc) {
  group_IDs = unique(data_i$group_ID)
  data_g$food_consumed_collection_days_dw = NA
  data_g$food_consumed_collection_days_fw = NA
  data_g$groupmass_7th_instar_j0_fw = NA
  data_g$groupmass_7th_instar_j3_fw = NA
  data_g$groupmass_7th_instar_j3_fw = NA
  data_g$seventh_instar_date = "2021-10-26 CEST"
  data_g$seventh_instar_date = as.POSIXct(data_g$seventh_instar_date)
  data_g$number_collection_days = NA
  data_g$growth_rate = NA

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
    data_g$growth_rate[group_row] = mean(data_i$growth_rate[individual_group_rows])
    data_g$growth_efficiency_fw[group_row] = mean(data_i$growth_efficiency_fw[individual_group_rows])
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
  data_g$food_d13C = NA
  data_g$food_d15N = NA 
  
  
  
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
    data_g$food_d13C[i] = data_fc$`food_d13C`[week_indexes[1]]
    data_g$food_d15N[i] = data_fc$`food_d15N`[week_indexes[1]]
    
    
  }
  
  # Computing the isotopic contents of egestions
  # Using PDB and air delta 13C and delta 15N 
  # of 0.0112372 and 0.003663 respectively
  
  data_g$`12C_egestion` = data_g$C_egestion/(1+(((data_g$`d13C_egestion`/1000)+1)*0.0112372))
  data_g$`13C_egestion` = data_g$C_egestion-data_g$`12C_egestion`
  
  data_g$`14N_egestion` = data_g$N_egestion/(1+(((data_g$`d15N_egestion`/1000)+1)*0.003663))
  data_g$`15N_egestion` = data_g$N_egestion-data_g$`14N_egestion`
  
  # Computing the isotopic contents of the larvae
  
  data_g$`12C_larvae` = data_g$C_larvae/(1+(((data_g$`d13C_larvae`/1000)+1)*0.0112372))
  data_g$`13C_larvae` = data_g$C_larvae-data_g$`12C_larvae`
  
  data_g$`14N_larvae` = data_g$N_larvae/(1+(((data_g$`d15N_larvae`/1000)+1)*0.003663))
  data_g$`15N_larvae` = data_g$N_larvae-data_g$`14N_larvae`
  
  # Computing the isotopic contents of the food
  
  data_g$`12C_food` = data_g$food_C/(1+(((data_g$`food_d13C`/1000)+1)*0.0112372))
  data_g$`13C_food` = data_g$food_C-data_g$`12C_food`
  
  data_g$`14N_food` = data_g$food_N/(1+(((data_g$`food_d15N`/1000)+1)*0.003663))
  data_g$`15N_food` = data_g$food_N-data_g$`14N_food`
  
  
  # Simulate some data while waiting for the chemical analysis to be done
  data_g$P_egestion = rnorm(nrow(data_g), 0.5, 0.08)
  data_g$S_egestion = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Na_egestion = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Mg_egestion = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$K_egestion = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Ca_egestion = rnorm(nrow(data_g), 0.03, 0.003)
  
  data_g$P_larvae = rnorm(nrow(data_g), 0.5, 0.08)
  data_g$S_larvae = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Na_larvae = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Mg_larvae = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$K_larvae = rnorm(nrow(data_g), 0.03, 0.003)
  data_g$Ca_larvae = rnorm(nrow(data_g), 0.03, 0.003)
  
  # We compute the group intake rate
  
  data_g$group_mass_specific_intake_rate_fw = data_g$food_consumed_collection_days_fw/(data_g$number_collection_days*((data_g$groupmass_7th_instar_j0_fw+data_g$groupmass_7th_instar_j3_fw)/2))
  
  # Computes the element absorption efficiency
  
  data_g$C_absorption_efficiency_dw = 1-((data_g$C_egestion*data_g$egestion_group_mass_dw)/(data_g$food_C*data_g$food_consumed_collection_days_dw))
  data_g$N_absorption_efficiency_dw = 1-((data_g$N_egestion*data_g$egestion_group_mass_dw)/(data_g$food_N*data_g$food_consumed_collection_days_dw))
  data_g$P_absorption_efficiency_dw = 1-((data_g$P_egestion*data_g$egestion_group_mass_dw)/(data_g$food_P*data_g$food_consumed_collection_days_dw))
  data_g$S_absorption_efficiency_dw = 1-((data_g$S_egestion*data_g$egestion_group_mass_dw)/(data_g$food_S*data_g$food_consumed_collection_days_dw))
  data_g$Na_absorption_efficiency_dw = 1-((data_g$Na_egestion*data_g$egestion_group_mass_dw)/(data_g$food_Na*data_g$food_consumed_collection_days_dw))
  data_g$Mg_absorption_efficiency_dw = 1-((data_g$Mg_egestion*data_g$egestion_group_mass_dw)/(data_g$food_Mg*data_g$food_consumed_collection_days_dw))
  data_g$K_absorption_efficiency_dw = 1-((data_g$K_egestion*data_g$egestion_group_mass_dw)/(data_g$food_K*data_g$food_consumed_collection_days_dw))
  data_g$Ca_absorption_efficiency_dw = 1-((data_g$Ca_egestion*data_g$egestion_group_mass_dw)/(data_g$food_Ca*data_g$food_consumed_collection_days_dw))
  data_g$`12C_absorption_efficiency_dw` = 1-((data_g$`12C_egestion`*data_g$egestion_group_mass_dw)/(data_g$`12C_food`*data_g$food_consumed_collection_days_dw))
  data_g$`13C_absorption_efficiency_dw` = 1-((data_g$`13C_egestion`*data_g$egestion_group_mass_dw)/(data_g$`13C_food`*data_g$food_consumed_collection_days_dw))
  data_g$`14N_absorption_efficiency_dw` = 1-((data_g$`14N_egestion`*data_g$egestion_group_mass_dw)/(data_g$`14N_food`*data_g$food_consumed_collection_days_dw))
  data_g$`15N_absorption_efficiency_dw` = 1-((data_g$`15N_egestion`*data_g$egestion_group_mass_dw)/(data_g$`15N_food`*data_g$food_consumed_collection_days_dw))
  
  # Isotopic absorption efficiency ratios

  
  data_g$`C_iaer` = data_g$`13C_absorption_efficiency_dw`/ data_g$`12C_absorption_efficiency_dw`
  data_g$`N_iaer` = data_g$`15N_absorption_efficiency_dw`/data_g$`14N_absorption_efficiency_dw`
  
  
  # Isotopic fractionation
  data_g$`13C_tf` = data_g$`d13C_larvae`-data_g$food_d13C
  data_g$`15N_tf` = data_g$`d15N_larvae`-data_g$food_d15N
  
  # Isotopic egestion-diet discrimination factor
  data_g$`13C_eddf` = data_g$`d13C_egestion`-data_g$food_d13C
  data_g$`15N_eddf` = data_g$`d15N_egestion`-data_g$food_d15N
  
  # Isotopic egestion-larvae discrimination factor
  data_g$`13C_eldf` = data_g$`d13C_egestion`-data_g$`d13C_larvae`
  data_g$`15N_eldf` = data_g$`d15N_egestion`-data_g$`d15N_larvae`
  
  data_g = tidyr::pivot_longer(
    data_g,
    cols = c(
      "C_absorption_efficiency_dw",
      "N_absorption_efficiency_dw",
      "P_absorption_efficiency_dw",
      "S_absorption_efficiency_dw",
      "Na_absorption_efficiency_dw",
      "Mg_absorption_efficiency_dw",
      "K_absorption_efficiency_dw",
      "Ca_absorption_efficiency_dw",
      "12C_absorption_efficiency_dw",
      "13C_absorption_efficiency_dw",
      "14N_absorption_efficiency_dw",
      "15N_absorption_efficiency_dw",
      "C_iaer",
      "N_iaer",
      "13C_tf",
      "15N_tf",
      "13C_eddf",
      "15N_eddf",
      "13C_eldf",
      "15N_eldf",
      "C_egestion",
      "N_egestion",
      "P_egestion",
      "S_egestion",
      "Na_egestion",
      "Mg_egestion",
      "K_egestion",
      "Ca_egestion",
      "d13C_egestion",
      "d15N_egestion",
      "C_larvae",
      "N_larvae",
      "P_larvae",
      "S_larvae",
      "Na_larvae",
      "Mg_larvae",
      "K_larvae",
      "Ca_larvae",
      "d13C_larvae",
      "d15N_larvae"
    ),
    names_to = "element_matrix",
    values_to = "elemental_value"
  )
  
  data_g = tidyr::separate(data_g, "element_matrix", c("element", "matrix"), sep =
                             "_")

  return(data_g)
}