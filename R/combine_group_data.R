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
  data_g$groupmass_7th_instar_j3_dw = NA
  data_g$seventh_instar_date = "2021-10-26 CEST"
  data_g$seventh_instar_date = as.POSIXct(data_g$seventh_instar_date)
  data_g$number_collection_days = NA
  data_g$geometric_mean_growth_fw = NA
  data_g$growth_efficiency_fw = NA
  data_g$assimilation_efficiency_dw = NA
  data_g$mean_mass_specific_intake_rate_fw = NA
  data_g$mean_egestion_rate_dw = NA
  data_g$mean_bodymass = NA
  data_g$mean_bodymass_dw = NA
  data_g$mean_mass_specific_intake_rate_dw = NA
  data_g$geometric_mean_growth_dw = NA
  
  ####  compute group-level measures based on the sum of the individual data ####
  
  for (i in 1:length(group_IDs)) {
    individual_group_rows = which(data_i$group_ID == group_IDs[i])
    group_row = which(data_g$group_ID == group_IDs[i])
    
    data_g$frass_group_mass_dw[group_row] = sum(data_i$frass_mass_dw[individual_group_rows])
    data_g$food_consumed_collection_days_dw[group_row] = sum(data_i$food_consumed_collection_days_dw[individual_group_rows])
    data_g$food_consumed_collection_days_fw[group_row] = sum(data_i$food_consumed_collection_days_fw[individual_group_rows])
    data_g$groupmass_7th_instar_j0_fw[group_row] = sum(data_i$bodymass_7th_instar_j0_fw[individual_group_rows])
    data_g$groupmass_7th_instar_j3_fw[group_row] = sum(data_i$bodymass_7th_instar_j3_fw[individual_group_rows])
    data_g$groupmass_7th_instar_j3_dw[group_row] = sum(data_i$bodymass_7th_instar_j3_dw[individual_group_rows], na.rm =
                                                         T) * 2
    data_g$mean_mass_specific_intake_rate_fw[group_row] = mean(data_i$mass_specific_ingestion_rate_fw[individual_group_rows])
    data_g$mean_mass_specific_intake_rate_dw[group_row] = mean(data_i$mass_specific_ingestion_rate_dw[individual_group_rows])
    
    if (data_g$groupmass_7th_instar_j3_dw[group_row] == 0) {
      data_g$groupmass_7th_instar_j3_dw[group_row] = NA
    }
    data_g$seventh_instar_date[group_row] = data_i$seventh_instar_date[individual_group_rows[1]]
    data_g$seventh_instar_date[group_row] = data_i$seventh_instar_date[individual_group_rows[1]]
    data_g$number_collection_days[group_row] = mean(data_i$number_collection_days[individual_group_rows])
    data_g$geometric_mean_growth_fw[group_row] = mean(data_i$geometric_mean_growth_fw[individual_group_rows])
    data_g$geometric_mean_growth_dw[group_row] = mean(data_i$geometric_mean_growth_dw[individual_group_rows])
    
    data_g$growth_efficiency_fw[group_row] = mean(data_i$growth_efficiency_fw[individual_group_rows])
    data_g$assimilation_efficiency_dw[group_row] = 1 - (sum(data_i$frass_mass_dw[individual_group_rows]) / sum(data_i$food_consumed_collection_days_dw[individual_group_rows]))
    data_g$mean_egestion_rate_dw[group_row] = mean(data_i$egestion_rate_dw[individual_group_rows])
    data_g$mean_bodymass[group_row] = mean(data_i$mean_bodymass[individual_group_rows])
    data_g$mean_bodymass_dw[group_row] = mean(data_i$mean_bodymass_dw[individual_group_rows])
  }
  
  
  
  ####  compute the group-level element-specific intake rate at the level of the group ####
  #### by combining food chemical analysis and group-level intake rate
  
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
    #  search for these dates in the food control dataset
    week_indexes = which(data_fc$date == week_dates)
    
    #  report the elemental content of food
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
  
  #### Computing the isotopic contents of frass ####
  #### Using PDB and air delta 13C and delta 15N
  #### of 0.0112372 and 0.003663 respectively
  
  data_g$`12C_frass` = data_g$C_frass / (1 + (((
    data_g$`d13C_frass` / 1000
  ) + 1) * 0.0112372))
  data_g$`13C_frass` = data_g$C_frass - data_g$`12C_frass`
  
  data_g$`14N_frass` = data_g$N_frass / (1 + (((
    data_g$`d15N_frass` / 1000
  ) + 1) * 0.003663))
  data_g$`15N_frass` = data_g$N_frass - data_g$`14N_frass`
  
  #### Computing the isotopic contents of the larvae ####
  
  data_g$`12C_larvae` = data_g$C_larvae / (1 + (((
    data_g$`d13C_larvae` / 1000
  ) + 1) * 0.0112372))
  data_g$`13C_larvae` = data_g$C_larvae - data_g$`12C_larvae`
  
  data_g$`14N_larvae` = data_g$N_larvae / (1 + (((
    data_g$`d15N_larvae` / 1000
  ) + 1) * 0.003663))
  data_g$`15N_larvae` = data_g$N_larvae - data_g$`14N_larvae`
  
  #### Computing the isotopic contents of the food ####
  
  data_g$`12C_food` = data_g$food_C / (1 + (((
    data_g$`food_d13C` / 1000
  ) + 1) * 0.0112372))
  data_g$`13C_food` = data_g$food_C - data_g$`12C_food`
  
  data_g$`14N_food` = data_g$food_N / (1 + (((
    data_g$`food_d15N` / 1000
  ) + 1) * 0.003663))
  data_g$`15N_food` = data_g$food_N - data_g$`14N_food`
  
  ####  compute the group intake rate ####
  
  # data_g$group_mass_specific_intake_rate_fw = data_g$food_consumed_collection_days_fw /
  #   (data_g$number_collection_days * ((
  #     data_g$groupmass_7th_instar_j0_fw + data_g$groupmass_7th_instar_j3_fw
  #   ) / 2
  #   ))
  
  

  
  #### Computes the element assimilation efficiency ####
  
  data_g$C_assimilation_efficiency_dw = 100 * (1 - ((data_g$C_frass * data_g$frass_group_mass_dw) /
                                                    (data_g$food_C * data_g$food_consumed_collection_days_dw)
  ))
  data_g$N_assimilation_efficiency_dw = 100 * (1 - ((data_g$N_frass * data_g$frass_group_mass_dw) /
                                                    (data_g$food_N * data_g$food_consumed_collection_days_dw)
  ))
  data_g$P_assimilation_efficiency_dw = 100 * (1 - ((data_g$P_frass * data_g$frass_group_mass_dw) /
                                                    (data_g$food_P * data_g$food_consumed_collection_days_dw)
  ))
  data_g$S_assimilation_efficiency_dw = 100 * (1 - ((data_g$S_frass * data_g$frass_group_mass_dw) /
                                                    (data_g$food_S * data_g$food_consumed_collection_days_dw)
  ))
  data_g$Na_assimilation_efficiency_dw = 100 * (1 - ((data_g$Na_frass * data_g$frass_group_mass_dw) /
                                                     (data_g$food_Na * data_g$food_consumed_collection_days_dw)
  ))
  data_g$Mg_assimilation_efficiency_dw = 100 * (1 - ((data_g$Mg_frass * data_g$frass_group_mass_dw) /
                                                     (data_g$food_Mg * data_g$food_consumed_collection_days_dw)
  ))
  data_g$K_assimilation_efficiency_dw = 100 * (1 - ((data_g$K_frass * data_g$frass_group_mass_dw) /
                                                    (data_g$food_K * data_g$food_consumed_collection_days_dw)
  ))
  data_g$Ca_assimilation_efficiency_dw = 100 * (1 - ((data_g$Ca_frass * data_g$frass_group_mass_dw) /
                                                     (data_g$food_Ca * data_g$food_consumed_collection_days_dw)
  ))
  data_g$`12C_assimilation_efficiency_dw` = 100 * (1 - ((data_g$`12C_frass` * data_g$frass_group_mass_dw) /
                                                        (
                                                          data_g$`12C_food` * data_g$food_consumed_collection_days_dw
                                                        )
  ))
  data_g$`13C_assimilation_efficiency_dw` = 100 * (1 - ((data_g$`13C_frass` * data_g$frass_group_mass_dw) /
                                                        (
                                                          data_g$`13C_food` * data_g$food_consumed_collection_days_dw
                                                        )
  ))
  data_g$`14N_assimilation_efficiency_dw` = 100 * (1 - ((data_g$`14N_frass` * data_g$frass_group_mass_dw) /
                                                        (
                                                          data_g$`14N_food` * data_g$food_consumed_collection_days_dw
                                                        )
  ))
  data_g$`15N_assimilation_efficiency_dw` = 100 * (1 - ((data_g$`15N_frass` * data_g$frass_group_mass_dw) /
                                                        (
                                                          data_g$`15N_food` * data_g$food_consumed_collection_days_dw
                                                        )
  ))
  
  #### Computes the element egestion rate ####
  
  data_g$C_egestion_rate_dw = data_g$C_frass * data_g$mean_egestion_rate_dw
  data_g$N_egestion_rate_dw = data_g$N_frass * data_g$mean_egestion_rate_dw
  data_g$P_egestion_rate_dw = data_g$P_frass * data_g$mean_egestion_rate_dw
  data_g$S_egestion_rate_dw = data_g$S_frass * data_g$mean_egestion_rate_dw
  data_g$Na_egestion_rate_dw = data_g$Na_frass * data_g$mean_egestion_rate_dw
  data_g$Mg_egestion_rate_dw = data_g$Mg_frass * data_g$mean_egestion_rate_dw
  data_g$K_egestion_rate_dw = data_g$K_frass * data_g$mean_egestion_rate_dw
  data_g$Ca_egestion_rate_dw = data_g$Ca_frass * data_g$mean_egestion_rate_dw
  data_g$`12C_egestion_rate_dw` = data_g$`12C_frass` * data_g$mean_egestion_rate_dw
  data_g$`13C_egestion_rate_dw` = data_g$`13C_frass` * data_g$mean_egestion_rate_dw
  data_g$`14N_egestion_rate_dw` = data_g$`14N_frass` * data_g$mean_egestion_rate_dw
  data_g$`15N_egestion_rate_dw` = data_g$`15N_frass` * data_g$mean_egestion_rate_dw
  
  #### Computes the element retention time ####
  
  data_g$C_retention_time = data_g$C_larvae * data_g$mean_bodymass_dw /
    data_g$C_egestion_rate_dw
  data_g$N_retention_time = data_g$N_larvae * data_g$mean_bodymass_dw /
    data_g$N_egestion_rate_dw
  data_g$P_retention_time = data_g$P_larvae * data_g$mean_bodymass_dw /
    data_g$P_egestion_rate_dw
  data_g$S_retention_time = data_g$S_larvae * data_g$mean_bodymass_dw /
    data_g$S_egestion_rate_dw
  data_g$Na_retention_time = data_g$Na_larvae * data_g$mean_bodymass_dw /
    data_g$Na_egestion_rate_dw
  data_g$Mg_retention_time = data_g$Mg_larvae * data_g$mean_bodymass_dw /
    data_g$Mg_egestion_rate_dw
  data_g$K_retention_time = data_g$K_larvae * data_g$mean_bodymass_dw /
    data_g$K_egestion_rate_dw
  data_g$Ca_retention_time = data_g$Ca_larvae * data_g$mean_bodymass_dw /
    data_g$Ca_egestion_rate_dw
  data_g$`12C_retention_time` = data_g$`12C_larvae` * data_g$mean_bodymass_dw /
    data_g$`12C_egestion_rate_dw`
  data_g$`13C_retention_time` = data_g$`13C_larvae` * data_g$mean_bodymass_dw /
    data_g$`13C_egestion_rate_dw`
  data_g$`14N_retention_time` = data_g$`14N_larvae` * data_g$mean_bodymass_dw /
    data_g$`14N_egestion_rate_dw`
  data_g$`15N_retention_time` = data_g$`15N_larvae` * data_g$mean_bodymass_dw /
    data_g$`15N_egestion_rate_dw`
  
  #### Isotopic assimilation efficiency ratios ####
  
  
  data_g$`C_iaer` = 1000 * (
    data_g$`13C_assimilation_efficiency_dw` / data_g$`12C_assimilation_efficiency_dw` -
      1
  )
  data_g$`N_iaer` = 1000 * (
    data_g$`15N_assimilation_efficiency_dw` / data_g$`14N_assimilation_efficiency_dw` -
      1
  )
  
  
  #### Isotopic fractionation ####
  data_g$`13C_tf` = data_g$`d13C_larvae` - data_g$food_d13C
  data_g$`15N_tf` = data_g$`d15N_larvae` - data_g$food_d15N
  
  #### Isotopic frass-food discrimination factor ####
  data_g$`13C_ffdf` = data_g$`d13C_frass` - data_g$food_d13C
  data_g$`15N_ffdf` = data_g$`d15N_frass` - data_g$food_d15N
  
  #### Isotopic frass-larvae discrimination factor ####
  data_g$`13C_fldf` = data_g$`d13C_frass` - data_g$`d13C_larvae`
  data_g$`15N_fldf` = data_g$`d15N_frass` - data_g$`d15N_larvae`
  
  data_g = tidyr::pivot_longer(
    data_g,
    cols = c(
      "C_assimilation_efficiency_dw",
      "N_assimilation_efficiency_dw",
      "P_assimilation_efficiency_dw",
      "S_assimilation_efficiency_dw",
      "Na_assimilation_efficiency_dw",
      "Mg_assimilation_efficiency_dw",
      "K_assimilation_efficiency_dw",
      "Ca_assimilation_efficiency_dw",
      "12C_assimilation_efficiency_dw",
      "13C_assimilation_efficiency_dw",
      "14N_assimilation_efficiency_dw",
      "15N_assimilation_efficiency_dw",
      "C_retention_time",
      "N_retention_time",
      "P_retention_time",
      "S_retention_time",
      "Na_retention_time",
      "Mg_retention_time",
      "K_retention_time",
      "Ca_retention_time",
      "12C_retention_time",
      "13C_retention_time",
      "14N_retention_time",
      "15N_retention_time",
      "C_iaer",
      "N_iaer",
      "13C_tf",
      "15N_tf",
      "13C_ffdf",
      "15N_ffdf",
      "13C_fldf",
      "15N_fldf",
      "C_frass",
      "N_frass",
      "P_frass",
      "S_frass",
      "Na_frass",
      "Mg_frass",
      "K_frass",
      "Ca_frass",
      "d13C_frass",
      "d15N_frass",
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
    names_to = "element_variable",
    values_to = "elemental_value"
  )
  
  data_g = tidyr::separate(
    data_g,
    "element_variable",
    c("element", "variable"),
    sep = "_",
    extra = "merge"
  )
  
  return(data_g)
}