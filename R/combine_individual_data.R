#' combine_data
#'
#' @return a datatable with all individual data combined and new variables computed
#' (growth rate, growth efficiency, ingestion rate, egestion/ingestion ratio)
#' @export
#'
#' @examples
combine_individual_data <- function(data_fc, data_ic, data_i) {
  ##### Day 0 larvae water content #####
  
  data_i$larvae_day0_wc = NA
  
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date = data_i$seventh_instar_date[i]
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes = which(data_ic$date == week_dates)
    
    # The day 0 larvae water content is equal to 1 - larval dry weight divided by larval fresh weight
    data_i$larvae_day0_wc[i] =  mean(data_ic$indiv_water_content[week_indexes])
  }
  
  ##### Growth rate #####
  
  # We compute the growth rate of the 7th instar before prepupation
  
  
  data_i$growth_rate = (data_i$bodymass_last_collection_date - data_i$bodymass_7th_instar_j0_fw) / data_i$number_collection_days
  data_i$growth_rate_unit = "mg_fw/day"
  
  # We compute the growth rate of the 7th instar before prepupation
  
  
  data_i$specific_growth_rate = 2 * (data_i$bodymass_last_collection_date - data_i$bodymass_7th_instar_j0_fw) / (
    data_i$number_collection_days * (
      data_i$bodymass_last_collection_date + data_i$bodymass_7th_instar_j0_fw
    )
  )
  data_i$specific_growth_rate_unit = "mg_fw/day/mg_fw"
  
  ##### Egested mass complete period #####
  
  data_i$frass_mass_dw = data_i$filled_tube_frass_mass - data_i$empty_tube_frass_mass
  
  
  ##### Food provided in dw #####
  
  data_i$food_provided_dw = NA
  
  for (i in 1:nrow(data_i)) {
    # Define the date of seventh instar
    seventh_instar_date = data_i$seventh_instar_date[i]
    # Define the week dates corresponding to the seventh instar
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    # We search for these dates in the food control dataset
    week_indexes = which(data_fc$date == week_dates)
    
    # We compute the dry weight of food provided based on fresh weight of food and water content of food
    data_i$food_provided_dw[i] = as.numeric(as.character((data_i$food_provided_fw[i]))) *
      (1 - mean(data_fc$food_water_content[week_indexes]))
  }
  
  ##### Food provided over the complete period #####
  
  # The food provided over the complete period is the food provided every day multiplied by the number of days
  
  # In dry weight
  data_i$food_provided_collection_days_dw = data_i$food_provided_dw *
    data_i$number_collection_days
  
  data_i$food_provided_collection_days_dw_unit = "mg dw"
  
  
  # In fresh weight
  data_i$food_provided_collection_days_fw = data_i$food_provided_fw *
    data_i$number_collection_days
  
  data_i$food_provided_collection_days_fw_unit = "mg fw"
  
  
  ##### Food consumed over the complete period #####
  
  # The food consumed over the collection days is equal to the mass of food provided minus the mass of remaining food
  
  
  # The remaining food mass corresponds to the weight of the filled tube minus the weight of the empty tube
  # In dry weight
  data_i$remaining_food_mass_dw = data_i$filled_tube_food_mass - data_i$empty_tube_food_mass
  
  # In dry weight
  
  data_i$food_consumed_collection_days_dw = NA
  for (i in 1:nrow(data_i)) {
    if (is.na(data_i$remaining_food_mass_dw[i])) {
      data_i$food_consumed_collection_days_dw[i] = data_i$food_provided_collection_days_dw[i]
    }
    else {
      data_i$food_consumed_collection_days_dw[i] = data_i$food_provided_collection_days_dw[i] -
        data_i$remaining_food_mass_dw[i]
    }
  }
  
  data_i$food_consumed_collection_days_dw_unit = "mg dw"
  
  # In fresh weight
  
  data_i$food_consumed_collection_days_fw = NA
  
  for (i in 1:nrow(data_i)) {
    # Define the date of seventh instar
    seventh_instar_date = data_i$seventh_instar_date[i]
    # Define the week dates corresponding to the seventh instar
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    # We search for these dates in the food control dataset
    week_indexes = which(data_fc$date == week_dates)
    
    # We compute the dry weight of food provided based on fresh weight of food and water content of food
    data_i$food_consumed_collection_days_fw[i] = data_i$food_consumed_collection_days_dw[i] /
      (1 - mean(data_fc$food_water_content[week_indexes]))
  }
  
  
  ##### Ingestion rate #####
  
  # The ingestion rate is equal to the total amount of food consumed over the collection days divided by the number of days
  
  # In dry weight
  data_i$ingestion_rate_dw = data_i$food_consumed_collection_days_dw /
    data_i$number_collection_days
  data_i$ingestion_rate_dw_unit = "mg dw / day"
  
  data_i$mass_specific_ingestion_rate_dw = 2 * data_i$ingestion_rate_dw / (
    data_i$bodymass_7th_instar_j3_dw
    + data_i$bodymass_7th_instar_j0_fw * (1 - data_i$larvae_day0_wc)
  )
  
  data_i$mass_specific_ingestion_rate_fw_unit = "mg fw / mg fw / day"
  
  # In fresh weight
  
  data_i$ingestion_rate_fw = data_i$food_consumed_collection_days_fw /
    data_i$number_collection_days
  data_i$ingestion_rate_fw_unit = "mg fw / day"
  
  data_i$mass_specific_ingestion_rate_fw = data_i$ingestion_rate_fw / ((
    data_i$bodymass_last_collection_date + data_i$bodymass_7th_instar_j0_fw
  ) / 2)
  data_i$mass_specific_ingestion_rate_fw_unit = "mg fw / mg fw / day"
  
  ##### Egestion rate #####
  
  # The egestion rate is equal to the total amount of frass produced over the collection days divided by the number of days
  
  # In dry weight
  data_i$egestion_rate_dw = data_i$frass_mass_dw /
    data_i$number_collection_days
  data_i$egestion_rate_dw_unit = "mg dw / day"
  
  ##### Absorbed mass of food #####
  
  # In dry weight only, because we don't have the water content of frass.
  data_i$absorbed_mass_dw = data_i$food_consumed_collection_days_dw - data_i$frass_mass_dw
  
  ##### Absorption rate of food #####
  # In dry weight only, because we don't have the water content of frass.
  
  data_i$absorption_rate_dw = (data_i$food_consumed_collection_days_dw - data_i$frass_mass_dw) / data_i$number_collection_days
  
  ##### Mass-specific absorption rate of food #####

  data_i$mass_specific_absorption_rate_dw = data_i$absorption_rate_dw / ((
    data_i$bodymass_last_collection_date + data_i$bodymass_7th_instar_j0_fw
  ) / 2)
  
  ##### Absorption efficiency of food #####
  
  data_i$absorption_efficiency_dw = 100 * (1 - (
    data_i$frass_mass_dw / data_i$food_consumed_collection_days_dw
  ))
  
  
  ##### Egestion - ingestion ratio #####
  # The egestion - ingestion ratio in equal to the total mass of frass produced
  # divided by the total amount of food consumed over the collection days
  
  data_i$egestion_ingestion_ratio_dw = data_i$frass_mass_dw / data_i$food_consumed_collection_days_dw
  
  
  
  
  
  ##### Food conversion efficiency #####
  # We compute the food conversion efficiency for the 7th instar period without the prepupation
  
  # In fresh weight
  data_i$growth_efficiency_fw = NA
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date = data_i$seventh_instar_date[i]
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes = which(data_fc$date == week_dates)
    
    # The growth efficiency in fresh weight is equal to fresh weight mass gains divided by fresh weight of food consumed
    
    data_i$growth_efficiency_fw[i] = 100 * ((
      data_i$bodymass_last_collection_date[i] - data_i$bodymass_7th_instar_j0_fw[i]
    ) / data_i$food_consumed_collection_days_fw[i]
    )  # It is in fresh weight of food
  }
  
  # In dry weight
  data_i$growth_efficiency_dw = NA
  
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date = data_i$seventh_instar_date[i]
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes = which(data_fc$date == week_dates)
    
    # The growth efficiency in dry weight is equal to estimated dry weight mass gains divided by dry weight of food consumed
    data_i$growth_efficiency_dw[i] = 100 * ((
      data_i$bodymass_7th_instar_j3_dw[i] - data_i$bodymass_7th_instar_j0_fw[i] *
        (1 - data_i$larvae_day0_wc[i])
    ) / (data_i$food_consumed_collection_days_dw[i])
    )  # It is in dry weight of food
  }
  
  # The growth investment is the proportion of mass which was absorbed that ended up in growth
  # In dry weight
  
  data_i$growth_investment_dw = (
    data_i$bodymass_7th_instar_j3_dw - data_i$bodymass_7th_instar_j0_fw * (1 -
                                                                             data_i$larvae_day0_wc)
  ) / (data_i$absorbed_mass_dw)
  
  # The mass-specific maintenance rate is the amount of mass which was absorbed that did not end up in growth,
  # divided by time and the average individual weight
  # In dry weight
  
  data_i$mass_specific_maintenance_rate_dw = (
    data_i$absorbed_mass_dw -
      (
        data_i$bodymass_7th_instar_j3_dw - data_i$bodymass_7th_instar_j0_fw * (1 -
                                                                                 data_i$larvae_day0_wc)
      )
  ) / (
    data_i$number_collection_days * (
      data_i$bodymass_last_collection_date + data_i$bodymass_7th_instar_j0_fw
    ) / 2
  )
  
  
  return(data_i)
}
