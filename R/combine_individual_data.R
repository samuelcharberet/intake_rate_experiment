#' combine_data
#'
#' @return a datatable with all individual data combined and new variables computed
#' (growth rate, growth efficiency, ingestion rate, egestion/ingestion ratio)
#' @export
#'
#' @examples
combine_individual_data <- function(data_fc, data_ic, data_i) {
  
  
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
    
    # We compute the dry weight of food provided based on fresh weight of food and weter content of food
    data_i$food_provided_dw[i] = as.numeric(as.character((data_i$food_provided_ww[i]))) *
      (1 - mean(data_fc$food_water_content[week_indexes]))
  }
  
  ##### Food provided over the complete period #####
  
  # The food provided over the complete period is the food provided every day multiplied by the number of days
  data_i$food_provided_collection_days = data_i$food_provided_dw *
    data_i$number_collection_days
  
  data_i$food_provided_collection_days_unit = "mg dw"
  
  
  ##### Food consumed over the complete period #####
  
  # The remaining food mass corresponds to the weight of the filled tube minus the weight of the empty tube
  data_i$food_mass = data_i$filled_tube_food_mass - data_i$empty_tube_food_mass
  
  # The food consumed over the collection days is equal to the dry mass of food provided minus the dry mass of remaining food
  data_i$food_consumed_collection_days = NA
  for (i in 1:nrow(data_i)) {
    if (is.na(data_i$food_mass[i])) {
      data_i$food_consumed_collection_days[i] = data_i$food_provided_collection_days[i]
    }
    else {
      data_i$food_consumed_collection_days[i] = data_i$food_provided_collection_days[i] -
        data_i$food_mass[i]
    }
  }
  
  data_i$food_consumed_collection_days_unit = "mg dw"
  
  ##### Ingestion rate #####
  
  # The ingestion rate is equal to the total amount of food consumed over the collection days divided by the number of days
  
  data_i$ingestion_rate = data_i$food_consumed_collection_days /
    data_i$number_collection_days
  data_i$ingestion_rate_unit = "mg dw / day"
  
  
  ##### Egestion - ingestion ratio #####
  # The egestion - ingestion ratio in equal to the total mass of egestion produced 
  # divided by the total amount of food consumed over the collection days
  
  data_i$egestion_ingestion_ratio = data_i$egestion_mass / data_i$food_consumed_collection_days
  
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
  
  ##### Food conversion efficiency in fresh weight  #####
  # We compute the food conversion efficiency in fresh weight for the 7th instar period without the prepupation
  
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
    data_i$growth_efficiency_fw[i] = (
      data_i$bodymass_last_collection_date[i] - data_i$bodymass_7th_instar_j0_ww[i]
    ) / (data_i$food_consumed_collection_days[i] / (1 - mean(data_fc$food_water_content[week_indexes]))) # It is in fresh weight of food
  }
  
  ##### Food conversion efficiency in dry weight  #####
  # We compute the food conversion efficiency in dry weight for the 7th instar period without prepupation

  data_i$growth_efficiency_dw = NA
  
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date = data_i$seventh_instar_date[i]
    week_dates = c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes = which(data_fc$date == week_dates)
    
    # The growth efficiency in fresh weight is equal to fresh weight mass gains divided by fresh weight of food consumed
    data_i$growth_efficiency_dw[i] = (
      data_i$bodymass_7th_instar_j3_dw[i] - data_i$bodymass_7th_instar_j0_ww[i]*(1-data_i$larvae_day0_wc[i])
    ) / (data_i$food_consumed_collection_days[i])  # It is in dry weight of food
  }
  

  
  return(data_i)
}