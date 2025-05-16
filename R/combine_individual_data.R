#' combine_data
#'
#' @return a datatable with all individual data combined and new variables computed
#' (growth rate, growth efficiency, ingestion rate, egestion/ingestion ratio)
#' @export
#'
#' @examples
combine_individual_data <- function(data_fc, data_ic, data_i) {
  
  
  ##### Day 0 larvae water content #####
  
  data_i$larvae_day0_wc <- NA
  
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date <- data_i$seventh_instar_date[i]
    week_dates <- c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes <- which(data_ic$date %in% week_dates)
    
    # The day 0 larvae water content is equal to 1 - larval dry weight divided by larval fresh weight
    data_i$larvae_day0_wc[i] <- mean(data_ic$indiv_water_content[week_indexes])
  }
  
  ##### Overall water content #####
  
  data_i$wc3 <- ifelse(
    data_i$number_collection_days == 3,
    (
      data_i$bodymass_7th_instar_j3_fw - data_i$bodymass_7th_instar_j3_dw
    ) / data_i$bodymass_7th_instar_j3_fw,
    NA
  )
  wc3 <- mean(data_i$wc3, na.rm = T)
  wc1 <- mean(data_ic$indiv_water_content, na.rm = T)
  wc <- mean(c(wc3, wc1))
  
  
  # We compute the growth rate of the 7th instar before prepupation
  
  
  data_i$growth_rate <- (data_i$bodymass_last_collection_date - data_i$bodymass_7th_instar_j0_fw) / data_i$number_collection_days
  data_i$growth_rate_unit <- "mg_fw/day"
  
  ##### Mean bodymass raw #####
  
  data_i <- data_i %>%
    rowwise() %>%
    mutate(mean_bodymass_raw = sum(c(
      bodymass_7th_instar_j0_fw,
      bodymass_7th_instar_j1_fw,
      bodymass_7th_instar_j2_fw,
      bodymass_7th_instar_j3_fw
    )[1:(number_collection_days + 1)])/(number_collection_days + 1)
    ) %>%
    ungroup()
  
  ##### Mean bodymass with cubic splines #####
  
  # Calculate the average value of the spline function over the range of the x values
  
  data_i <- data_i %>%
    rowwise() %>%
    mutate(mean_bodymass = average_value(
      splinefun(
        1:(number_collection_days + 1),
        c(
          bodymass_7th_instar_j0_fw,
          bodymass_7th_instar_j1_fw,
          bodymass_7th_instar_j2_fw,
          bodymass_7th_instar_j3_fw
        )[1:(number_collection_days + 1)],
        method = "natural"
      ),
      1,
      (number_collection_days + 1)
    )) %>%
    ungroup()
  
  ##### Mean dw bodymass with cubic splines ####
  
  data_i <- data_i %>%
    rowwise() %>%
    mutate(mean_bodymass_dw = average_value(
      splinefun(
        1:(number_collection_days + 1),
        c(
          bodymass_7th_instar_j0_fw * (1 - wc),
          bodymass_7th_instar_j1_fw * (1 - wc),
          bodymass_7th_instar_j2_fw * (1 - wc),
          bodymass_7th_instar_j3_fw * (1 - wc)
        )[1:(number_collection_days + 1)],
        method = "natural"
      ),
      1,
      (number_collection_days + 1)
    )) %>%
    ungroup()
  

  
  ##### Geometric mean growth rate #####
  
  # Calculate daily growth rates
  data_i <- data_i %>%
    mutate(
      growth_day1 = (bodymass_7th_instar_j1_fw - bodymass_7th_instar_j0_fw) / bodymass_7th_instar_j0_fw,
      growth_day2 = (bodymass_7th_instar_j2_fw - bodymass_7th_instar_j1_fw) / bodymass_7th_instar_j1_fw,
      growth_day3 = (bodymass_7th_instar_j3_fw - bodymass_7th_instar_j2_fw) / bodymass_7th_instar_j2_fw
    )
  
  # Compute geometric mean growth rate for each individual and only for collection days
  data_i <- data_i %>%
    rowwise() %>%
    mutate(growth_rates = list(c(growth_day1, growth_day2, growth_day3)[1:number_collection_days]),
           geometric_mean_growth_fw = exp(mean(log(growth_rates), na.rm = TRUE))) %>%
    ungroup()
  
  ##### Geometric mean growth rate in dw #####
  
  # Calculate daily growth rates
  data_i <- data_i %>%
    mutate(
      growth_day1 = (
        bodymass_7th_instar_j1_fw * (1 - wc) - bodymass_7th_instar_j0_fw * (1 -
                                                                              wc)
      ) / (bodymass_7th_instar_j0_fw * (1 - wc)),
      growth_day2 = (
        bodymass_7th_instar_j2_fw * (1 - wc) - bodymass_7th_instar_j1_fw * (1 -
                                                                              wc)
      ) / (bodymass_7th_instar_j1_fw * (1 - wc)),
      growth_day3 = (
        bodymass_7th_instar_j3_fw * (1 - wc) - bodymass_7th_instar_j2_fw * (1 -
                                                                              wc)
      ) / (bodymass_7th_instar_j2_fw * (1 - wc))
    )
  
  # Compute geometric mean growth rate for each individual and only for collection days
  data_i <- data_i %>%
    rowwise() %>%
    mutate(growth_rates = list(c(growth_day1, growth_day2, growth_day3)[1:number_collection_days]),
           geometric_mean_growth_dw = exp(mean(log(growth_rates), na.rm = TRUE))) %>%
    ungroup()
  
  ##### Mean growth rate dw ####
  
  data_i <- data_i %>%
    rowwise() %>%
    mutate(mean_growth_dw = (log(c(
      bodymass_7th_instar_j0_fw * (1 - wc),
      bodymass_7th_instar_j1_fw * (1 - wc),
      bodymass_7th_instar_j2_fw * (1 - wc),
      bodymass_7th_instar_j3_fw * (1 - wc)
    )[number_collection_days + 1]) - log(bodymass_7th_instar_j0_fw * (1 - wc)))/(number_collection_days)) %>%
    ungroup()
  
  
  ##### Egested mass complete period in mg dw #####
  
  data_i$frass_mass_dw <- data_i$filled_tube_frass_mass - data_i$empty_tube_frass_mass
  data_i$frass_mass_dw_unit <- "mg_dw"
  
  ##### Food provided in dw #####
  
  data_i$food_provided_dw <- NA
  
  
  for (i in 1:nrow(data_i)) {
    # Define the date of seventh instar
    seventh_instar_date <- data_i$seventh_instar_date[i]
    # Define the week dates corresponding to the seventh instar
    week_dates <- c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    # We search for these dates in the food control dataset
    week_indexes <- which(data_fc$date == week_dates)
    
    # We compute the dry weight of food provided based on fresh weight of food and water content of food
    data_i$food_provided_dw[i] <- as.numeric(as.character((data_i$food_provided_fw[i]))) *
      (1 - mean(data_fc$food_water_content[week_indexes]))
  }
  
  ##### Food provided over the complete period #####
  
  # The food provided over the complete period is the food provided every day multiplied by the number of days
  
  # In dry weight
  data_i$food_provided_collection_days_dw <- data_i$food_provided_dw *
    data_i$number_collection_days
  
  data_i$food_provided_collection_days_dw_unit <- "mg dw"
  
  
  # In fresh weight
  data_i$food_provided_collection_days_fw <- data_i$food_provided_fw *
    data_i$number_collection_days
  
  data_i$food_provided_collection_days_fw_unit <- "mg fw"
  
  
  ##### Food consumed over the complete period #####
  
  # The food consumed over the collection days is equal to the mass of food provided minus the mass of remaining food
  
  
  # The remaining food mass corresponds to the weight of the filled tube minus the weight of the empty tube
  # In dry weight
  data_i$remaining_food_mass_dw <- data_i$filled_tube_food_mass - data_i$empty_tube_food_mass
  data_i$remaining_food_mass_dw_unit <- "mg_dw"
  
  # In dry weight
  
  data_i$food_consumed_collection_days_dw <- NA
  for (i in 1:nrow(data_i)) {
    if (is.na(data_i$remaining_food_mass_dw[i])) {
      data_i$food_consumed_collection_days_dw[i] <- data_i$food_provided_collection_days_dw[i]
    } else {
      data_i$food_consumed_collection_days_dw[i] <- data_i$food_provided_collection_days_dw[i] -
        data_i$remaining_food_mass_dw[i]
    }
  }
  
  data_i$food_consumed_collection_days_dw_unit <- "mg dw"
  
  # In fresh weight
  
  data_i$food_consumed_collection_days_fw <- NA
  
  for (i in 1:nrow(data_i)) {
    # Define the date of seventh instar
    seventh_instar_date <- data_i$seventh_instar_date[i]
    # Define the week dates corresponding to the seventh instar
    week_dates <- c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    # We search for these dates in the food control dataset
    week_indexes <- which(data_fc$date == week_dates)
    
    # We compute the dry weight of food provided based on fresh weight of food and water content of food
    data_i$food_consumed_collection_days_fw[i] <- data_i$food_consumed_collection_days_dw[i] /
      (1 - mean(data_fc$food_water_content[week_indexes]))
  }
  
  
  ##### Ingestion rate #####
  
  # The ingestion rate is equal to the total amount of food consumed over the collection days divided by the number of days
  
  # In dry weight
  data_i$ingestion_rate_dw <- data_i$food_consumed_collection_days_dw /
    data_i$number_collection_days
  data_i$ingestion_rate_dw_unit <- "mg dw / day"
  
  # In fresh weight
  
  data_i$ingestion_rate_fw <- data_i$food_consumed_collection_days_fw /
    data_i$number_collection_days
  data_i$ingestion_rate_fw_unit <- "mg fw / day"
  
  
  ##### Mass- specific ingestion rate #####
  # In dry weight
  
  data_i$mass_specific_ingestion_rate_dw <- data_i$ingestion_rate_dw / data_i$mean_bodymass_dw
  
  data_i$mass_specific_ingestion_rate_dw_unit <- "dw fw / dw fw / day"
  
  # In fresh weight
  
  data_i$mass_specific_ingestion_rate_fw <- data_i$ingestion_rate_fw / data_i$mean_bodymass
  data_i$mass_specific_ingestion_rate_fw_unit <- "mg fw / mg fw / day"
  
  
  
  # The ingestion rate is equal to the total amount of food consumed over the collection days divided by the number of days
  
  # # Try to model velocity using isometric growth
  # data_i$mass_specific_ingestion_rate_fw = data_i$ingestion_rate_fw /
  #   (0.11 * ((
  #     0.5 * (
  #       data_i$bodymass_last_collection_date + data_i$bodymass_7th_instar_j0_fw
  #     ) / 2
  #   ) ^ (2 / 3)))
  # data_i$mass_specific_ingestion_rate_fw_unit = "mg fw / mg fw to the 2/3 / day"
  
  ##### Egestion rate #####
  
  # The egestion rate is equal to the total amount of frass produced over the collection days divided by the number of days
  
  # In dry weight
  data_i$egestion_rate_dw <- data_i$frass_mass_dw /
    data_i$number_collection_days
  data_i$egestion_rate_dw_unit <- "mg dw / day"
  
  ##### Assimilated mass of food #####
  
  # In dry weight only, because we don't have the water content of frass.
  data_i$assimilated_mass_dw <- data_i$food_consumed_collection_days_dw - data_i$frass_mass_dw
  
  ##### Assimilation rate of food #####
  # In dry weight only, because we don't have the water content of frass.
  
  data_i$assimilation_rate_dw <- (data_i$food_consumed_collection_days_dw - data_i$frass_mass_dw) / data_i$number_collection_days
  
  ##### Mass-specific assimilation rate of food #####
  
  data_i$mass_specific_assimilation_rate_dw <- data_i$assimilation_rate_dw / data_i$mean_bodymass_dw
  
  ##### Assimilation efficiency of food #####
  
  data_i$assimilation_efficiency_dw <- (1 - (
    data_i$frass_mass_dw / data_i$food_consumed_collection_days_dw
  ))
  
  
  ##### Egestion - ingestion ratio #####
  # The egestion - ingestion ratio in equal to the total mass of frass produced
  # divided by the total amount of food consumed over the collection days
  
  data_i$egestion_ingestion_ratio_dw <- data_i$frass_mass_dw / data_i$food_consumed_collection_days_dw
  
  
  
  
  
  ##### Food conversion efficiency #####
  # We compute the food conversion efficiency for the 7th instar period without the prepupation
  
  # In fresh weight
  data_i$growth_efficiency_fw <- NA
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date <- data_i$seventh_instar_date[i]
    week_dates <- c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes <- which(data_fc$date == week_dates)
    
    # The growth efficiency in fresh weight is equal to fresh weight mass gains divided by fresh weight of food consumed
    
    data_i$growth_efficiency_fw[i] <- ((
      data_i$bodymass_last_collection_date[i] - data_i$bodymass_7th_instar_j0_fw[i]
    ) / data_i$food_consumed_collection_days_fw[i]
    ) # It is in fresh weight of food
  }
  
  # In dry weight
  data_i$growth_efficiency_dw <- NA
  data_i$growth_dw <- NA
  data_i$growth_investment_dw <- NA
  data_i$mass_specific_respiration_rate_dw <- NA
  data_i$respiration_rate_dw <- NA
  
  for (i in 1:nrow(data_i)) {
    seventh_instar_date <- data_i$seventh_instar_date[i]
    week_dates <- c(
      seventh_instar_date,
      seventh_instar_date + 24 * 60 * 60,
      seventh_instar_date + 2 * 24 * 60 * 60
    )
    week_indexes <- which(data_fc$date == week_dates)
    
    # The growth efficiency in dry weight is equal to estimated dry weight mass gains divided by dry weight of food consumed
    
    data_i$growth_efficiency_dw[i] <- ((
      data_i$bodymass_last_collection_date[i] * (1 - wc) - data_i$bodymass_7th_instar_j0_fw[i] *
        (1 - wc)
    ) / data_i$food_consumed_collection_days_dw[i]
    )
    # It is in dry weight of food
    
    # The growth over the collection days in dry weight is equal to estimated dry weight mass gains
    data_i$growth_dw[i] <- (
      data_i$bodymass_last_collection_date[i] * (1 - wc) - data_i$bodymass_7th_instar_j0_fw[i] *
        (1 - wc)
    )
    # It is in dry weight of food
    
    # The growth investment is the proportion of mass which was assimilated that ended up in growth
    # In dry weight
    data_i$growth_investment_dw[i] <- (
      data_i$bodymass_last_collection_date[i] * (1 - wc) - data_i$bodymass_7th_instar_j0_fw[i] *
        (1 - wc)
    ) / (data_i$assimilated_mass_dw[i])
    
    # The mass-specific maintenance rate is the amount of mass which was assimilated that did not end up in growth,
    # divided by time and the average individual weight
    # In dry weight
    
    data_i$mass_specific_respiration_rate_dw[i] <- (data_i$assimilated_mass_dw[i] - data_i$growth_dw[i]) / (data_i$number_collection_days[i] * data_i$mean_bodymass_dw[i])
    
    
    
    data_i$respiration_rate_dw[i] <- (data_i$assimilated_mass_dw[i] - data_i$growth_dw[i]) / data_i$number_collection_days[i]
  }
  
  ##### Group for body chemical analyses #####
  # Which groups of caterpillar had only individuals having been fed for 3 days before chemical analysis ?
  
  data_i$bca_3 = NA
    for (i in unique(data_i$group_ID)){
      group_rows = which(data_i$group_ID == i)
      bca_rows = group_rows[which(data_i$body_analysis[group_rows] == 1)]
      if (all(data_i$number_collection_days[bca_rows] == 3)){
        data_i$bca_3[group_rows] = 1
      } else {data_i$bca_3[group_rows] = 0}
    }
  

  return(data_i)
}
