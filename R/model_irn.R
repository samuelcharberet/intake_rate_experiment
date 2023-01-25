#' model_irn
#'
#' @return model results from the intake rate and nutrient experiment
#' @export
#'
#' @examples
model_irn <- function(data_i, data_g) {
  variable_list_tm = c(
    "absorption_rate_dw",
    "absorption_efficiency_dw",
    "growth_rate",
    "growth_efficiency_fw"
  ) # The variables in the total mass balance study
  
  nb_variable_tm = length(variable_list_tm)
  matrix_list_ch = c("absorption", "larvae", "frass") # The variables in the chemical study
  nb_matrix_ch = length(matrix_list_ch)
  elements_list = c("C",
                    "N",
                    "P",
                    "Na",
                    "Mg",
                    "S",
                    "K",
                    "Ca") # The elements
  nb_elements = length(elements_list)
  
  # Creating a dataframe containing GAM statistics for both total mass and chemical balances
  
  
  variable = paste(c(variable_list_tm,
                     rep(matrix_list_ch, each = nb_elements)), c(rep("", nb_variable_tm),
                                                                 rep(elements_list, nb_matrix_ch)))
  
  
  nb_row = length(variable)
  
  n = rep(NA, nb_row)
  r_squared = rep(NA, nb_row)
  edf = rep(NA, nb_row)
  p_value = rep(NA, nb_row)
  difference = rep(NA, nb_row)
  
  gam_nutrients = data.frame(
    variable = variable,
    n = n,
    r_squared = r_squared,
    edf = edf,
    p_value = p_value,
    difference = difference
  )
  
  lm_nutrients = data.frame(
    variable = variable,
    cor_coef = rep(NA, nb_row),
    p_value_cor = rep(NA, nb_row),
    oao = rep(NA, nb_row),
    slope = rep(NA, nb_row),
    r_squared = rep(NA, nb_row),
    p_value_lm = rep(NA, nb_row),
    signif_level = rep(NA, nb_row)
  )
  
  # Creating a dataframe containing selected predicted values to generate a radar chart
  
  n_polygon = 3
  gam_larvae_values_radar = as.data.frame(matrix(rep(NA , (n_polygon + 2) * nb_elements) , ncol = nb_elements))
  colnames(gam_larvae_values_radar) <- elements_list
  rownames(gam_larvae_values_radar) <-
    c("max", "min", "IR_0.4", "IR_0.8", "IR_1.2")
  
  gam_frass_values_radar = as.data.frame(matrix(rep(NA , (n_polygon + 2) * nb_elements) , ncol = nb_elements))
  colnames(gam_frass_values_radar) <- elements_list
  rownames(gam_frass_values_radar) <-
    c("max", "min", "IR_0.4", "IR_0.8", "IR_1.2")
  
  # We have two datasets, one at the level of individuals for total mass balance
  # and another at the level of the group, for chemical data only (elements and isotopes)
  
  ###### 1. Total mass balance ######
  
  for (i in 1:nb_variable_tm) {
    formula_gam = as.formula(paste(
      variable_list_tm[i],
      "~ s(mass_specific_ingestion_rate_fw)"
    ))
    formula_spearman = as.formula(paste(
      "~",
      variable_list_tm[i],
      "+",
      "mass_specific_ingestion_rate_fw"
    ))
    sp = cor.test(
      formula_spearman,
      method = "spearman",
      exact = F,
      data = data_i,
      alternative = "two.sided"
    )
    lm_nutrients$cor_coef[i] = sp$estimate
    lm_nutrients$p_value_cor[i] = sp$p.value
    gam_mod = mgcv::gam(formula_gam, data = data_i) # a GAM model
    summary_gam = summary(gam_mod)
    if (gam_mod$converged == "TRUE") {
      gam_nutrients$n[i] = summary_gam$n
      gam_nutrients$r_squared[i] = format(signif(summary_gam$r.sq, digits = 3), scientific = F)
      gam_nutrients$edf[i] = format(signif(summary_gam$edf, digits = 3), scientific = F)
      if (summary_gam$s.pv == 0) {
        gam_nutrients$p_value[i] = "<2e-16"
      }
      else{
        gam_nutrients$p_value[i] = format(signif(summary_gam$s.pv, digits = 2), scientific = T)
      }
    }
  }
  
  ###### 2. Chemical balance ######
  
  for (i in 1:nb_matrix_ch) {
    data_matrix = subset(data_g, data_g$matrix == matrix_list_ch[i]) # selecting only element i
    for (j in 1:nb_elements) {
      data_matrix_element = subset(data_matrix, data_matrix$element == elements_list[j]) # selecting only matrix j
      
      formula_spearman = as.formula(paste(
        "~",
        "group_mass_specific_intake_rate_fw",
        "+",
        "elemental_value"
      ))
      formula_gam = as.formula(paste(
        "elemental_value",
        "~ s(group_mass_specific_intake_rate_fw,sp=20)"
      ))
      gam_mod = mgcv::gam(formula_gam, data = data_matrix_element) # creates a GAM for this element i in matrix j according to IR
      formula_lm = as.formula(paste(
        "elemental_value",
        "~ group_mass_specific_intake_rate_fw"
      ))
      lm_mod = lm(formula_lm, data = data_matrix_element) # creates a LMfor this element i in matrix j according to IR
      summary_gam = summary(gam_mod)
      summary_lm = summary(lm_mod)
      k = nb_variable_tm + (i - 1) * (nb_elements) + j #The row number in the final result tables
      
      sp = cor.test(
        formula_spearman,
        method = "spearman",
        exact = F,
        data = data_matrix_element
      )
      lm_nutrients$cor_coef[k] = sp$estimate
      lm_nutrients$p_value_cor[k] = sp$p.value
      lm_nutrients$oao[k] = summary_lm$coefficients[1, 1]
      lm_nutrients$slope[k] = summary_lm$coefficients[2, 1]
      lm_nutrients$r_squared[k] = summary_lm$adj.r.squared
      lm_nutrients$p_value_lm[k] = summary_lm$coefficients[2, 4]
      
      #Adding the significance level using the stars symbols
      
      if (summary_lm$coefficients[2, 4] < 0.001) {
        lm_nutrients$signif_level[k] = "***"
      }
      else if (0.001 < summary_lm$coefficients[2, 4] &
               summary_lm$coefficients[2, 4] < 0.01) {
        lm_nutrients$signif_level[k] = "**"
      }
      else if (0.01 < summary_lm$coefficients[2, 4] &
               summary_lm$coefficients[2, 4] < 0.05) {
        lm_nutrients$signif_level[k] = "*"
      }
      
      if (gam_mod$converged == "TRUE") {
        gam_nutrients$n[k] = summary_gam$n
        gam_nutrients$r_squared[k] = format(signif(summary_gam$r.sq, digits = 3), scientific = F)
        gam_nutrients$edf[k] = format(signif(summary_gam$edf, digits = 3), scientific = F)
        gam_nutrients$difference[k] = (max(gam_mod$fitted.values) / min(gam_mod$fitted.values)) -
          1
        
        
        if (summary_gam$s.pv == 0) {
          gam_nutrients$p_value[k] = "<2e-16"
        }
        else{
          gam_nutrients$p_value[k] = format(signif(summary_gam$s.pv, digits = 2), scientific = T)
        }
        if (matrix_list_ch[i] == "larvae") {
          # We want to estimate the fitted GAM values of larvae content for GMSIR of 0.4, 0.8, and 1.2
          new_data = as.data.frame(c(0.4, 0.8, 1.2))
          colnames(new_data) = "group_mass_specific_intake_rate_fw"
          
          gam_larvae_values_radar["max" , j] = max(gam_mod$fitted.values)
          gam_larvae_values_radar["min", j] = min(gam_mod$fitted.values)
          gam_larvae_values_radar[3:5, j] = predict(gam_mod, new_data)
          
        }
        if (matrix_list_ch[i] == "frass") {
          # We want to estimate the fitted GAM values of larvae content for GMSIR of 0.4, 0.8, and 1.2
          new_data = as.data.frame(c(0.4, 0.8, 1.2))
          colnames(new_data) = "group_mass_specific_intake_rate_fw"
          
          gam_frass_values_radar["max" , j] = max(gam_mod$fitted.values)
          gam_frass_values_radar["min", j] = min(gam_mod$fitted.values)
          gam_frass_values_radar[3:5, j] = predict(gam_mod, new_data)
          
        }
      }
    }
  }
  
  
  write.csv(
    lm_nutrients,
    file = here::here("4_outputs",
                      "1_statistical_results",
                      "lm_nutrients.csv")
  )
  
  
  write.csv(
    gam_nutrients,
    file = here::here("4_outputs",
                      "1_statistical_results",
                      "gam_nutrients.csv")
  )
  
  write.csv(
    gam_larvae_values_radar,
    file = here::here(
      "4_outputs",
      "1_statistical_results",
      "gam_larvae_radar.csv"
    )
  )
  
  write.csv(
    gam_frass_values_radar,
    file = here::here(
      "4_outputs",
      "1_statistical_results",
      "gam_frass_radar.csv"
    )
  )
  
  ###### 3. For isotopes #####
  
  # We wish to build models to test
  # The effect of growth rate on trophic fractionations
  # The effect of absorption efficiency on the FLDF
  # The effect of mass-specific intake rate on IAER
  
  dependant_variables_list = c("tf", "fldf", "iaer")
  independant_variables_list = c(
    "growth_rate",
    "absorption_efficiency_dw",
    "group_mass_specific_intake_rate_fw"
  )
  
  nb_dependant_variables = length(dependant_variables_list)
  
  isotopes_list = c("13C",
                    "15N")
  nb_isotopes = length(isotopes_list)
  
  
  # Creating a dataframe containing statistics for the publication
  
  # Column for isotope
  nb_row = nb_isotopes * nb_dependant_variables
  
  
  n = rep(NA, nb_row)
  formula = rep(NA, nb_row)
  F_stat = rep(NA, nb_row)
  R_squared = rep(NA, nb_row)
  edf = rep(NA, nb_row)
  equation = rep(NA, nb_row)
  p_value = rep(NA, nb_row)
  
  # Creating the dataframe
  models_isotopes = data.frame(
    equation = equation,
    n = n,
    R_squared = R_squared,
    F_stat = F_stat,
    p_value = p_value
  )
  
  gam_isotopes = data.frame(
    formula = formula,
    n = n,
    R_squared = R_squared,
    edf = edf,
    p_value = p_value
  )
  
  k = 0
  
  for (i in 1:nb_dependant_variables) {
    data_variable = subset(data_g, data_g$matrix == dependant_variables_list[i])
    for (j in 1:nb_isotopes) {
      data_variable_isotope = subset(
        data_variable,
        substr(
          data_variable$element,
          nchar(data_variable$element),
          nchar(data_variable$element)
        ) == substr(
          isotopes_list[j],
          nchar(isotopes_list[j]),
          nchar(isotopes_list[j])
        )
      )
      
      formula_lm = as.formula(paste("elemental_value",
                                    "~ ",
                                    independant_variables_list[i]))
      
      formula_gam = as.formula(paste(
        "elemental_value",
        "~ ",
        "s",
        "(",
        independant_variables_list[i],
        ")"
      ))
      
      mod_linear = lm(formula_lm, data = data_variable_isotope)
      summary_mod = summary(mod_linear)
      mod_gam = mgcv::gam(formula_gam, data = data_variable_isotope)
      summary_gam = summary(mod_gam)
      
      k = k + 1
      
      if (mod_gam$converged == "TRUE") {
        gam_isotopes$formula[k] = paste(
          isotopes_list[j],
          dependant_variables_list[i],
          " = ",
          "a",
          "x",
          independant_variables_list[i],
          "+",
          "b"
        )
        gam_isotopes$n[k] = summary_gam$n
        gam_isotopes$r_squared[k] = format(signif(summary_gam$r.sq, digits = 3), scientific = F)
        gam_isotopes$edf[k] = format(signif(summary_gam$edf, digits = 3), scientific = F)
        if (summary_gam$s.pv == 0) {
          gam_isotopes$p_value[k] = "<2e-16"
        }
        else{
          gam_isotopes$p_value[k] = format(signif(summary_gam$s.pv, digits = 2), scientific = T)
        }
      }
      
      
      models_isotopes$equation[k] = paste(
        isotopes_list[j],
        dependant_variables_list[i],
        " = ",
        signif(summary_mod$coefficients[2, 1], digits = 2),
        "x",
        independant_variables_list[i],
        "+",
        signif(summary_mod$coefficients[1, 1], digits = 5)
      )
      
      models_isotopes$n[k] = length(data_variable_isotope$elemental_value) - sum(is.na(data_variable_isotope$elemental_value))
      models_isotopes$F_stat[k] = signif(summary_mod$fstatistic[1], digits =
                                           2)
      models_isotopes$p_value[k] = scales::pvalue(
        summary_mod$coefficients[2, 4],
        accuracy = 0.01,
        # Number to round to
        decimal.mark = ".",
        # The character to be used to indicate the numeric decimal point
        add_p = TRUE
      )
      
      models_isotopes$R_squared[k] = signif(summary_mod$r.squared, digits = 2)
      
    }
  }
  
  write.csv(
    models_isotopes,
    file = here::here(
      "4_outputs",
      "1_statistical_results",
      "models_isotopes_linear.csv"
    )
  )
  
  write.csv(
    gam_isotopes,
    file = here::here("4_outputs",
                      "1_statistical_results",
                      "gam_isotopes.csv")
  )
  
  list_radar = list(gam_larvae_values_radar, gam_frass_values_radar)
  list_model = list(list_radar, lm_nutrients)
  names(list_model) = c("radar", "lm_nutrients")
  return(list_model)
  
}