#' model_irn
#'
#' @return model results from the intake rate and nutrient experiment
#' @export
#'
#' @examples
model_irn <- function(data_i, data_g) {
  # GAM
  # for all relationships
  # Extract the p-value and the edf. When edf is close to 1, the relationship is close to linear.
  # Non convergence can be due to too high number of parameters compared to the number of data
  
  ###### 1. For chemical elements #####
  
  variables_list = c("absorption", "larvae", "egestion")
  nb_variables = length(variables_list)
  elements_list = c("C",
                    "N",
                    "P",
                    "Na",
                    "Mg",
                    "S",
                    "K",
                    "Ca")
  nb_elements = length(elements_list)
  
  
  # Creating a dataframe containing statistics for the publication
  
  constituent = c("total mass",
                  "total mass",
                  rep(elements_list, nb_variables))
  variable = c(
    "growth_efficiency",
    rep("absorption", nb_elements + 1),
    rep("larvae", nb_elements),
    rep("egestion", nb_elements)
  )
  
  n = rep(NA, length(constituent))
  r_squared = rep(NA, length(constituent))
  edf = rep(NA, length(constituent))
  p_value = rep(NA, length(constituent))
  difference = rep(NA, length(constituent))
  
  models_nutrients = data.frame(
    constituent = constituent,
    variable = variable,
    n = n,
    r_squared = r_squared,
    edf = edf,
    p_value = p_value,
    difference = difference
  )
  
  # We have two datasets, one at the level of individuals
  # and another at the level of the group
  
  n_mod_data_i = length(which(constituent == "total mass"))
  n_mod_data_g = length(constituent) - length(which(constituent == "total mass"))
  
  # At the level of individuals
  
  formula = as.formula(paste("growth_efficiency_fw", "~ s(ingestion_rate_fw)"))
  mod = mgcv::gam(formula, data = data_i)
  summary_mod = summary(mod)
  if (mod$converged == "TRUE") {
    models_nutrients$n[1] = summary_mod$n
    models_nutrients$r_squared[1] = format(signif(summary_mod$r.sq, digits = 3), scientific = F)
    models_nutrients$edf[1] = format(signif(summary_mod$edf, digits = 3), scientific = F)
    if (summary_mod$s.pv == 0) {
      models_nutrients$p_value[1] = "<2e-16"
    }
    else{
      models_nutrients$p_value[1] = format(signif(summary_mod$s.pv, digits = 2), scientific = T)
    }
  }
  
  formula = as.formula(paste("absorption_efficiency_dw", "~ s(ingestion_rate_dw)"))
  mod = mgcv::gam(formula, data = data_i)
  summary_mod = summary(mod)
  if (mod$converged == "TRUE") {
    models_nutrients$n[2] = summary_mod$n
    models_nutrients$r_squared[2] = format(signif(summary_mod$r.sq, digits = 3), scientific = F)
    models_nutrients$edf[2] = format(signif(summary_mod$edf, digits = 3), scientific = F)
    if (summary_mod$s.pv == 0) {
      models_nutrients$p_value[2] = "<2e-16"
    }
    else{
      models_nutrients$p_value[2] = format(signif(summary_mod$s.pv, digits = 2), scientific = T)
    }
  }
  
  # At the level of groups
  
  for (i in 1:nb_variables) {
    data_variable = subset(data_g, data_g$matrix == variables_list[i])
    for (j in 1:nb_elements) {
      data_variable_element = subset(data_variable, data_variable$element == elements_list[j])
      formula = as.formula(paste(
        "elemental_value",
        "~ s(group_mass_specific_intake_rate_fw)"
      ))
      mod = mgcv::gam(formula, data = data_variable_element)
      summary_mod = summary(mod)
      k = which(
        models_nutrients$variable == variables_list[i] &
          models_nutrients$constituent == elements_list[j]
      )
      if (mod$converged == "TRUE") {
        models_nutrients$n[k] = summary_mod$n
        models_nutrients$r_squared[k] = format(signif(summary_mod$r.sq, digits = 3), scientific = F)
        models_nutrients$edf[k] = format(signif(summary_mod$edf, digits = 3), scientific = F)
        models_nutrients$difference[k] = (max(mod$fitted.values)/min(mod$fitted.values))-1
        if (summary_mod$s.pv == 0) {
          models_nutrients$p_value[k] = "<2e-16"
        }
        else{
          models_nutrients$p_value[k] = format(signif(summary_mod$s.pv, digits = 2), scientific = T)
        }
      }
    }
  }
  
  write.csv(
    models_nutrients,
    file = here::here(
      "4_outputs",
      "1_statistical_results",
      "models_nutrients.csv"
    ),
  )
  
  ###### 2. For isotopes #####
  
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
  
}