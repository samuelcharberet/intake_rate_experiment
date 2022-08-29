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
  
  
  edf = rep(NA, length(constituent))
  p_value = rep(NA, length(constituent))
  models_nutrients = data.frame(
    constituent = constituent,
    variable = variable,
    edf = edf,
    p_value = p_value
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
    models_nutrients$edf[1] = signif(summary_mod$edf, digits = 2)
    models_nutrients$p_value[1] = signif(summary_mod$s.pv, digits = 2)
  }
  
  formula = as.formula(paste("absorption_efficiency_dw", "~ s(ingestion_rate_dw)"))
  mod = mgcv::gam(formula, data = data_i)
  summary_mod = summary(mod)
  if (mod$converged == "TRUE") {
    models_nutrients$edf[2] = signif(summary_mod$edf, digits = 2)
    models_nutrients$p_value[2] = signif(summary_mod$s.pv, digits = 2)
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
        models_nutrients$edf[k] = signif(summary_mod$edf, digits = 2)
        models_nutrients$p_value[k] = signif(summary_mod$s.pv, digits = 2)
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
  
  variables_list = c("tf", "fldf", "ffdf")
  nb_variables = length(variables_list)
  isotopes_list = c("13C",
                    "15N")
  nb_isotopes = length(isotopes_list)
  
  
  # Creating a dataframe containing statistics for the publication
  
  isotope = c(rep(isotopes_list, nb_variables))
  variable = c(rep("tf", nb_isotopes),
               rep("fldf", nb_isotopes),
               rep("ffdf", nb_isotopes))
  
  
  F_stat = rep(NA, length(isotope))
  equation = rep(NA, length(isotope))
  p_value = rep(NA, length(isotope))
  models_isotopes = data.frame(
    isotope = isotope,
    variable = variable,
    equation = equation,
    F_stat = F_stat,
    p_value = p_value
  )
  
  for (i in 1:nb_variables) {
    data_variable = subset(data_g, data_g$matrix == variables_list[i])
    for (j in 1:nb_isotopes) {
      data_variable_isotope = subset(data_variable, data_variable$element == isotopes_list[j])
      formula = as.formula(paste(
        "elemental_value",
        "~ group_mass_specific_intake_rate_fw"
      ))
      mod = lm(formula, data = data_variable_isotope)
      summary_mod = summary(mod)
      k = which(
        models_isotopes$variable == variables_list[i] &
          models_isotopes$isotope == isotopes_list[j]
      )
      models_isotopes$equation[k] = paste(
        round(summary_mod$coefficients[1, 1], digits = 2),
        round(summary_mod$coefficients[2, 1], digits = 2),
        ".",
        "msir"
      )
      models_isotopes$F_stat[k] = round(summary_mod$fstatistic[1], digits =
                                          2)
      models_isotopes$p_value[k] = signif(summary_mod$coefficients[2, 4], digits = 2)
      
      
    }
  }
  
  write.csv(
    models_isotopes,
    file = here::here(
      "4_outputs",
      "1_statistical_results",
      "models_isotopes.csv"
    ),
  )
  
}