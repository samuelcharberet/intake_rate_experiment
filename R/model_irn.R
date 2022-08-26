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
  
  matrices = c("absorption", "larvae", "egestion")
  nb_matrices = length(matrices)
  elements = c("C",
               "N",
               "P",
               "Na",
               "Mg",
               "S",
               "K",
               "Ca")
  nb_elements = length(elements)
  
  
  # Creating a dataframe containing statistics for the publication
  
  constituents = c("total mass",
                   "total mass",
                   rep(elements, 3))
  variables = c("growth_efficiency",
                rep("absorption", nb_elements+1),
                rep("larvae", nb_elements),
                rep("egestion", nb_elements))
  
  
  edf = rep(NA, length(constituents))
  p_value = rep(NA, length(constituents))
  models_nutrients = data.frame(
    constituents = constituents,
    variables = variables,
    edf = edf,
    p_value = p_value
  )
  
  # We have two datasets, one at the level of individuals
  # and another at the level of the group
  
  n_mod_data_i = length(which(constituents == "total mass"))
  n_mod_data_g = length(constituents) - length(which(constituents == "total mass"))
  
  # At the level of individuals
  
  formula = as.formula(paste("growth_efficiency_fw", "~ s(ingestion_rate_fw)"))
  mod = mgcv::gam(formula, data = data_i)
  summary_mod = summary(mod)
  if (mod$converged == "TRUE") {
    models_nutrients$edf[1] = summary_mod$edf
    models_nutrients$p_value[1] = summary_mod$s.pv
  }
  
  formula = as.formula(paste("absorption_efficiency_dw", "~ s(ingestion_rate_dw)"))
  mod = mgcv::gam(formula, data = data_i)
  summary_mod = summary(mod)
  if (mod$converged == "TRUE") {
    models_nutrients$edf[2] = summary_mod$edf
    models_nutrients$p_value[2] = summary_mod$s.pv
  }
  
  # At the level of groups
  
  for (i in 1:nb_matrices) {
    data_matrix = subset(data_g, data_g$matrix == matrices[i])
    for (j in 1:nb_elements) {
      data_matrix_element = subset(data_matrix, data_matrix$element == elements[j])
      formula = as.formula(paste(
        "elemental_value",
        "~ s(group_mass_specific_intake_rate_fw)"
      ))
      mod = mgcv::gam(formula, data = data_matrix_element)
      summary_mod = summary(mod)
      k = which(
        models_nutrients$variables == matrices[i] &
          models_nutrients$constituents == elements[j]
      )
      if (mod$converged == "TRUE") {
        models_nutrients$edf[k] = summary_mod$edf
        models_nutrients$p_value[k] = summary_mod$s.pv
      }
    }
  }
  
  write.csv(
    models_nutrients,
    file = here::here("4_outputs", "1_statistical_results", "models_nutrients.csv"),
  )
  
  ###### 2. For isotopes #####
  
  variables = c("tf", "fldf", "ffdf")
  nb_variables = length(matrices)
  isotopes = c("13C",
               "15N")
  nb_isotopes = length(isotopes)
  
  
  # Creating a dataframe containing statistics for the publication
  
  constituents = c(rep(isotopes, nb_variables))
  variables = c(rep("tf", nb_isotopes),
                rep("fldf", nb_isotopes),
                rep("ffdf", nb_isotopes))
  
  
  edf = rep(NA, length(constituents))
  p_value = rep(NA, length(constituents))
  models_isotopes = data.frame(
    constituents = constituents,
    variables = variables,
    edf = edf,
    p_value = p_value
  )
  
  for (i in 1:nb_variables) {
    data_variable = subset(data_g, data_g$matrix == variables[i])
    for (j in 1:nb_isotopes) {
      data_variable_isotope = subset(data_variable, data_variable$element == isotopes[j])
      formula = as.formula(paste(
        "elemental_value",
        "~ s(group_mass_specific_intake_rate_fw)"
      ))
      mod = mgcv::gam(formula, data = data_matrix_element)
      summary_mod = summary(mod)
      k = which(
        models_isotopes$variables == variables[i] &
          models_isotopes$constituents == isotopes[j]
      )
      if (mod$converged == "TRUE") {
        models_nutrients$edf[k] = summary_mod$edf
        models_nutrients$p_value[k] = summary_mod$s.pv
      }
    }
  }
  
  write.csv(
    models_isotopes,
    file = here::here("4_outputs", "1_statistical_results", "models_isotopes.csv"),
  )
  
}