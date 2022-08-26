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
  # gam.check function to check whether the model converges or not.
  # Non convergence can be due to too high number of parameters compared to the number of data
  # We also check that smooths have enough basis functions
  # by looking at the significance result in the diagnostic test.
  
  # Using the broom package, and the augment, tidy, and glance functions, we can
  # insect, evaluate, and predict
  

  matrices = c("absorption", "larva", "egestion")
  elements = c("C",
               "N",
               "P",
               "Na",
               "Mg",
               "S",
               "K",
               "Ca")
  
  
  # Creating a dataframe containing statistics for the publication
  
  constituents = c(
    "total mass",
    "total mass",
    rep(elements,3)
  )
  variables = c("growth_efficiency",
                rep("absorption", 9),
                rep("larva", 8),
                rep("egestion", 8))
  
  
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
  
  forumlae = as.formula(paste(models_nutrients$variables, "~ s(ingestion_rate_fw)"))

  
  for (i in 1:nb_matrices) {
    data_matrix = subset(data_g, data_g$matrix == matrices[j])
    for (j in 1:nb_elements) {
      formula = 
        mod = mgcv::gam(formula, data = data_i)
      summary_mod = summary(mod)
      if (mod$converged == "T") {
        models_nutrients$edf[i] = summary_mod$edf
        models_nutrients$p_value[i] = summary_mod$s.pv
      }
    }
  }
  
  
  ##### Isotopes paper #####
  
}