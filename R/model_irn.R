#' model_irn
#'
#' @return model results from the intake rate and nutrient experiment
#' @export
#'
#' @examples
model_irn <- function(data_i, data_g) {
  # We have two datasets, one at the level of individuals for total mass balance
  # and another at the level of the group, for chemical data only (elements and isotopes)
  
  
  # I. Total mass balance GAM models #####
  
  # The predictor variables in the total mass balance study
  predictors_list_tm <- c("MSIR", "MSIR", "MSIR", "GR")
  # The response variables in the total mass balance study
  responses_list_tm <- c("GR", "AE", "GE", "GE")
  
  nb_models_tm <- length(responses_list_tm)
  
  # Creating a dataframe containing GAM statistics for total mass balance
  
  n <- rep(NA, nb_models_tm)
  adj_r_squared <- rep(NA, nb_models_tm)
  n_par <- rep(NA, nb_models_tm)
  edf <- rep(NA, nb_models_tm)
  ref_df <- rep(NA, nb_models_tm)
  p_value <- rep(NA, nb_models_tm)
  family <- rep(NA, nb_models_tm)
  smoother <- c("TP", "TP", "AD", "AD")
  
  gam_tm <- tibble(
    Predictor = predictors_list_tm,
    Response = responses_list_tm,
    n = n,
    edf = edf,
    "ref df" = ref_df,
    "n parameters" = n_par,
    p = p_value,
    "Adjusted R^2" = adj_r_squared,
    Family = family,
    Smoother = smoother
  )
  
  
  ## 1. The models ######
  
  mod_msgrdw_msirdw <- mgcv::gam(
    geometric_mean_growth_dw ~ s(mass_specific_ingestion_rate_fw),
    family = scat(),
    method = "REML",
    data = data_i
  )
  mod_aedw_msirdw <- mgcv::gam(
    assimilation_efficiency_dw ~ s(mass_specific_ingestion_rate_dw),
    family = scat(),
    method = "REML",
    data = data_i
  )
  mod_gedw_msirdw <- mgcv::gam(
    growth_efficiency_dw ~ s(
      mass_specific_ingestion_rate_dw,
      bs = "ad",
      k = 10
    ),
    data = data_i,
    method = "REML",
    family = scat()
  )
  mod_gedw_msgrdw <- mgcv::gam(
    growth_efficiency_dw ~ s(geometric_mean_growth_dw, bs = "ad", k = 10),
    data = data_i,
    method = "REML",
    family = scat()
  )
  ##  2. Removing outliers   ######
  
  models <- list(mod_msgrdw_msirdw,
                 mod_aedw_msirdw,
                 mod_gedw_msirdw,
                 mod_gedw_msgrdw)
  
  for (i in 1:length(models)) {
    hlt <- 10 * sum(mgcv::influence.gam(models[[i]]) / length(mgcv::influence.gam(models[[i]])))
    data_i_f <- filter(data_i, mgcv::influence.gam(models[[i]]) < hlt)
    call = models[[i]]$call
    call$data <- quote(data_i_f)
    models[[i]] = eval(call)
  }
  
  ##  3. Constructing a table  ######
  
  
  for (i in 1:nb_models_tm) {
    if (models[[i]]$converged == "TRUE") {
      gam_tm$n[i] <- broom::glance(models[[i]])$nobs
      gam_tm$edf[i] <- format(signif(broom::tidy(models[[i]])$edf, digits = 3), scientific = F)
      gam_tm$`ref df`[i] <- format(signif(broom::tidy(models[[i]])$ref.df, digits = 3), scientific = F)
      gam_tm$`n parameters`[i] <- broom::glance(models[[i]])$npar
      if (broom::tidy(models[[i]])$p.value == 0) {
        gam_tm$p[i] <- "<2e-16"
      } else {
        gam_tm$p[i] <- format(signif(broom::tidy(models[[i]])$p.value, digits = 2), scientific = T)
      }
      gam_tm$`Adjusted R^2`[i] <- format(signif(broom::glance(models[[i]])$adj.r.squared, digits = 2), scientific = F) 
      gam_tm$Family[i] <- models[[i]]$family$family
    }
  }
  
  # Save the table
  write.csv(gam_tm,
            file = here::here("4_outputs", "1_statistical_results", "gam_tm.csv"))
  
  
  # II. Chemical mass balance models ######
  # The variables in the chemical mass balance study
  
  responses_list_ch <- c("larvae",
                         "frass",
                         "assimilation_efficiency_dw",
                         "retention_time") # The variables in the chemical study
  
  responses_list_ch_nice <- c("Larvae", "Frass", "AE", "RT")
  nb_responses_ch <- length(responses_list_ch)
  # The elements
  elements_list <- c("C", "N", "P", "Na", "Mg", "S", "K", "Ca")
  nb_elements <- length(elements_list)
  
  nb_models_ch <- nb_responses_ch * nb_elements
  
  # Creating a dataframe containing GAM statistics for total mass balance
  elements <- rep(elements_list, nb_responses_ch)
  response = rep(responses_list_ch, each = nb_elements)
  response_nice = rep(responses_list_ch_nice, each = nb_elements)
  n <- rep(NA, nb_models_ch)
  adj_r_squared <- rep(NA, nb_models_ch)
  n_par <- rep(NA, nb_models_ch)
  edf <- rep(NA, nb_models_ch)
  ref_df <- rep(NA, nb_models_ch)
  p_value <- rep(NA, nb_models_ch)
  family <- rep(NA, nb_models_ch)
  smoother <- rep("TP", nb_models_ch)
  link_function <- rep(NA, nb_models_ch)
  gam_ch <- tibble(
    Response = response_nice,
    Element = elements,
    n = n,
    edf = edf,
    "ref df" = ref_df,
    "n parameters" = n_par,
    p = p_value,
    "Adjusted R^2" = adj_r_squared,
    Family = family,
    Link_function = link_function,
    Smoother = smoother
  )
  
  models_methods = c(
    replicate(nb_elements, list(family = gaussian()), simplify = FALSE),
    replicate(nb_elements, list(family = gaussian()), simplify = FALSE),
    replicate(nb_elements, list(family = scat()), simplify = FALSE),
    replicate(nb_elements, list(family = Gamma(link = log)), simplify = FALSE)
  )
  
  for (i in 1:length(models_methods)) {
    data = filter(data_g,
                  element == elements[i],
                  variable == response[i],
                  !is.na(elemental_value))
    
    mod = mgcv::gam(
      elemental_value ~ s(mean_mass_specific_intake_rate_dw),
      family = models_methods[[i]]$family,
      method = "REML",
      data = data
    )
    
    hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
    data_f <- filter(data, mgcv::influence.gam(mod) < hlt)
    
    mod = mgcv::gam(
      elemental_value ~ s(mean_mass_specific_intake_rate_dw),
      family = models_methods[[i]]$family,
      method = "REML",
      data = data_f
    )
    
    
    
    if (mod$converged == "TRUE") {
      gam_ch$n[i] <- broom::glance(mod)$nobs
      gam_ch$edf[i] <- format(signif(broom::tidy(mod)$edf, digits = 3), scientific = F) 
      gam_ch$`ref df`[i] <- format(signif(broom::tidy(mod)$ref.df, digits = 3), scientific = F)  
      gam_ch$`n parameters`[i] <- broom::glance(mod)$npar
      if (broom::tidy(mod)$p.value == 0) {
        gam_ch$p[i] <- "<2e-16"
      } else {
        gam_ch$p[i] <- format(signif((broom::tidy(mod)$p.value), digits = 2), scientific = T)
      }
      gam_ch$`Adjusted R^2`[i] <- format(signif(broom::glance(mod)$adj.r.squared, digits = 2), scientific = F) 
      gam_ch$Family[i] <- mod$family$family
      gam_ch$Link_function[i] <- mod$family$link
    }
    
  }
  
  # Save the table
  write.csv(gam_ch,
            file = here::here("4_outputs", "1_statistical_results", "gam_ch.csv"))
  
  # III. For isotopes #####
  
  # We wish to build models to test
  # The effect of growth rate on trophic fractionations
  # The effect of assimilation efficiency on the FLDF
  # The effect of mass-specific intake rate on IAER
  
  dependant_variables_list <- c("tf", "fldf", "iaer")
  independant_variables_list <- c(
    "geometric_mean_growth_dw",
    "assimilation_efficiency_dw",
    "mean_mass_specific_intake_rate_fw"
  )
  
  nb_dependant_variables <- length(dependant_variables_list)
  
  isotopes_list <- c("13C", "15N")
  nb_isotopes <- length(isotopes_list)
  
  
  # Creating a dataframe containing statistics for the publication
  
  # Column for isotope
  nb_row <- nb_isotopes * nb_dependant_variables
  
  
  n <- rep(NA, nb_row)
  formula <- rep(NA, nb_row)
  F_stat <- rep(NA, nb_row)
  R_squared <- rep(NA, nb_row)
  edf <- rep(NA, nb_row)
  equation <- rep(NA, nb_row)
  p_value <- rep(NA, nb_row)
  
  # Creating the dataframe
  models_isotopes <- data.frame(
    equation = equation,
    n = n,
    R_squared = R_squared,
    F_stat = F_stat,
    p_value = p_value
  )
  
  gam_isotopes <- data.frame(
    formula = formula,
    n = n,
    R_squared = R_squared,
    edf = edf,
    p_value = p_value
  )
  
  k <- 0
  
  for (i in 1:nb_dependant_variables) {
    data_variable <- subset(data_g, data_g$variable == dependant_variables_list[i])
    for (j in 1:nb_isotopes) {
      data_variable_isotope <- subset(
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
      
      formula_lm <- as.formula(paste("elemental_value", "~ ", independant_variables_list[i]))
      
      formula_gam <- as.formula(paste(
        "elemental_value",
        "~ ",
        "s",
        "(",
        independant_variables_list[i],
        ")"
      ))
      
      mod_linear <- lm(formula_lm, data = data_variable_isotope)
      summary_mod <- summary(mod_linear)
      mod_gam <- mgcv::gam(formula_gam, data = data_variable_isotope)
      summary_gam <- summary(mod_gam)
      
      k <- k + 1
      
      if (mod_gam$converged == "TRUE") {
        gam_isotopes$formula[k] <- paste(
          isotopes_list[j],
          dependant_variables_list[i],
          " = ",
          "a",
          "x",
          independant_variables_list[i],
          "+",
          "b"
        )
        gam_isotopes$n[k] <- summary_gam$n
        gam_isotopes$r_squared[k] <- format(signif(summary_gam$r.sq, digits = ), scientific = F)
        gam_isotopes$edf[k] <- format(signif(summary_gam$edf, digits = 3), scientific = F)
        if (summary_gam$s.pv == 0) {
          gam_isotopes$p_value[k] <- "<2e-16"
        } else {
          gam_isotopes$p_value[k] <- format(signif(summary_gam$s.pv, digits = 2), scientific = T)
        }
      }
      
      
      models_isotopes$equation[k] <- paste(
        isotopes_list[j],
        dependant_variables_list[i],
        " = ",
        signif(summary_mod$coefficients[2, 1], digits = 2),
        "x",
        independant_variables_list[i],
        "+",
        signif(summary_mod$coefficients[1, 1], digits = 5)
      )
      
      models_isotopes$n[k] <- length(data_variable_isotope$elemental_value) - sum(is.na(data_variable_isotope$elemental_value))
      models_isotopes$F_stat[k] <- signif(summary_mod$fstatistic[1], digits =
                                            2)
      models_isotopes$p_value[k] <- scales::pvalue(
        summary_mod$coefficients[2, 4],
        accuracy = 0.01,
        # Number to round to
        decimal.mark = ".",
        # The character to be used to indicate the numeric decimal point
        add_p = TRUE
      )
      
      models_isotopes$R_squared[k] <- signif(summary_mod$r.squared, digits = 2)
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
    file = here::here("4_outputs", "1_statistical_results", "gam_isotopes.csv")
  )
  
}
