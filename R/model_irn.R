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
    "Adjusted R$^2$" = adj_r_squared,
    Family = family,
    Smoother = smoother
  )
  
  
  ## 1. The models ######
  
  mod_msgrdw_msirdw <- mgcv::gam(
    mean_growth_dw ~ s(mass_specific_ingestion_rate_fw),
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
    growth_efficiency_dw ~ s(mean_growth_dw, bs = "ad", k = 10),
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
    call <- models[[i]]$call
    call$data <- quote(data_i_f)
    models[[i]] <- eval(call)
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
      gam_tm$`Adjusted R$^2$`[i] <- format(signif(broom::glance(models[[i]])$adj.r.squared, digits = 2),
                                           scientific = F)
      gam_tm$Family[i] <- models[[i]]$family$family
    }
  }
  
  
  
  # Save the table
  
  kbl(
    gam_tm,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    caption = "Summaries of the four GAMs plotted in fig. \\ref{fig_massbalance}. MSIR stand for mass-specific intake rate, GR for growth rate, GE for growth efficiency, and AE for assimilation efficiency, edf for effective degrees of freedom. TP refers to thin-plate splines, and AD to adaptive splines. The parameter $\\phi$ for Beta regressions is indicated in parentheses \\citep{Wood2016}",
    label = "table_gam_tm"
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "gam_tm.tex"))
  
  # II. Chemical mass balance models ######
  
  ## 1. General models ####
  
  # The elements
  elements_list <- c("C", "N", "P", "Na", "Mg", "S", "K", "Ca")
  nb_elements <- length(elements_list)
  
  ### i. Assimilation efficiency ####
  
  data_ae <- filter(
    data_g,
    variable == "assimilation_efficiency_dw",
    element %in% elements_list,
    !is.na(elemental_value)
  )
  
  data_ae$element <- as.factor(data_ae$element)
  gam_ch <- mgcv::gam(
    elemental_value ~ element + s(mean_mass_specific_intake_rate_dw, by = element),
    method = "REML",
    data = data_ae,
    family = betar()
  )
  
  # Predictions
  pairs_i_ae <- avg_predictions(gam_ch, by = "element", hypothesis = "pairwise") |>
    print(style = "tinytable") |>
    print(output = "dataframe")
  p_vals <- pairs_i_ae$`Pr(>|z|)` == "< 0.001" |
    as.numeric(pairs_i_ae$`Pr(>|z|)`) < 0.05
  
  kbl(
    pairs_i_ae,
    format = "latex",
    booktabs = T,
    escape = T,
    linesep = "",
    align = "lrrrrrrr",
    caption = "Pairwise comparisons of elemental absorption efficiencies predicted by the general GAM model. From left to right: elements being compared, the average difference is their estimate, the standard error associated, the z value for the test followed by the p- and the S- values. Finally, the confidence interval of the difference.",
    label = "table_pairs_i_ae"
  ) |>
    row_spec(0, bold = TRUE) |>
    column_spec(c(1, 5), bold = p_vals) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position")) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "pairs_i_ae.tex"))
  
  # Derivatives
  pairs_d_ae <- avg_slopes(gam_ch,
                           by = "element",
                           variables = "mean_mass_specific_intake_rate_dw",
                           hypothesis = "pairwise") |>
    print(style = "tinytable") |>
    print(output = "dataframe")
  
  p_vals <- pairs_d_ae$`Pr(>|z|)` == "< 0.001" |
    as.numeric(pairs_d_ae$`Pr(>|z|)`) < 0.05
  
  kbl(
    pairs_d_ae,
    format = "latex",
    booktabs = T,
    escape = T,
    linesep = "",
    align = "lrrrrrrr",
    caption = "Pairwise comparisons of the effect of intake rate on elemental absorption efficiencies predicted by the general GAM model. From left to right: elements being compared, the average difference in the slopes (see fig \\ref{fig_deriv_asmeffall}), the standard error associated, the z value for the test followed by the p- and the S- values. Finally, the confidence interval of the difference.",
    label = "table_pairs_d_ae"
  ) |>
    row_spec(0, bold = TRUE) |>
    column_spec(c(1, 5), bold = p_vals) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position")) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "pairs_d_ae.tex"))
  
  
  
  ### ii. Retention times ####
  
  data_rt <- filter(
    data_g,
    variable == "retention_time",
    element %in% elements_list,
    !is.na(elemental_value)
  )
  data_rt$element <- as.factor(data_rt$element)
  
  # Predictions
  
  gam_ch <- mgcv::gam(
    elemental_value ~ s(mean_mass_specific_intake_rate_dw, by = element) + element,
    method = "REML",
    family = Gamma(link = "log"),
    data = data_rt
  )
  
  pairs_i_rt <- avg_predictions(gam_ch, by = "element", hypothesis = "pairwise") |>
    print(style = "tinytable") |>
    print(output = "dataframe")
  
  p_vals <- pairs_i_rt$`Pr(>|z|)` == "<0.001" |
    as.numeric(pairs_i_rt$`Pr(>|z|)`) < 0.05
  
  kbl(
    pairs_i_rt,
    format = "latex",
    booktabs = T,
    escape = T,
    linesep = "",
    align = "lrrrrrrr",
    caption = "Pairwise comparisons of elemental retention times predicted by the general GAM model. From left to right: elements being compared, the average difference is their estimate, the standard error associated, the z value for the test followed by the p- and the S- values. Finally, the confidence interval of the difference.",
    label = "table_pairs_i_rt"
  ) |>
    row_spec(0, bold = TRUE) |>
    column_spec(c(1, 5), bold = p_vals) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position")) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "pairs_i_rt.tex"))
  
  # Derivatives
  pairs_d_rt <- avg_slopes(gam_ch,
                           by = "element",
                           variables = "mean_mass_specific_intake_rate_dw",
                           hypothesis = "pairwise") |>
    print(style = "tinytable") |>
    print(output = "dataframe")
  
  p_vals <- pairs_d_rt$`Pr(>|z|)` == "< 0.001" |
    as.numeric(pairs_d_rt$`Pr(>|z|)`) < 0.05
  
  kbl(
    pairs_d_rt,
    format = "latex",
    booktabs = T,
    escape = T,
    linesep = "",
    align = "lrrrrrrr",
    caption = "Pairwise comparisons of the effect of intake rate on elemental retention times predicted by the general GAM model. From left to right: elements being compared, the average difference in the slopes (see fig \\ref{fig_deriv_rettimesall}), the standard error associated, the z value for the test followed by the p- and the S- values. Finally, the confidence interval of the difference.",
    label = "table_pairs_d_rt"
  ) |>
    row_spec(0, bold = TRUE) |>
    column_spec(c(1, 5), bold = p_vals) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position")) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "pairs_d_rt.tex"))
  
  
  
  ## 2. Element-wise models ####
  # The variables in the chemical mass balance study
  
  responses_list_ch <- c("larvae",
                         "frass",
                         "assimilation_efficiency_dw",
                         "retention_time") # The variables in the chemical study
  
  responses_list_ch_nice <- c("Larvae", "Frass", "AE", "RT")
  nb_responses_ch <- length(responses_list_ch)
  
  
  
  nb_models_ch <- nb_responses_ch * nb_elements
  
  # Creating a dataframe containing GAM statistics for total mass balance
  elements <- rep(elements_list, nb_responses_ch)
  response <- rep(responses_list_ch, each = nb_elements)
  response_nice <- rep(responses_list_ch_nice, each = nb_elements)
  n <- rep(NA, nb_models_ch)
  adj_r_squared <- rep(NA, nb_models_ch)
  n_par <- rep(NA, nb_models_ch)
  edf <- rep(NA, nb_models_ch)
  ref_df <- rep(NA, nb_models_ch)
  p_value <- rep(NA, nb_models_ch)
  family <- rep(NA, nb_models_ch)
  smoother <- rep("TP", nb_models_ch)
  link_function <- rep(NA, nb_models_ch)
  gam_ch_table <- tibble(
    Response = response_nice,
    Element = elements,
    n = n,
    edf = edf,
    "ref df" = ref_df,
    "n parameters" = n_par,
    p = p_value,
    "Adjusted R$^2$" = adj_r_squared,
    Family = family,
    "Link function" = link_function,
    Smoother = smoother
  )
  gam_ch_list <- vector(mode = "list", length = nb_models_ch)
  models_methods <- c(
    replicate(nb_elements, list(family = gaussian()), simplify = FALSE),
    replicate(nb_elements, list(family = gaussian()), simplify = FALSE),
    replicate(nb_elements, list(family = betar()), simplify = FALSE),
    replicate(nb_elements, list(family = Gamma(link = log)), simplify = FALSE)
  )
  
  for (i in 1:length(models_methods)) {
    data <- filter(data_g,
                   element == elements[i],
                   variable == response[i],
                   !is.na(elemental_value))
    
    gam_ch_list[[i]] <- mgcv::gam(
      elemental_value ~ s(mean_mass_specific_intake_rate_dw),
      family = models_methods[[i]]$family,
      method = "REML",
      data = data
    )
    
    hlt <- 10 * sum(mgcv::influence.gam(gam_ch_list[[i]]) / length(mgcv::influence.gam(gam_ch_list[[i]])))
    data_f <- filter(data, mgcv::influence.gam(gam_ch_list[[i]]) < hlt)
    
    gam_ch_list[[i]] <- mgcv::gam(
      elemental_value ~ s(mean_mass_specific_intake_rate_dw),
      family = models_methods[[i]]$family,
      method = "REML",
      data = data_f
    )
    
    
    
    if (gam_ch_list[[i]]$converged == "TRUE") {
      gam_ch_table$n[i] <- broom::glance(gam_ch_list[[i]])$nobs
      gam_ch_table$edf[i] <- format(signif(broom::tidy(gam_ch_list[[i]])$edf, digits = 3), scientific = F)
      gam_ch_table$`ref df`[i] <- format(signif(broom::tidy(gam_ch_list[[i]])$ref.df, digits = 3), scientific = F)
      gam_ch_table$`n parameters`[i] <- broom::glance(gam_ch_list[[i]])$npar
      if (broom::tidy(gam_ch_list[[i]])$p.value == 0) {
        gam_ch_table$p[i] <- "<2e-16"
      } else {
        gam_ch_table$p[i] <- format(signif((
          broom::tidy(gam_ch_list[[i]])$p.value
        ), digits = 2), scientific = T)
      }
      gam_ch_table$`Adjusted R$^2$`[i] <- format(signif(broom::glance(gam_ch_list[[i]])$adj.r.squared, digits = 2),
                                                 scientific = F)
      gam_ch_table$Family[i] <- gam_ch_list[[i]]$family$family
      gam_ch_table$`Link function`[i] <- gam_ch_list[[i]]$family$link
    }
  }
  
  
  # Save the table
  
  # Remove redundant variables
  gam_ch_table <- select(gam_ch_table, !c("n parameters", "Smoother"))
  kbl(
    gam_ch_table,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    caption = "Summaries of element-wise GAM models used to produce the plots in figs. \\ref{fig_eff_times}, \\ref{fig_asmeffall}, \\ref{fig_rettimesall}, \\ref{fig_larvaecontentall} and \\ref{fig_frasscontentall}. AE stands for assimilation efficiency, and RT for retention time, edf for effective degrees of freedom. All models were fitted using thin-plate splines and with 10 parameters. The parameter $\\phi$ for Beta regressions is indicated in parentheses \\citep{Wood2016}",
    label = "table_gam_ch"
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    collapse_rows(columns = c(1, 8, 9)) |>
    save_kable(here::here("4_outputs", "1_statistical_results", "gam_ch_table.tex"))
  
  
  # III. For isotopes #####
  
  # We wish to build models to test
  # The effect of growth rate on trophic fractionations
  # The effect of assimilation efficiency on the FLDF
  # The effect of mass-specific intake rate on IAER
  
  dependant_variables_list <- c("tf", "fldf", "iaer")
  independant_variables_list <- c(
    "mean_growth_dw",
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
        gam_isotopes$r_squared[k] <- format(signif(summary_gam$r.sq, digits =), scientific = F)
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
