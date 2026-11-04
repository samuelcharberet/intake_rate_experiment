#' Choice of the distribution family and family sensitivity analysis
#'
#'
#' Every model is fitted with each family (and link function) compatible with
#' the support of the data, on all observations and with the same smoother,
#' so that only the family differs. For each response variable, the family
#' with the lowest total AIC across its models (e.g. summed over the eight
#' elements) is selected, among the families that model the mean of the
#' response on its original scale (the log-normal model, which models the
#' median of log-transformed retention times, is only used as a sensitivity
#' check). The same fits show whether the family drives the shapes of the
#' fitted relationships (Figs 1 and 3).
#'
#' The model specifications are those of model_irn() and plot_irn(): the
#' individual-level models of Fig. 1 and fig. S6a (penalised cubic regression
#' splines with 5 knots at Harrell's quantiles, linear beyond the outer knots,
#' see harrell_knots_irn()) and the element-wise models (thin plate splines,
#' k = -1, mass-specific intake rate in dry weight). knots_sensitivity_irn()
#' checks that the conclusions of the individual-level models do not depend
#' on the number of knots.
#'
#' Entry points
#'   run_sensitivity_irn()          fits all models, returns tidy data frames
#'   save_sensitivity_tables_irn()  writes the LaTeX tables (called in model_irn())
#'   plot_sensitivity_irn()         writes the PDF figures (called in plot_irn())
#'
#' Derivatives are computed exactly as gratia::derivatives(type = "central",
#' interval = "confidence") does (central finite differences of the linear
#' predictor matrix, pointwise 95% confidence interval), but directly with mgcv
#' so that the results do not depend on gratia's output format.


# 1. Model specifications ####

#' Knots of the cubic regression splines used for the individual-level models.
#' With knots inside the range of the data, mgcv's "cr" basis is a natural cubic
#' spline: linear beyond the outer knots, so that the few individuals at the
#' extremes of the gradient cannot bend the ends of the curves. Quantiles are
#' Harrell's (2015, Regression Modeling Strategies) defaults for 3-7 knots, and
#' evenly spaced between the 5th and 95th percentiles for more knots.
harrell_knots_irn <- function(x, k = 5) {
  q <- switch(
    as.character(k),
    "3" = c(0.10, 0.5, 0.90),
    "4" = c(0.05, 0.35, 0.65, 0.95),
    "5" = c(0.05, 0.275, 0.5, 0.725, 0.95),
    "6" = c(0.05, 0.23, 0.41, 0.59, 0.77, 0.95),
    "7" = c(0.025, 0.1833, 0.3417, 0.5, 0.6583, 0.8167, 0.975),
    seq(0.05, 0.95, length.out = k)
  )
  unname(stats::quantile(x, q, na.rm = TRUE))
}

#' One row per model: the 3 bulk mass models of Fig. 1 and the 32 element-wise
#' models. `expected` is the conclusion stated in the main text.
sensitivity_specs_irn <- function() {
  elements <- c("C", "N", "P", "Na", "Mg", "S", "K", "Ca")

  mass_balance <- tibble::tibble(
    block = "Mass balance",
    response = c(
      "assimilation_efficiency_dw",
      "growth_efficiency_dw",
      "growth_efficiency_dw"
    ),
    response_label = c("AE", "GE", "GE"),
    element = NA_character_,
    term_label = c("MSIR", "MSIR", "GR"),
    predictor = c(
      "mass_specific_ingestion_rate_dw",
      "mass_specific_ingestion_rate_dw",
      "mean_growth_dw"
    ),
    dataset = "individuals",
    bs = "cr",
    family_set = "efficiency_individual",
    expected = c("decreasing", "humped", "humped")
  )

  # Growth rate as a function of intake rate (derivative in fig. S6a)
  growth_rate <- tibble::tibble(
    block = "Mass balance",
    response = "mean_growth_dw",
    response_label = "GR",
    element = NA_character_,
    term_label = "MSIR",
    predictor = "mass_specific_ingestion_rate_dw",
    dataset = "individuals",
    bs = "cr",
    family_set = "growth_rate",
    expected = "increasing"
  )

  response_labels <- c(
    larvae = "Larvae",
    frass = "Frass",
    assimilation_efficiency_dw = "AE",
    retention_time = "RT"
  )
  family_sets <- c(
    larvae = "larvae_content",
    frass = "frass_content",
    assimilation_efficiency_dw = "efficiency_element",
    retention_time = "retention_time"
  )

  elementwise <- tidyr::expand_grid(response = names(response_labels), element = elements)
  elementwise$block <- "Elements"
  elementwise$response_label <- unname(response_labels[elementwise$response])
  elementwise$term_label <- elementwise$element
  elementwise$predictor <- "mean_mass_specific_intake_rate_dw"
  elementwise$dataset <- "groups"
  elementwise$bs <- "tp"
  elementwise$family_set <- unname(family_sets[elementwise$response])
  # Main text: larvae richer in C but poorer in all other elements at high intake,
  # frass richer in all elements at high intake, AE and RT decrease with intake
  elementwise$expected <- dplyr::case_when(
    elementwise$response == "larvae" & elementwise$element == "C" ~ "increasing",
    elementwise$response == "larvae" ~ "decreasing",
    elementwise$response == "frass" ~ "increasing",
    TRUE ~ "decreasing"
  )

  specs <- dplyr::bind_rows(mass_balance, growth_rate, elementwise)
  # The family is selected per response type (one family per response variable)
  specs$response_type <- ifelse(
    specs$block == "Mass balance",
    paste("Bulk", specs$response_label),
    unname(c(Larvae = "Larvae content", Frass = "Frass content", AE = "Elemental AE", RT = "RT")[specs$response_label])
  )
  specs$id <- seq_len(nrow(specs))
  specs
}

#' Candidate families for each type of response variable. The first element
#' of each list is the family used in the paper (the one selected by AIC, see
#' select_families_irn()). Only families compatible with the support of the
#' data are included (all efficiencies lie strictly within (0, 1); growth
#' rates, retention times and contents are strictly positive).
sensitivity_families_irn <- function(family_set) {
  fam <- function(label, family, log_response = FALSE) {
    list(label = label, family = family, log_response = log_response)
  }
  switch(
    family_set,
    # Individual-level AE and GE: continuous proportions with heavy-tailed residuals
    efficiency_individual = list(
      fam("Scaled t (identity)", mgcv::scat()),
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Beta (logit)", mgcv::betar(link = "logit"))
    ),
    # Individual-level growth rate
    growth_rate = list(
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Scaled t (identity)", mgcv::scat()),
      fam("Gamma (log)", stats::Gamma(link = "log"))
    ),
    # Group-level elemental AE: continuous proportions
    efficiency_element = list(
      fam("Beta (logit)", mgcv::betar(link = "logit")),
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Scaled t (identity)", mgcv::scat())
    ),
    # Retention times: strictly positive, right-skewed, variance increasing with the mean
    retention_time = list(
      fam("Gamma (log)", stats::Gamma(link = "log")),
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Gaussian (log)", stats::gaussian(link = "log")),
      fam("Inverse Gaussian (log)", stats::inverse.gaussian(link = "log")),
      fam("Log-normal", stats::gaussian(), log_response = TRUE)
    ),
    # Larvae and frass elemental contents: positive and far from zero
    larvae_content = list(
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Scaled t (identity)", mgcv::scat()),
      fam("Gamma (log)", stats::Gamma(link = "log"))
    ),
    frass_content = list(
      fam("Scaled t (identity)", mgcv::scat()),
      fam("Gaussian (identity)", stats::gaussian()),
      fam("Gamma (log)", stats::Gamma(link = "log"))
    )
  )
}


# 2. Fitting and assessment helpers ####

#' Extract the data of one model as columns x (predictor) and y (response)
sensitivity_data_irn <- function(spec, data_i, data_g) {
  if (spec$dataset == "individuals") {
    df <- data.frame(x = data_i[[spec$predictor]], y = data_i[[spec$response]])
  } else {
    sub <- data_g[data_g$variable == spec$response &
                    data_g$element == spec$element &
                    !is.na(data_g$elemental_value), ]
    df <- data.frame(x = sub[[spec$predictor]], y = sub$elemental_value)
  }
  df[stats::complete.cases(df), ]
}

#' Fit one GAM, y ~ s(x), with the smoother used in the paper: cubic
#' regression spline with linear tails (k knots, harrell_knots_irn()) for
#' bs = "cr", default basis dimension otherwise
fit_sensitivity_gam_irn <- function(df, bs, fam, k = 5) {
  df$y_model <- if (isTRUE(fam$log_response)) log(df$y) else df$y
  if (bs == "cr") {
    mgcv::gam(stats::as.formula(sprintf("y_model ~ s(x, bs = 'cr', k = %d)", k)),
              knots = list(x = harrell_knots_irn(df$x, k)),
              family = fam$family,
              method = "REML",
              data = df)
  } else {
    mgcv::gam(stats::as.formula(sprintf("y_model ~ s(x, bs = '%s', k = -1)", bs)),
              family = fam$family,
              method = "REML",
              data = df)
  }
}

#' First derivative of the smooth on the link scale with a pointwise CI
#' (equivalent to gratia::derivatives(type = "central", interval = "confidence"))
derivatives_irn <- function(model,
                            n = 200,
                            eps = 1e-7,
                            level = 0.95) {
  x <- seq(min(model$model$x), max(model$model$x), length.out = n)
  X_up <- mgcv::predict.gam(model, data.frame(x = x + eps / 2), type = "lpmatrix")
  X_lo <- mgcv::predict.gam(model, data.frame(x = x - eps / 2), type = "lpmatrix")
  X_d <- (X_up - X_lo) / eps
  d <- drop(X_d %*% stats::coef(model))
  se <- sqrt(rowSums((X_d %*% stats::vcov(model)) * X_d))
  crit <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(
    x = x,
    derivative = d,
    lower = d - crit * se,
    upper = d + crit * se
  )
}

#' Summarise the sequence of significantly positive (+) and negative (-)
#' portions of the derivative. All links used here are increasing functions,
#' so the sign on the link scale is the sign on the response scale.
shape_irn <- function(deriv) {
  sign <- ifelse(deriv$lower > 0, "+", ifelse(deriv$upper < 0, "-", "0"))
  sign <- sign[sign != "0"]
  code <- if (length(sign) == 0) "" else paste(rle(sign)$values, collapse = "")
  label <- if (code == "") {
    "no trend"
  } else {
    switch(
      code,
      "-" = "decreasing",
      "+" = "increasing",
      "+-" = "humped",
      "-+" = "U-shaped",
      "+-+" = "humped, then increasing",
      "-+-" = "U-shaped, then decreasing",
      "non-monotonic"
    )
  }
  list(code = code, label = label)
}

#' Does the fitted model support the conclusion stated in the main text?
#' Requires a significant smooth (p < alpha) and a matching derivative pattern:
#' - decreasing / increasing: significant slope of the expected sign somewhere
#'   and nowhere significant slope of the opposite sign;
#' - humped: a significantly positive portion followed by a significantly
#'   negative one (as stated in the Results).
conclusion_holds_irn <- function(code, p, expected, alpha = 0.05) {
  shape_ok <- switch(
    expected,
    decreasing = code == "-",
    increasing = code == "+",
    humped = grepl("+-", code, fixed = TRUE)
  )
  isTRUE(p < alpha) && isTRUE(shape_ok)
}

#' Predictor value at which the fitted curve is maximal (monotone links, so the
#' maximum on the link scale is the maximum on the response scale)
peak_irn <- function(model, n = 500) {
  x <- seq(min(model$model$x), max(model$model$x), length.out = n)
  eta <- mgcv::predict.gam(model, data.frame(x = x), type = "link")
  x[which.max(eta)]
}

#' AIC on the scale of the original response (Jacobian correction for the
#' log-normal model, so that all families are compared on the same data)
aic_irn <- function(model, fam) {
  aic <- stats::AIC(model)
  if (isTRUE(fam$log_response)) {
    aic <- aic + 2 * sum(model$model$y_model)
  }
  aic
}

#' One-row summary of a fitted model
summarise_gam_irn <- function(model, fam, expected) {
  if (is.null(model)) {
    return(tibble::tibble(
      n = NA_integer_, edf = NA_real_, p = NA_real_, shape = "not fitted",
      code = NA_character_, holds = NA, peak = NA_real_, aic = NA_real_,
      converged = FALSE
    ))
  }
  s <- summary(model)
  p <- s$s.pv[1]
  sh <- shape_irn(derivatives_irn(model))
  tibble::tibble(
    n = nrow(model$model),
    edf = sum(s$edf),
    p = p,
    shape = sh$label,
    code = sh$code,
    holds = conclusion_holds_irn(sh$code, p, expected),
    peak = if (expected == "humped") peak_irn(model) else NA_real_,
    aic = aic_irn(model, fam),
    converged = isTRUE(model$converged)
  )
}

#' Fitted mean (median for the log-normal model) and 95% CI on the response
#' scale, computed as geom_smooth() does for GAMs (link scale, then inverse link)
curve_irn <- function(model, fam, n = 200, level = 0.95) {
  x <- seq(min(model$model$x), max(model$model$x), length.out = n)
  pr <- mgcv::predict.gam(model, data.frame(x = x), type = "link", se.fit = TRUE)
  crit <- stats::qnorm(1 - (1 - level) / 2)
  inv <- if (isTRUE(fam$log_response)) exp else model$family$linkinv
  data.frame(
    x = x,
    fit = inv(pr$fit),
    lower = inv(pr$fit - crit * pr$se.fit),
    upper = inv(pr$fit + crit * pr$se.fit)
  )
}


# 3. Run the family sensitivity analysis ####

#' Fits every model of sensitivity_specs_irn() with every candidate family, on
#' all observations, so that only the family differs between fits, and selects
#' one family per response variable (select_families_irn()).
#' Returns data frames only (no model objects), to keep the targets store small.
run_sensitivity_irn <- function(data_i, data_g) {
  specs <- sensitivity_specs_irn()

  families <- list()
  curves <- list()
  points <- vector("list", nrow(specs))

  safe_fit <- function(df, bs, fam) {
    tryCatch(
      suppressWarnings(fit_sensitivity_gam_irn(df, bs, fam)),
      error = function(e) NULL
    )
  }

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    df <- sensitivity_data_irn(spec, data_i, data_g)
    fams <- sensitivity_families_irn(spec$family_set)
    points[[i]] <- tibble::tibble(id = spec$id, x = df$x, y = df$y)

    for (j in seq_along(fams)) {
      fam <- fams[[j]]
      mod <- safe_fit(df, spec$bs, fam)
      families[[length(families) + 1]] <- dplyr::bind_cols(
        spec,
        tibble::tibble(family = fam$label, family_rank = j, reference = j == 1, log_response = fam$log_response),
        summarise_gam_irn(mod, fam, spec$expected)
      )
      if (!is.null(mod)) {
        curves[[length(curves) + 1]] <- dplyr::mutate(
          curve_irn(mod, fam),
          id = spec$id,
          family = fam$label,
          reference = j == 1
        )
      }
    }
  }

  families <- dplyr::bind_rows(families) |>
    dplyr::group_by(id) |>
    dplyr::mutate(delta_aic = aic - min(aic, na.rm = TRUE)) |>
    dplyr::ungroup()

  list(
    specs = specs,
    families = families,
    selection = select_families_irn(families),
    knots = knots_sensitivity_irn(specs, data_i, data_g),
    curves = dplyr::bind_rows(curves),
    points = dplyr::bind_rows(points)
  )
}

#' Do the conclusions of the individual-level models depend on the number of
#' knots of their cubic regression splines? Refits each of them, with the
#' family used in the paper, with 4 to 10 knots.
knots_sensitivity_irn <- function(specs, data_i, data_g, ks = 4:10) {
  out <- list()
  for (i in which(specs$bs == "cr")) {
    spec <- specs[i, ]
    df <- sensitivity_data_irn(spec, data_i, data_g)
    fam <- sensitivity_families_irn(spec$family_set)[[1]]
    for (k in ks) {
      mod <- fit_sensitivity_gam_irn(df, "cr", fam, k = k)
      out[[length(out) + 1]] <- dplyr::bind_cols(
        spec[, c("id", "response_label", "term_label", "expected")],
        tibble::tibble(knots = k),
        summarise_gam_irn(mod, fam, spec$expected)
      )
    }
  }
  dplyr::bind_rows(out) |>
    dplyr::group_by(id) |>
    dplyr::mutate(delta_aic = aic - min(aic)) |>
    dplyr::ungroup()
}

#' Total AIC of each family per response variable (summed over the models of
#' that variable, e.g. the eight elements) and selected family: the lowest
#' total AIC among the families that model the mean of the response on its
#' original scale (i.e. excluding the log-normal model). Warns if the selected
#' family is not the one used in model_irn() and plot_irn().
select_families_irn <- function(families) {
  selection <- families |>
    dplyr::group_by(response_type, family, family_rank, reference, log_response) |>
    dplyr::summarise(
      n_models = dplyr::n(),
      total_aic = sum(aic),
      n_holds = sum(holds, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(response_type) |>
    dplyr::mutate(
      delta_total_aic = total_aic - min(total_aic, na.rm = TRUE),
      selected = !log_response &
        total_aic == min(total_aic[!log_response], na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  mismatch <- selection$response_type[selection$selected & !selection$reference]
  if (length(mismatch) > 0) {
    warning(
      "The family selected by AIC differs from the one used in the paper for: ",
      paste(mismatch, collapse = ", ")
    )
  }
  selection
}


# 4. Tables ####

format_p_irn <- function(p) {
  ifelse(is.na(p), "--", ifelse(p < 2e-16, "<2e-16", format(signif(p, digits = 2), scientific = TRUE)))
}

format_shape_irn <- function(shape, peak) {
  ifelse(is.na(peak), shape, sprintf("%s (%.3g)", shape, peak))
}

yes_no_irn <- function(x) {
  ifelse(is.na(x), "--", ifelse(x, "Yes", "No"))
}

#' Writes family_selection.tex, family_sensitivity_tm.tex,
#' family_sensitivity_ch.tex and knots_sensitivity.tex in `path`, in the same
#' style as the other tables
save_sensitivity_tables_irn <- function(sensitivity, path) {
  fam <- sensitivity$families

  ## Number of knots of the individual-level models ####
  kn <- sensitivity$knots
  kn$cell <- sprintf("%s (%.1f)", yes_no_irn(kn$holds), kn$delta_aic)
  knots_table <- kn |>
    dplyr::mutate(Model = paste(response_label, "$\\sim$", term_label), knots = paste(knots, "knots")) |>
    dplyr::select(Model, knots, cell) |>
    tidyr::pivot_wider(names_from = knots, values_from = cell)

  kbl(
    knots_table,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    align = paste0("l", strrep("c", ncol(knots_table) - 1)),
    caption = paste(
      "\\label{supptable:table_knots_sensitivity} Sensitivity of the individual-level models (fig. \\ref{fig_massbalance} and fig. \\ref{derivatives}a) to the number of knots of their cubic regression splines (5 knots in the main analyses).",
      "Each cell indicates whether the conclusion stated in the main text holds (see table \\ref{supptable:table_family_sensitivity_tm}) and, in parentheses, the difference in AIC with the best number of knots for the same model.",
      "Knots were placed at the quantiles recommended by \\citet{Harrell2015} for 4 to 7 knots and evenly between the 5th and 95th percentiles for 8 to 10 knots; the splines are linear beyond the outer knots.",
      "MSIR stands for mass-specific intake rate, GR for growth rate, AE for assimilation efficiency and GE for growth efficiency."
    ),
    label = NULL
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    save_kable(file.path(path, "knots_sensitivity.tex"))

  ## Selection: one row per response variable x family ####
  type_order <- c("Bulk AE", "Bulk GE", "Bulk GR", "Elemental AE", "RT", "Larvae content", "Frass content")
  sel <- sensitivity$selection |>
    dplyr::mutate(response_type = factor(response_type, levels = type_order)) |>
    dplyr::arrange(response_type, delta_total_aic)
  selection_table <- tibble::tibble(
    Response = as.character(sel$response_type),
    Family = ifelse(sel$selected, paste0("\\textbf{", sel$family, "}"), sel$family),
    Models = sel$n_models,
    "$\\Delta$AIC (total)" = sprintf("%.1f", sel$delta_total_aic),
    "Conclusions supported" = paste0(sel$n_holds, "/", sel$n_models)
  )

  kbl(
    selection_table,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    align = "llrrr",
    caption = paste(
      "\\label{supptable:table_family_selection} Selection of the distribution family and link function of each response variable.",
      "Each model was fitted to all observations with every family compatible with the support of the data, with the same smoother.",
      "$\\Delta$AIC (total): difference with the lowest AIC summed over the models of the same response variable (one model for bulk AE and GR, two for bulk GE, one per element otherwise).",
      "The selected family, used in the main analyses, is in bold: the lowest total AIC among the families that model the mean of the response on its original scale.",
      "The log-normal model (Gaussian model of log-transformed retention times, AIC corrected by the Jacobian of the transformation) models the median retention time and was only used as a sensitivity check; it was within 2 AIC units of the gamma family.",
      "Conclusions supported: number of models for which the conclusion stated in the main text holds (see table \\ref{supptable:table_family_sensitivity_tm}).",
      "AE stands for assimilation efficiency, GE for growth efficiency, GR for growth rate and RT for retention time."
    ),
    label = NULL
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    collapse_rows(columns = 1) |>
    save_kable(file.path(path, "family_selection.tex"))

  shape_definition <- paste(
    "Shape: pattern of the first derivative of the fitted smooth (pointwise 95\\% confidence interval); decreasing (increasing) when the derivative is significantly negative (positive) over part of the range and never significantly positive (negative); humped when a significantly positive portion precedes a significantly negative one, the predictor value at the fitted maximum being given in parentheses.",
    "Holds: the smooth term is significant ($p<0.05$) and its shape matches the conclusion stated in the main text."
  )

  ## Bulk mass models: one row per model x family ####
  fam_tm <- fam[fam$block == "Mass balance", ]
  family_table_tm <- tibble::tibble(
    Model = paste(fam_tm$response_label, "$\\sim$", fam_tm$term_label),
    Family = ifelse(fam_tm$reference, paste0("\\textbf{", fam_tm$family, "}"), fam_tm$family),
    n = fam_tm$n,
    edf = format(signif(fam_tm$edf, digits = 3), scientific = F),
    p = format_p_irn(fam_tm$p),
    Shape = format_shape_irn(fam_tm$shape, fam_tm$peak),
    Holds = yes_no_irn(fam_tm$holds),
    "$\\Delta$AIC" = sprintf("%.1f", fam_tm$delta_aic)
  )

  kbl(
    family_table_tm,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    align = "llrrllll",
    caption = paste(
      "\\label{supptable:table_family_sensitivity_tm} Bulk (total) mass models of fig. \\ref{fig_massbalance} and growth rate model of fig. \\ref{derivatives}a fitted with alternative distribution families and link functions (see fig. \\ref{fig_family_sensitivity_tm}).",
      "The family selected by AIC (table \\ref{supptable:table_family_selection}) and used in the main analyses is in bold.",
      "For each model, all fits used all observations and the same smoother, so that only the family differs.",
      shape_definition,
      "$\\Delta$AIC is the difference in AIC with the best family for the same model.",
      "MSIR stands for mass-specific intake rate, GR for growth rate, AE for assimilation efficiency and GE for growth efficiency."
    ),
    label = NULL
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    collapse_rows(columns = 1) |>
    save_kable(file.path(path, "family_sensitivity_tm.tex"))

  ## Element-wise models: rows = response x family, columns = elements ####
  fam_ch <- fam[fam$block == "Elements", ]
  fam_ch$cell <- ifelse(
    is.na(fam_ch$holds),
    "--",
    sprintf("%s (%.1f)", yes_no_irn(fam_ch$holds), fam_ch$delta_aic)
  )
  fam_ch$family_label <- ifelse(fam_ch$reference, paste0("\\textbf{", fam_ch$family, "}"), fam_ch$family)
  family_table_ch <- fam_ch |>
    dplyr::mutate(
      Response = factor(response_label, levels = c("Larvae", "Frass", "AE", "RT")),
      Family = family_label,
      element = factor(element, levels = c("C", "N", "P", "Na", "Mg", "S", "K", "Ca"))
    ) |>
    dplyr::select(Response, family_rank, Family, element, cell) |>
    tidyr::pivot_wider(names_from = element, values_from = cell) |>
    dplyr::arrange(Response, family_rank) |>
    dplyr::select(-family_rank) |>
    dplyr::mutate(Response = as.character(Response))

  kbl(
    family_table_ch,
    format = "latex",
    booktabs = T,
    escape = F,
    linesep = "",
    align = "llcccccccc",
    caption = paste(
      "\\label{supptable:table_family_sensitivity_ch} Element-wise models fitted with alternative distribution families and link functions (see figs \\ref{fig_family_sensitivity_ae} and \\ref{fig_family_sensitivity_rt}).",
      "Each cell indicates whether the conclusion stated in the main text holds (Yes/No) and, in parentheses, the difference in AIC with the best family for the same response and element.",
      "Holds: the smooth term is significant ($p<0.05$) and the pattern of its first derivative matches the conclusion (see table \\ref{supptable:table_family_sensitivity_tm}).",
      "The family selected by AIC (table \\ref{supptable:table_family_selection}) and used in the main analyses is in bold.",
      "All models were fitted to all observations with the same thin plate smoother, so that only the family differs.",
      "Log-normal: Gaussian model of the log-transformed retention time (AIC corrected by the Jacobian of the transformation).",
      "AE stands for assimilation efficiency and RT for retention time."
    ),
    label = NULL
  ) |>
    row_spec(0, bold = TRUE) |>
    kable_styling(position = "center",
                  latex_options = c("HOLD_position", "scale_down")) |>
    collapse_rows(columns = 1) |>
    save_kable(file.path(path, "family_sensitivity_ch.tex"))

  invisible(NULL)
}


# 5. Figures ####

#' Colours are attached to families (identical across figures); the family
#' used in the main analyses is drawn with a solid line, the others dashed.
sensitivity_colours_irn <- function() {
  c(
    "Scaled t (identity)" = "#4F94CD",
    "Gaussian (identity)" = "#E69F00",
    "Beta (logit)" = "#009E73",
    "Gamma (log)" = "#D55E00",
    "Gaussian (log)" = "#CC79A7",
    "Inverse Gaussian (log)" = "#56B4E9",
    "Log-normal" = "#882255"
  )
}

sensitivity_theme_irn <- function() {
  theme_classic() +
    theme(
      panel.grid.major = element_line(color = "gray95", linetype = 1),
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.6, "lines")
    )
}

#' One panel: data points and the curves fitted with several families
sensitivity_panel_irn <- function(pts,
                                  crv,
                                  x_lab,
                                  y_lab,
                                  linetypes,
                                  ribbons = TRUE,
                                  log_y = FALSE) {
  crv$curve <- factor(crv$family, levels = names(linetypes))
  colours <- sensitivity_colours_irn()[names(linetypes)]

  p <- ggplot2::ggplot() +
    geom_point(
      data = pts,
      aes(x = x, y = y),
      colour = "grey45",
      shape = 16,
      size = 0.7,
      alpha = 0.6
    )
  if (ribbons) {
    p <- p + geom_ribbon(
      data = crv,
      aes(x = x, ymin = lower, ymax = upper, fill = curve),
      alpha = 0.12
    ) +
      scale_fill_manual(values = colours, name = NULL, drop = FALSE)
  }
  p <- p +
    geom_line(data = crv,
              aes(x = x, y = fit, colour = curve, linetype = curve),
              linewidth = 0.7) +
    scale_colour_manual(values = colours, name = NULL, drop = FALSE) +
    scale_linetype_manual(values = linetypes, name = NULL, drop = FALSE) +
    labs(x = x_lab, y = y_lab) +
    sensitivity_theme_irn()
  if (log_y) {
    p <- p + scale_y_log10(
      breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500),
      labels = function(b) format(b, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
    )
  }
  p
}

#' Writes the family sensitivity figures in `path`:
#' - family_sensitivity_mass_balance.pdf (Fig. 1 b-d)
#' - family_sensitivity_assimilation_efficiency.pdf (element-wise AE, Fig. 3a)
#' - family_sensitivity_retention_time.pdf (element-wise RT, Fig. 3b)
plot_sensitivity_irn <- function(sensitivity, path) {
  specs <- sensitivity$specs
  curves <- sensitivity$curves
  points <- sensitivity$points

  dashes <- c("22", "42", "12", "4212")
  linetypes_for <- function(family_set) {
    fams <- vapply(sensitivity_families_irn(family_set), function(f) f$label, character(1))
    stats::setNames(c("solid", dashes)[seq_along(fams)], fams)
  }

  lab_msir <- "Intake rate <br> (<span style='font-size:8pt;'>g<sub>intake</sub> ⋅ g<sub>body</sub><sup>-1</sup> ⋅ d<sup>-1</sup></span>)"
  lab_gr <- "Growth rate <br> (<span style='font-size:8pt;'>g<sub>growth</sub> ⋅ g<sub>body</sub><sup>-1</sup> ⋅ d<sup>-1</sup></span>)"
  lab_ae <- "Assimilation efficiency <br> (<span style='font-size:8pt;'>g<sub>assim</sub> ⋅ g<sub>intake</sub><sup>-1</sup> </span>)"
  lab_ge <- "Growth efficiency <br> (<span style='font-size:8pt;'>g<sub>growth</sub> ⋅ g<sub>intake</sub><sup>-1</sup></span>)"

  save_plot <- function(plot, filename, width, height) {
    ggsave(
      filename = filename,
      plot = plot,
      device = pdf,
      path = path,
      scale = 1,
      width = width,
      height = height,
      units = "in"
    )
  }

  ## 1. Bulk mass models (Fig. 1 b-d) ####
  ids_tm <- specs$id[specs$block == "Mass balance" & specs$response_label != "GR"]
  x_labs_tm <- c(lab_msir, lab_msir, lab_gr)
  y_labs_tm <- c(lab_ae, lab_ge, lab_ge)
  lt <- linetypes_for("efficiency_individual")
  panels <- lapply(seq_along(ids_tm), function(k) {
    id <- ids_tm[k]
    sensitivity_panel_irn(
      points[points$id == id, ],
      curves[curves$id == id, ],
      x_labs_tm[k],
      y_labs_tm[k],
      linetypes = lt
    )
  })
  fig <- ggpubr::ggarrange(
    plotlist = panels,
    ncol = 3,
    nrow = 1,
    labels = c("(a)", "(b)", "(c)"),
    hjust = 0,
    vjust = 0,
    common.legend = TRUE,
    legend = "bottom"
  )
  fig <- ggpubr::annotate_figure(fig, top = "")
  save_plot(fig, "family_sensitivity_mass_balance.pdf", 7.5, 3.2)

  ## 2. Element-wise AE and RT, one panel per element ####
  elementwise_figure <- function(response, family_set, y_prefix, log_y, filename) {
    lt <- linetypes_for(family_set)
    sp <- specs[specs$block == "Elements" & specs$response == response, ]
    panels <- lapply(seq_len(nrow(sp)), function(k) {
      id <- sp$id[k]
      sensitivity_panel_irn(
        points[points$id == id, ],
        curves[curves$id == id, ],
        "",
        paste(y_prefix, sp$element[k]),
        linetypes = lt,
        ribbons = FALSE,
        log_y = log_y
      ) + ggpubr::rremove("xlab")
    })
    fig <- ggpubr::ggarrange(
      plotlist = panels,
      ncol = 4,
      nrow = 2,
      labels = paste0("(", letters[1:8], ")"),
      hjust = 0,
      vjust = 0,
      common.legend = TRUE,
      legend = "bottom"
    )
    fig <- ggpubr::annotate_figure(
      fig,
      bottom = gridtext::richtext_grob(text = lab_msir),
      top = ""
    )
    save_plot(fig, filename, 7.5, 4.8)
  }

  elementwise_figure(
    "assimilation_efficiency_dw",
    "efficiency_element",
    "AE of",
    FALSE,
    "family_sensitivity_assimilation_efficiency.pdf"
  )
  elementwise_figure(
    "retention_time",
    "retention_time",
    "RT of",
    TRUE,
    "family_sensitivity_retention_time.pdf"
  )

  invisible(NULL)
}
