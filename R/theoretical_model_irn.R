#'theoretical_model_irn
#'
#'@return
#'@export
#'
#'@examples

theoretical_model_irn <- function(data_i) {
  #Giving the variables friendly names to work with
  data_i$abs_eff = data_i$absorption_efficiency_dw / 100
  data_i$msir = data_i$mass_specific_ingestion_rate_fw
  data_i$g_eff = data_i$growth_efficiency_fw / 100
  
  # Fixing gut length and gut section to real life values
  L_g = 0.1
  S = 0.002
  
  # Fitting a non-linear model
  abs_model = minpack.lm::nlsLM(
    abs_eff ~ (p_0 / ((p_0 + p_u) * (r_D - r_A))) * (r_A * (exp(
      -(L_g * r_D * S * (p_0 + p_u)) / msir
    ) - 1) - r_D * (exp(
      -(L_g * r_A * S * (p_0 + p_u)) / msir
    ) - 1)),
    start = list(
      p_0 = 1500,
      p_u = 1000,
      r_D = 5,
      r_A = 6
    ),
    data = data_i
  )
  
  # Computing the variance explained
  sse = abs_model$m$deviance()
  null_model = lm(abs_eff ~ 1, data_i)
  sst = data.frame(summary.aov(null_model)[[1]])$Sum.Sq
  percent_variation_explained = 100 * (sst - sse)
  
  #Plotting the fit
  plot(data_i$msir, data_i$abs_eff)
  lines(data_i$msir[order(data_i$msir)],
        predict(abs_model)[order(data_i$msir)],
        col = 2,
        lwd = 5)
  
  # Fitting a non linear model that minimizes orthogonal distances rather than vertical ones
  
  abs_model_onls = onls::onls(
    abs_eff ~ (p_0 / ((p_0 + p_u) * (r_D - r_A))) * (r_A * (exp(
      -(L_g * r_D * S * (p_0 + p_u)) / msir
    ) - 1) - r_D * (exp(
      -(L_g * r_A * S * (p_0 + p_u)) / msir
    ) - 1)),
    start = list(
      p_0 = summary(abs_model)$p[1, 1],
      p_u = summary(abs_model)$p[2, 1],
      r_D = summary(abs_model)$p[3, 1],
      r_A = summary(abs_model)$p[4, 1]
    ),
    data = data_i
  )
  
  # Plotting the fit
  plot(data_i$msir, data_i$abs_eff)
  lines(data_i$msir[order(data_i$msir)],
        predict(abs_model_onls)[order(data_i$msir)],
        col = 2,
        lwd = 5)
  
  # Try to fit growth efficiency using parameters obtained from absorption efficiency
  p_0 = summary(abs_model_onls)$p[1, 1]
  p_u = summary(abs_model_onls)$p[2, 1]
  r_D = summary(abs_model_onls)$p[3, 1]
  r_A = summary(abs_model_onls)$p[4, 1]
  
  growth_model_onls = onls::onls(g_eff ~ (p_0 / ((p_0 + p_u) * (r_D - r_A))) * (r_A * (exp(
    -(L_g * r_D * S * (p_0 + p_u)) / msir
  ) - 1) - r_D * (exp(
    -(L_g * r_A * S * (p_0 + p_u)) / msir
  ) - 1)) - M / msir,
  start = list(M = 0.5),
  data = data_i)
  
  summary(growth_model_onls)
  
  # Plotting the fit
  plot(data_i$msir, data_i$g_eff)
  lines(data_i$msir[order(data_i$msir)],
        predict(growth_model_onls)[order(data_i$msir)],
        col = 2,
        lwd = 5)
  
  
  #Does not work well, trying with free parameters and nlsLM
  
  
  growth_model_nls_fp = minpack.lm::nlsLM(
    g_eff ~ (p_0 / ((p_0 + p_u) * (r_D - r_A))) * (r_A * (exp(
      -(L_g * r_D * S * (p_0 + p_u)) / msir
    ) - 1) - r_D * (exp(
      -(L_g * r_A * S * (p_0 + p_u)) / msir
    ) - 1)) - M / msir,
    start = list(
      p_0 = 1500,
      p_u = 1000,
      r_D = 5,
      r_A = 6,
      M = 0.02
    ),
    data = data_i,
    control = list(maxiter = 500)
  )
  
  summary(growth_model_nls_fp)
  # Plotting the fit
  plot(data_i$msir, data_i$g_eff)
  lines(data_i$msir[order(data_i$msir)],
        predict(growth_model_nls_fp)[order(data_i$msir)],
        col = 2,
        lwd = 5,
  )
  
  
  #Works well, trying with free parameters and onls
  
  
  growth_model_onls_fp = onls::onls(
    g_eff ~ (p_0 / ((p_0 + p_u) * (r_D - r_A))) * (r_A * (exp(
      -(L_g * r_D * S * (p_0 + p_u)) / msir
    ) - 1) - r_D * (exp(
      -(L_g * r_A * S * (p_0 + p_u)) / msir
    ) - 1)) - M / msir,
    start = list(
      p_0 = 1500,
      p_u = 1000,
      r_D = 5,
      r_A = 6,
      M = 0.02
    ),
    lower = c(0, 0, 0, 0, 0),
    extend = c(.3, .3),
    window = 100,
    data = data_i
  )
  
  plot(growth_model_onls_fp)
  
  growth_model_onls_fp
  # Works !
  onls::check_o(growth_model_onls_fp) # Orthogonality passes
  # Plotting the fit
  
  pdf(
    file = here::here(
      "4_outputs",
      "2_figures",
      "theoretical_model_fit_ge_msir.pdf"
    ),
    width = 5,
    height = 4
  )
  plot(
    data_i$msir,
    data_i$g_eff,
    col = rgb(
      red = 0,
      green = 0,
      blue = 0,
      alpha = 0.3
    ),
    xlab = expression(
      paste("Mass-specific intake rate ", "(", g, "·", g ^ {
        -1
      }, "·", d ^ {
        -1
      }, ")")
    ),
    ylab = "",
    pch = 16,
    cex = 0.75
  )
  title(ylab = expression(paste("Growth efficiency", "(", g , "·", g ^ {
    -1
  }, ")")), line = 2)
  lines(data_i$msir[order(data_i$msir)],
        predict(growth_model_onls_fp)[order(data_i$msir)],
        col = 2,
        lwd = 5)
  dev.off()
  
  return(here::here(
    "4_outputs",
    "2_figures",
    "theoretical_model_fit_ge_msir.pdf"
  ))
  
  
  
  
}