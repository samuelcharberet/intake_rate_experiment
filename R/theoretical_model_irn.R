#' theoretical_model_irn
#'
#' @return
#' @export
#'
#' @examples
theoretical_model_irn <- function(data_i) {
  # Giving the variables friendly names to work with
  data_i$abs_eff <- data_i$assimilation_efficiency_dw
  data_i$msir <- data_i$mass_specific_ingestion_rate_dw
  data_i$g_eff <- data_i$growth_efficiency_dw
  data_i$g_r <- data_i$geometric_mean_growth_dw
  # Fixing gut length and gut section to real life values
  L_g <- 0.1
  S <- 0.002

  # Growth efficiency according to msir trying with free parameters and nlsLM


  growth_model_nls_fp <- minpack.lm::nlsLM(
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


  # Works well, trying with free parameters and onls


  growth_model_onls_fp <- onls::onls(
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
      paste("Mass-specific intake rate ", "(", g, "·", g^{
        -1
      }, "·", d^{
        -1
      }, ")")
    ),
    ylab = "",
    pch = 16,
    cex = 0.75
  )
  title(ylab = expression(paste("Growth efficiency", "(", g, "·", g^{
    -1
  }, ")")), line = 2)
  lines(data_i$msir[order(data_i$msir)],
    predict(growth_model_onls_fp)[order(data_i$msir)],
    col = 2,
    lwd = 5
  )
  dev.off()

  return(here::here(
    "4_outputs",
    "2_figures",
    "theoretical_model_fit_ge_msir.pdf"
  ))

  # Looking at the relationship between growth rate and growth efficiency:

  # Given constants
  p_0 <- 2.067e+04
  r_D <- 2.055e-01
  r_A <- 1.444e+01
  M <- 1.474e-01

  # Parametric equations
  E_G <- function(t) {
    (1 / (r_D - r_A)) * (r_A * (exp(-L_g * r_D / t) - 1) - r_D * (exp(-L_g * r_A / t) - 1)) - M / (S *
      t * p_0)
  }

  R_G <- function(t) {
    (S * t * p_0 / (r_D - r_A)) * (r_A * (exp(-L_g * r_D / t) - 1) - r_D * (exp(-L_g * r_A / t) - 1)) - M
  }

  # Generate sequence for t
  t_vals <- seq(0.005, 1, length.out = 1000) # Adjust the range and length as needed

  # Compute E_G and R_G for the sequence of t
  E_G_vals <- sapply(t_vals, E_G)
  R_G_vals <- sapply(t_vals, R_G)


  plot(
    R_G_vals,
    E_G_vals,
    type = "l",
    xlim = c(0, 0.6),
    ylim = c(0, 0.6)
  )
  # Plot the original data
  plot(
    data_i$g_r,
    data_i$g_eff,
    xlab = "Growth Rate rate",
    ylab = "Growth efficiency",
    main = "Overlay of Parametric Curve on Data",
    xlim = c(0, 0.6),
    ylim = c(0, 0.6)
  )

  # Overlay the parametric curve

  lines(E_G_vals, R_G_vals, col = "blue", lwd = 2)
}
