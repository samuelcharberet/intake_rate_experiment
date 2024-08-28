#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples
plot_irn <- function(data_i, data_g, data_model, data_ic) {
  # Set global options for the ggplot2 plots
  ggplot2::theme_set(
    theme_classic() + theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray95", linetype = 1)
    )
  )
  
  # The food provided daily is a factor rather than a numeric variable
  data_i$food_provided_fw <- as.factor(data_i$food_provided_fw)
  
  # 0. Preliminary figures ####
  
  # Reshape the data from wide to long format
  
  long_df <- data_i %>%
    select(food_provided_fw, starts_with("bodymass_7th_instar_j")) %>%
    gather(key = "time", value = "body_mass", -food_provided_fw) %>%
    mutate(time = gsub("bodymass_7th_instar_j|_fw", "", time),
           time = as.numeric(time))
  
  # Filter the data to include only the first three days
  long_df_filtered <- long_df %>%
    filter(time <= 3)
  
  # Compute the mean body mass for each treatment and time point
  mean_df <- long_df_filtered %>%
    group_by(food_provided_fw, time) %>%
    dplyr::summarize(mean_body_mass = mean(body_mass, na.rm = TRUE))
  
  # Create the plot
  growth_curve <- ggplot(mean_df, aes(
    x = time,
    y = mean_body_mass,
    color = as.factor(food_provided_fw)
  )) +
    geom_line() +
    geom_point() +
    labs(y = "Body mass (mg fw)", x = "Time (days)", color = "Food provided (mg)") +
    theme_minimal() +
    scale_color_viridis(discrete = TRUE, option = "C")
  
  ggsave(
    filename = "growth_curve.pdf",
    plot = growth_curve,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ## Effect of treatment on water content in the larvae at the start ######
  data_f <- filter(data_i, number_collection_days == 3)
  data_f$wc <- (data_f$bodymass_7th_instar_j3_fw - data_f$bodymass_7th_instar_j3_dw) /
    data_f$bodymass_7th_instar_j3_fw
  
  data_wc_t <- data_f[, c("wc", "food_provided_fw")]
  data_wc_0 <- data.frame(wc = data_ic$indiv_water_content,
                          food_provided_fw = "Initial")
  data_wc <- rbind(data_wc_t, data_wc_0)
  
  p <- ggplot2::ggplot(data_wc, aes(
    x = as.factor(food_provided_fw),
    y = wc * 100,
    group = as.factor(food_provided_fw)
  )) +
    geom_boxplot(fill = "steelblue3") +
    geom_jitter() +
    ylim(NA, 90) +
    labs(x = "Food distributed (mg)", y = "Water content (%)") +
    theme(legend.position = "none") +
    ggpubr::stat_compare_means(method = "anova")
  
  ggsave(
    filename = "wc_&_treatment.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ## Effect of week on bodymass at the start ######
  
  p <- ggplot2::ggplot(
    data_i,
    aes(x = seventh_instar_date, y = bodymass_7th_instar_j0_fw, group = seventh_instar_date)
  ) +
    geom_boxplot(fill = "steelblue3") +
    labs(x = "Date of the experiment", y = "Bodymass at the start of 7th instar (mg fw)") +
    theme(legend.position = "none")
  
  ggsave(
    filename = "bm_j0_fw_&_week.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ## Effect of treatment on bodymass at the start ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = food_provided_fw, y = bodymass_7th_instar_j0_fw)) +
    geom_boxplot(fill = "steelblue3") +
    labs(x = "Treatment (mg daily food)", y = "Bodymass at the start of 7th instar (mg fw)") +
    theme(legend.position = "none")
  
  ggsave(
    filename = "bm_j0_fw_&_treatment.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ## Dry weight bodymass at the end of the 7th instar according to food consumed  ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = food_consumed_collection_days_dw, y = bodymass_7th_instar_j3_dw)) +
    geom_point() +
    labs(x = "Total amount of food consumed (mg dw)", y = "Bodymass at the end of the 7th instar (mg dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    )
  
  ggsave(
    filename = "bm_j3_dw_&_food_consumed.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ## Dry weight bodymass of the imagos according to food consumed  ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = food_consumed_collection_days_dw, y = bodymass_imago_dw)) +
    geom_point() +
    labs(x = "Total amount of food consumed (mg dw)", y = "Bodymass of the imago (mg dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    )
  
  ggsave(
    filename = "bm_imago_dw_&_food_consumed_dw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ## Dry weight bodymass at the end of the 7th instar according to MSIR  ######
  
  p <- ggplot2::ggplot(
    data_i,
    aes(
      x = mass_specific_ingestion_rate_dw,
      y = bodymass_7th_instar_j3_fw * (1 - wc),
      colour = interaction(treatment_ID, seventh_instar_date)
    )
  ) +
    geom_point() +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Bodymass at the end of the 7th instar (mg dw)") +
    geom_smooth(color = "steelblue3",
                method = lm,
                formula = y ~ x) +
    theme(axis.title.x = element_markdown())
  
  
  
  
  
  # 1. Mass balance figures ##########
  
  ### Assimilation rate according to intake rate ######
  
  mod <- mgcv::gam(assimilation_rate_dw ~ s(ingestion_rate_dw, bs = "cs", k =
                                              3),
                   data = data_i)
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  ardw_irfw <- ggplot2::ggplot(data_i_f, aes(x = ingestion_rate_dw, y = assimilation_rate_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i$assimilation_rate_dw) + 0.1 * (
      max(data_i$assimilation_rate_dw) - min(data_i$assimilation_rate_dw)
    )) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> day<sup>-1</sup>)", y = "Assimilation rate <br> (mg<sub>(dw)</sub> day<sup>-1</sup>)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "black"
    )
  
  ggsave(
    filename = "ardw_&_irfw.pdf",
    plot = ardw_irfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Assimilation rate according to mass specific intake rate ######
  mod <- mgcv::gam(assimilation_rate_dw ~ s(
    mass_specific_ingestion_rate_fw,
    bs = "cs",
    k = 3
  ),
  data = data_i)
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  ardw_msirfw <- ggplot2::ggplot(data_i_f,
                                 aes(x = mass_specific_ingestion_rate_fw, y = assimilation_rate_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i$assimilation_rate_dw) + 0.1 * (
      max(data_i$assimilation_rate_dw) - min(data_i$assimilation_rate_dw)
    )) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Assimilation rate <br> (mg<sub>(dw)</sub> day<sup>-1</sup>)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "ardw_&_msirfw.pdf",
    plot = ardw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Mass specific Assimilation rate according to mass specific intake rate ######
  
  
  msardw_msirfw <- ggplot2::ggplot(
    data_i,
    aes(x = mass_specific_ingestion_rate_dw, y = mass_specific_assimilation_rate_dw)
  ) +
    geom_point() +
    xlim(0, NA) +
    ylim(0,
         max(data_i$mass_specific_assimilation_rate_dw) + 0.1 * (
           max(data_i$mass_specific_assimilation_rate_dw) - min(data_i$mass_specific_assimilation_rate_dw)
         )) +
    labs(x = "Intake rate <br> (mg<sub>food(dw)</sub> mg<sub>body(dw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Assimilation rate <br> (mg<sub>(dw)</sub> mg<sub>body(dw)</sub><sup>-1</sup> day<sup>-1</sup>)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x),
      method.args = list(method = "REML")
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "black"
    )
  
  ggsave(
    filename = "msardw_&_msirfw.pdf",
    plot = msardw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Assimilation efficiency in dw according to mass specific intake rate in fw ######
  
  mod <- mgcv::gam(
    assimilation_efficiency_dw ~ s(mass_specific_ingestion_rate_fw),
    family = scat(),
    method = "REML",
    data = data_i
  )
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  aedw_msirfw <- ggplot2::ggplot(data_i_f,
                                 aes(x = mass_specific_ingestion_rate_fw, y = assimilation_efficiency_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA,
         max(data_i$assimilation_efficiency_dw) + 0.1 * (
           max(data_i$assimilation_efficiency_dw) - min(data_i$assimilation_efficiency_dw)
         )) +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Assimilation efficiency") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x),
      method.args = list(method = "REML", family = scat())
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  
  ggsave(
    filename = "aedw_&_msirfw.pdf",
    plot = aedw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  
  
  ### Growth efficiency in fresh weight according to the mass specific intake rate in fresh weight  ######
  mod <- mgcv::gam(growth_efficiency_fw ~ s(
    mass_specific_ingestion_rate_fw,
    bs = "cs",
    k = 3
  ),
  data = data_i)
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  gefw_msirfw <- ggplot2::ggplot(data_i,
                                 aes(x = mass_specific_ingestion_rate_fw, y = growth_efficiency_fw)) +
    xlim(0, NA) +
    ylim(NA, max(data_i$growth_efficiency_fw) + 0.1 * (
      max(data_i$growth_efficiency_fw) - min(data_i$growth_efficiency_fw)
    )) +
    geom_vline(xintercept = 1,
               color = "black",
               linetype = "dashed") +
    geom_hline(yintercept = 0.5,
               color = "black",
               linetype = "dashed") +
    geom_point() +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 4)
    ) +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency") +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "gefw_&_msirfw.pdf",
    plot = gefw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth efficiency in fresh weight according to specific growth rate in fresh weight ######
  
  mod <- mgcv::gam(
    growth_efficiency_fw ~ s(geometric_mean_growth_fw, bs = "cs", k = 3) + s(ingestion_rate_fw, bs = "cs", k = 3),
    data = data_i
  )
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  gefw_msgrfw <- ggplot2::ggplot(data_i_f,
                                 aes(x = geometric_mean_growth_fw, y = growth_efficiency_fw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i_f$growth_efficiency_fw) + 0.1 * (
      max(data_i_f$growth_efficiency_fw) - min(data_i_f$growth_efficiency_fw)
    )) +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 4)
    ) +
    labs(x = "Growth rate", y = "Growth efficiency") +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "gefw_&_msgrfw.pdf",
    plot = gefw_msgrfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth efficiency according to assimilation efficiency  ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = assimilation_efficiency_dw, y = growth_efficiency_dw *
                             100)) +
    geom_point() +
    labs(x = "Assimilation efficiency (% dw)", y = "Growth efficiency (dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    )
  
  ggsave(
    filename = "gedw_&_aedw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth rate according to intake ######
  
  mod <- mgcv::gam(geometric_mean_growth_dw ~ s(ingestion_rate_fw, bs = "cs", k =
                                                  3),
                   data = data_i)
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  grfw_irfw <- ggplot2::ggplot(data_i_f,
                               aes(x = ingestion_rate_fw, y = geometric_mean_growth_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i$geometric_mean_growth_dw) + 0.1 * (
      max(data_i$geometric_mean_growth_dw) - min(data_i$geometric_mean_growth_dw)
    )) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> day<sup>-1</sup>)", y = "Growth rate") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown()) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "black"
    )
  
  ggsave(
    filename = "grfw_&_irfw.pdf",
    plot = grfw_irfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  ### Mass-specific growth rate in fresh weight according to the mass specific intake rate in fresh weight  ######
  mod <- mgcv::gam(
    geometric_mean_growth_fw ~ s(
      mass_specific_ingestion_rate_fw,
      bs = "cs",
      k = 3
    ),
    data = data_i
  )
  hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod) < hlt)
  
  msgrfw_msirfw <- ggplot2::ggplot(data_i_f,
                                   aes(x = mass_specific_ingestion_rate_fw, y = geometric_mean_growth_fw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i$geometric_mean_growth_fw) + 0.1 * (
      max(data_i$geometric_mean_growth_fw) - min(data_i$geometric_mean_growth_fw)
    )) +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth rate") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 4)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "msgrfw_&_msirfw.pdf",
    plot = msgrfw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  
  
  
  
  
  ### Growth investment according to mass specific amount assimilated  ######
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = assimilated_mass_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_investment_dw)) +
    geom_point() +
    labs(x = "Mass-specific assimilated mass (% dw)", y = " Growth investment (% dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 4)
    )
  
  ggsave(
    filename = "gidw_&_msamdw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth investment according to mass specific intake rate   ######
  
  gidw_msirfw <- ggplot2::ggplot(data_i,
                                 aes(x = mass_specific_ingestion_rate_fw, y = growth_investment_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(data_i$growth_investment_dw) + 0.2 * abs((
      max(data_i$growth_investment_dw) - min(data_i$growth_investment_dw)
    ))) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth investment (% dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "gidw_msirfw.pdf",
    plot = gidw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Maintenance investment according to mass specific intake rate   ######
  
  midw_msirfw <- ggplot2::ggplot(data_i,
                                 aes(x = mass_specific_ingestion_rate_fw, y = 1 -
                                       growth_investment_dw)) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA, max(1 - data_i$growth_investment_dw) + 0.2 * abs((
      max(1 - data_i$growth_investment_dw) - min(1 - data_i$growth_investment_dw)
    ))) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Maintenance investment (% dw)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "midw_msirfw.pdf",
    plot = midw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Mass-specific maintenance rate according to mass specific intake rate   ######
  
  msmrdw_msirdw <- ggplot2::ggplot(
    data_i,
    aes(x = mass_specific_ingestion_rate_dw, y = mass_specific_maintenance_rate_dw)
  ) +
    geom_point() +
    xlim(0, NA) +
    ylim(NA,
         max(data_i$mass_specific_maintenance_rate_dw) + 0.2 * abs((
           max(data_i$mass_specific_maintenance_rate_dw) - min(data_i$mass_specific_maintenance_rate_dw)
         ))) +
    labs(x = "Intake rate <br> (mg<sub>food(dw)</sub> mg<sub>body(dw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Mass-specific maintenance rate <br> (mg<sub>food(dw)</sub> mg<sub>body(dw)</sub><sup>-1</sup> day<sup>-1</sup>)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "msmrdw_msirdw.pdf",
    plot = msmrdw_msirdw,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  
  
  ### Mass-specific egestion rate according to mass-specific ingestion rate  ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = mass_specific_ingestion_rate_fw, y = egestion_rate_dw / ((bodymass_7th_instar_j3_fw + bodymass_7th_instar_j0_fw) / 2
                       ))) +
    geom_point() +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Egestion rate <br> (mg<sub>frass(dw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "cs", k = 3)
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "mser_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  ### Mass-specific growth rate in dry weight according to the mass specific intake rate in dry weight  ######
  mod_msgrdw_msirdw <- mgcv::gam(
    geometric_mean_growth_dw ~ s(mass_specific_ingestion_rate_fw),
    family = scat(),
    method = "REML",
    data = data_i
  )
  hlt <- 10 * sum(mgcv::influence.gam(mod_msgrdw_msirdw) / length(mgcv::influence.gam(mod_msgrdw_msirdw)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod_msgrdw_msirdw) < hlt)
  
  msgrdw_msirdw <- ggplot2::ggplot(data_i_f,
                                   aes(x = mass_specific_ingestion_rate_dw, y = geometric_mean_growth_dw)) +
    geom_point() +
    xlim(0, NA) +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth rate") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x),
      method.args = list(method = "REML", family = scat())
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "msgrdw_&_msirdw.pdf",
    plot = msgrdw_msirdw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ### Assimilation efficiency in dw according to mass specific intake rate in dw ######
  mod_aedw_msirdw <- mgcv::gam(
    assimilation_efficiency_dw ~ s(mass_specific_ingestion_rate_dw),
    family = scat(),
    method = "REML",
    data = data_i
  )
  hlt <- 10 * sum(mgcv::influence.gam(mod_aedw_msirdw) / length(mgcv::influence.gam(mod_aedw_msirdw)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod_aedw_msirdw) < hlt)
  
  aedw_msirdw <- ggplot2::ggplot(data_i_f,
                                 aes(x = mass_specific_ingestion_rate_dw, y = assimilation_efficiency_dw)) +
    geom_point() +
    xlim(0, NA) +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Assimilation efficiency") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x),
      method.args = list(family = scat(), method = "REML")
    ) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  
  ggsave(
    filename = "aedw_&_msirdw.pdf",
    plot = aedw_msirdw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth efficiency in dry weight according to the mass specific intake rate in dry weight  ######
  
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
  hlt <- 10 * sum(mgcv::influence.gam(mod_gedw_msirdw) / length(mgcv::influence.gam(mod_gedw_msirdw)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod_gedw_msirdw) < hlt)
  
  gedw_msirdw <- ggplot2::ggplot(data_i_f,
                                 aes(x = mass_specific_ingestion_rate_dw, y = growth_efficiency_dw)) +
    xlim(0, NA) +
    geom_point() +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency") +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "ad", k = 10),
      method.args = list(family = scat(), method = "REML")
    ) +
    theme(axis.title.x = element_markdown())
  
  ggsave(
    filename = "gedw_&_msirdw.pdf",
    plot = gedw_msirdw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ### Growth efficiency in dry weight according to specific growth rate in dry weight ######
  
  mod_gedw_msgrdw <- mgcv::gam(
    growth_efficiency_dw ~ s(geometric_mean_growth_dw, bs = "ad", k = 10),
    data = data_i,
    method = "REML",
    family = scat()
  )
  
  hlt <- 10 * sum(mgcv::influence.gam(mod_gedw_msgrdw) / length(mgcv::influence.gam(mod_gedw_msgrdw)))
  data_i_f <- filter(data_i, mgcv::influence.gam(mod_gedw_msgrdw) < hlt)
  gedw_msgrdw <- ggplot2::ggplot(data_i_f,
                                 aes(x = geometric_mean_growth_dw, y = growth_efficiency_dw)) +
    geom_point() +
    xlim(0, NA) +
    geom_smooth(
      color = "steelblue3",
      method = mgcv::gam,
      formula = y ~ s(x, bs = "ad", k = 10),
      method.args = list(family = scat(), method = "REML")
    ) +
    labs(x = "Growth rate", y = "Growth efficiency") +
    theme(axis.title.x = element_markdown(), axis.title.y = element_markdown())
  
  ggsave(
    filename = "gedw_&_msgrdw.pdf",
    plot = gedw_msgrdw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  # Mass balance complete plot ######
  
  complete_plot <- (msgrdw_msirdw | aedw_msirdw) /
    (gedw_msirdw | gedw_msgrdw) + plot_annotation(tag_levels = "a")
  
  
  ggsave(
    filename = paste("total_mass_balance.pdf", sep = ""),
    plot = complete_plot,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 6.5
  )
  
  
  
  # Mass balance derivatives #####
  
  
  # Derivative of mass specific growth rate dw according to mass specific intake rate dw
  deriv <- gratia::derivatives(mod_msgrdw_msirdw, interval = "simultaneous")
  deriv_msgrdw_msirdw <- draw(
    deriv,
    add_change = T,
    change_type = "sizer",
    decrease_col = "#F4DF4EFF",
    increase_col = "#00A4CCFF",
    alpha = 0.2,
    guides = "auto"
  ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = expression(paste(frac(
      italic(d), italic(dx)
    ), "  Growth rate"))) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_text()) +
    ggtitle("")
  
  # Derivative of assimilation efficiency dw according to mass specific intake rate dw
  
  deriv <- gratia::derivatives(mod_aedw_msirdw, interval = "simultaneous")
  deriv_aedw_msirdw <- draw(
    deriv,
    add_change = T,
    change_type = "sizer",
    decrease_col = "#F4DF4EFF",
    increase_col = "#00A4CCFF",
    alpha = 0.2,
    guides = "auto"
  ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = expression(paste(
      frac(italic(d), italic(dx)), "  Assimilation efficiency"
    ))) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_text()) +
    ggtitle("")
  
  # Derivative of growth efficiency dw according to mass specific intake rate dw
  
  deriv <- gratia::derivatives(mod_gedw_msirdw, interval = "simultaneous")
  deriv_gedw_msirdw <- draw(
    deriv,
    add_change = T,
    change_type = "sizer",
    decrease_col = "#F4DF4EFF",
    increase_col = "#00A4CCFF",
    alpha = 0.2,
    guides = "auto"
  ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = expression(paste(
      frac(italic(d), italic(dx)), "  Growth efficiency"
    ))) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_text()) +
    ggtitle("")
  
  # Derivative of growth efficiency dw according to mass specific growth rate dw
  broom::tidy(mod_gedw_msgrdw)
  
  deriv <- gratia::derivatives(mod_gedw_msgrdw, interval = "simultaneous")
  deriv_gedw_msgrdw <- draw(
    deriv,
    add_change = T,
    change_type = "sizer",
    decrease_col = "#F4DF4EFF",
    increase_col = "#00A4CCFF",
    alpha = 0.2,
    guides = "auto"
  ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(x = "Gowth rate", y = expression(paste(
      frac(italic(d), italic(dx)), "  Growth efficiency"
    ))) +
    theme(axis.title.x = element_markdown(), axis.title.y = element_text()) +
    ggtitle("")
  
  derivative_complete_plot <- (deriv_msgrdw_msirdw[[1]] |
                                 deriv_aedw_msirdw[[1]]) /
    (deriv_gedw_msirdw[[1]] | deriv_gedw_msgrdw[[1]]) +
    plot_annotation(tag_levels = "a")
  
  
  ggsave(
    filename = paste("derivative_complete_plot.pdf", sep = ""),
    plot = derivative_complete_plot,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 6.5
  )
  
  # 2. Chemical balance figures ##########
  
  
  # Options for the plots
  
  variables <- c("larvae",
                 "frass",
                 "assimilation_efficiency_dw",
                 "retention_time")
  nb_variables <- length(variables)
  elements_isotopes <- c("C", "N", "P", "Na", "Mg", "S", "K", "Ca", "15N", "13C")
  nb_elements_isotopes <- length(elements_isotopes)
  colours_elements_isotopes <- c(
    "C" = "#808080",
    "N" = "#5A5ACA",
    "P" = "#EC9200",
    "Na" = "#403EFF",
    "Mg" = "#5CC55C",
    "S" = "#D69F09",
    "K" = "#9B4BE1",
    "Ca" = "#DF4F4F",
    "15N" = "black",
    "13C" = "black"
  ) # The colors used for elements and isotopes, modified after Jmol
  
  elements <- elements_isotopes[1:8]
  nb_elements <- length(elements)
  colours_elements <- colours_elements_isotopes[1:8]
  units_content <- c("%", "%", "ppm", "ppm", "ppm", "ppm", "ppm", "ppm")
  
  ## Differences in elemental content between the variables: food, larva, frass ######
  
  plots_matrices <- vector("list", nb_elements)
  
  for (i in 1:nb_elements) {
    data_element <- as.data.frame(subset(data_g, data_g$element == elements[i]))
    food_col <- which(names(data_element) == paste("food_", elements[i], sep =
                                                     ""))
    
    # Food elemental content
    average_food <- mean(data_element[, food_col])
    sd_food <- sd(data_element[, food_col])
    
    # Larvae elemental content
    average_larvae <- mean(data_element[which(data_element$variable == "larvae"), ]$elemental_value, na.rm =
                             T)
    sd_larvae <- sd(data_element[which(data_element$variable == "larvae"), ]$elemental_value, na.rm =
                      T)
    
    # Frass elemental content
    average_frass <- mean(data_element[which(data_element$variable == "frass"), ]$elemental_value, na.rm =
                            T)
    sd_frass <- sd(data_element[which(data_element$variable == "frass"), ]$elemental_value, na.rm =
                     T)
    data <- data.frame(
      name = c("Food", "Larvae", "Frass"),
      value = c(average_food, average_larvae, average_frass),
      sd = c(sd_food, sd_larvae, sd_frass)
    )
    
    # Bar plot + error bar
    p <- ggplot(data) +
      geom_bar(
        aes(x = name, y = value),
        width = 0.5,
        stat = "identity",
        fill = "skyblue",
        alpha = 0.7
      ) +
      geom_errorbar(
        aes(
          x = name,
          ymin = value - sd,
          ymax = value + sd
        ),
        width = 0.2,
        colour = "orange",
        alpha = 0.9,
        size = 1
      ) +
      labs(x = "",
           y = paste(elements[i], " (", units_content[i], ")", sep = "")) +
      scale_x_discrete(limits = c("Food", "Larvae", "Frass")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plots_matrices[[i]] <- p
    
    # Save each plot
    ggsave(
      filename = paste("matrices_", elements[i], ".pdf", sep = ""),
      plot = p,
      device = pdf,
      path = here::here("4_outputs", "2_figures"),
      scale = 1,
      width = 3,
      height = 3,
      units = "in"
    )
  }
  
  # Create the complete matrices plots
  complete_matrices <- ggpubr::ggarrange(
    plots_matrices[[1]],
    plots_matrices[[2]],
    plots_matrices[[3]],
    plots_matrices[[4]],
    plots_matrices[[5]],
    plots_matrices[[6]],
    plots_matrices[[7]],
    plots_matrices[[8]],
    ncol = 4,
    nrow = 2,
    labels = c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h."),
    label.y = 1,
    label.x = 0,
    heights = c(0.5, 0.5),
    widths = c(1, 1, 1, 1)
  )
  
  # Save the complete plot
  ggsave(
    filename = paste("matrices_", "all_elements", ".pdf", sep = ""),
    plot = complete_matrices,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 4,
  )
  
  ## Growth rate hypothesis #####
  # We check whether the growth rate in positively related to P body content
  # according to the growth rate hypothesis of Elser
  data_larvae <- subset(data_g, data_g$variable == "larvae")
  data_larvae <- pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  
  larvaePdw_gr <- ggplot2::ggplot(data_larvae, aes(x = geometric_mean_growth_dw, y = P /
                                                     1000)) +
    ylim(NA, max(data_larvae$P /
                   1000, na.rm = T) + 0.15 * (
                     max(data_larvae$P /
                           1000, na.rm = T) - min(data_larvae$P /
                                                    1000, na.rm = T)
                   )) +
    geom_point(alpha = 0.2, shape = 16) +
    labs(x = "", y = "Larvae P (g/kg)") +
    geom_smooth(color = "steelblue3", method = lm) +
    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                     label.x.npc = 0.1,
                     label.y.npc = 0.85)
  
  ggsave(
    filename = "larvaePdw_&_gr.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # We check whether the growth rate in positively related to P/N body content
  # according to nobody
  data_larvae$PN <- data_larvae$P / (10000 * data_larvae$N)
  larvaePNdw_gr <- ggplot2::ggplot(data_larvae, aes(x = geometric_mean_growth_dw, y = PN)) +
    ylim(NA, max(data_larvae$PN, na.rm = T) + 0.1 * (max(data_larvae$PN, na.rm = T) - min(data_larvae$PN, na.rm = T))) +
    geom_point(alpha = 0.2, shape = 16) +
    labs(x = "", y = "Larvae P/N") +
    geom_smooth(color = "steelblue3", method = lm) +
    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                     label.x.npc = 0.1,
                     label.y.npc = 0.85)
  
  ggsave(
    filename = "larvaePNdw_&_gr.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # We check whether the growth rate in positively related to P/C body content
  # according to nobody
  data_larvae$PC <- data_larvae$P / (10000 * data_larvae$C)
  larvaePCdw_gr <- ggplot2::ggplot(data_larvae, aes(x = geometric_mean_growth_dw, y = PC)) +
    ylim(NA, max(data_larvae$PC, na.rm = T) + 0.1 * (max(data_larvae$PC, na.rm = T) - min(data_larvae$PC, na.rm = T))) +
    geom_point(alpha = 0.2, shape = 16) +
    labs(x = "", y = "Larvae P/C") +
    geom_smooth(color = "steelblue3", method = lm) +
    ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                     label.x.npc = 0.1,
                     label.y.npc = 0.85)
  
  
  ggsave(
    filename = "larvaePCdw_&_gr.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # GRH complete plot
  
  
  grh_complete_plot <-
    (larvaePdw_gr |
       larvaePCdw_gr |
       larvaePNdw_gr) + patchwork::plot_annotation(tag_levels = "a")
  
  
  
  grh_complete_plot <- wrap_elements(panel = grh_complete_plot) +
    labs(tag = "Growth rate") +
    theme(plot.tag = element_text(size = rel(1)),
          plot.tag.position = "bottom")
  
  # Saving the the complete plots
  
  ggsave(
    filename = "grh.pdf",
    plot = grh_complete_plot,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 8,
    height = 4,
    units = "in"
  )
  
  ## Sterner Elser body - waste relationship #####
  # We check whether the body N:P is negatively correlated with the waste N:P
  # according to the Sterner & Elser model of 2002
  
  data_larvae_frass <- subset(data_g,
                              data_g$variable == "larvae" |
                                data_g$variable == "frass")
  data_larvae_frass <- pivot_wider(data_larvae_frass,
                                   names_from = element,
                                   values_from = elemental_value)
  data_larvae_frass$N_P <- data_larvae_frass$N / (data_larvae_frass$P / (10 ^
                                                                           4))
  data_lf_np <- select(
    data_larvae_frass,
    "N_P",
    "group_ID",
    "variable",
    "mean_mass_specific_intake_rate_fw"
  )
  data_lf_np <- pivot_wider(data_lf_np, names_from = variable, values_from = N_P)
  
  p <- ggplot2::ggplot(data_lf_np,
                       aes(x = larvae, y = frass, color = mean_mass_specific_intake_rate_fw)) +
    geom_point() +
    labs(x = "Larvae N:P", y = "Frass N:P") +
    geom_smooth(color = "black", method = lm) +
    scale_color_gradient(low = "blue",
                         high = "red",
                         name = "Intake level") +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "larvae_frass_NP.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ## Elements assimilation efficiency, larval content, frass content, retention time according to total mass-specific intake rate  ######
  
  # Set a new theme to produce the complete figures
  
  ggplot2::theme_set(
    theme_classic() + theme(
      panel.grid.major = element_line(color = "gray95", linetype = 1),
      legend.position = "none",
      axis.title.x = element_blank()
    )
  )
  
  # Create the legend plot
  
  legend <- ggplot(
    data_g,
    aes(
      x = mean_mass_specific_intake_rate_fw,
      y = elemental_value,
      color = element,
      fill = element
    )
  ) +
    geom_point() +
    lims(x = c(0, 0), y = c(0, 0)) +
    theme_void() +
    theme(legend.position.inside = c(0.5, 0.5)) +
    scale_color_manual(values = colours_elements, aesthetics = c("colour", "fill")) +
    guides(colour = guide_legend(override.aes = list(size = 8), ncol = 2))
  
  
  models <- list(
    list(family = gaussian(), method = "REML"),
    list(family = gaussian(), method = "REML"),
    list(family = betar(), method = "REML"),
    list(family = Gamma(link = log), method = "REML")
  )
  
  
  plots <- vector("list", nb_variables + 2)
  names(plots) <- c(variables,
                    paste("ddx_", variables[3], sep = ""),
                    paste("ddx_", variables[4], sep = ""))
  
  y_axes <- c("Larvae", "Frass", "AE of", "RT of")
  units_loop <- cbind(units_content, units_content, "", "days")
  
  y_plot_names <- c(
    "larvae_content",
    "frass_content",
    "assimilation_efficiency_dw",
    "retention_time",
    "ddx_assimilation_efficiency_dw",
    "ddx_retention_time"
  )
  
  # ylim min and ymax tables
  
  ylim_mins <- matrix(
    c(rep(NA, nb_elements * 2), rep(0, nb_elements * 2)),
    ncol = length(y_axes),
    nrow = nb_elements,
    byrow = F
  )
  ylim_maxs <- matrix(
    c(
      rep(NA, nb_elements * 2),
      rep(1.1, nb_elements),
      rep(NA, nb_elements)
    ),
    ncol = length(y_axes),
    nrow = nb_elements,
    byrow = F
  )
  
  
  # Creating the list of plots for the other elements
  
  for (i in 1:(nb_variables + 2)) {
    plots[[i]] <- vector("list", nb_elements)
    names(plots[[i]]) <- elements
  }
  
  
  # A for loop to create the plots of assimilation efficiency, larvae content, frass content and retention time
  # according to mass-specific intake rate for all elements
  
  
  
  for (j in 1:nb_variables) {
    data_matrix <- subset(data_g, data_g$variable == variables[j])
    ylim_min <- ylim_mins[i, j]
    
    for (i in 1:nb_elements) {
      data_matrix_element <- filter(
        data_matrix,
        data_matrix$element == elements[i],
        !is.na(data_matrix$elemental_value)
      )
      
      mod <- mgcv::gam(
        elemental_value ~ s(mean_mass_specific_intake_rate_fw),
        data = data_matrix_element,
        method = "REML",
        family = models[[j]]$family
      )
      
      hlt <- 10 * sum(mgcv::influence.gam(mod) / length(mgcv::influence.gam(mod)))
      data_matrix_element_f <- filter(data_matrix_element, mgcv::influence.gam(mod) < hlt)
      
      mod <- mgcv::gam(
        elemental_value ~ s(mean_mass_specific_intake_rate_fw),
        data = data_matrix_element_f,
        method = "REML",
        family = models[[j]]$family
      )
      
      if (is.na(ylim_maxs[i, j])) {
        ylim_max <- max(data_matrix_element_f$elemental_value, na.rm = T) + 0.2 * (
          max(data_matrix_element_f$elemental_value, na.rm = T) - min(data_matrix_element_f$elemental_value, na.rm = T)
        )
      } else {
        ylim_max <- ylim_maxs[i, j]
      }
      
      plots[[j]][[i]] <- ggplot2::ggplot(
        data_matrix_element_f,
        aes(x = mean_mass_specific_intake_rate_fw, y = elemental_value)
      ) +
        geom_point() +
        ylim(ylim_min, ylim_max) +
        geom_smooth(
          formula = y ~ s(x),
          method = gam,
          color = colours_elements[i],
          method.args = models[[j]]
        ) +
        labs(x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)", y = paste(y_axes[j], elements[i], paste("(", units_loop[i, j], ")", sep =
                                                                                                                                                   ""))) +
        theme(axis.title.x = element_markdown())
      
      
      if (j == 4) {
        plots[[j]][[i]] <- plots[[j]][[i]] +
          coord_trans(y = "log10") +
          scale_y_continuous(
            breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
            transform = "identity"
          )
      }
      
      
      # Save each plot
      ggsave(
        filename = paste(y_plot_names[j], elements[i], "dw_&_msirfw.pdf", sep = ""),
        plot = plots[[j]][[i]],
        device = pdf,
        path = here::here("4_outputs", "2_figures"),
        scale = 1,
        width = 7,
        height = 4,
        units = "in"
      )
      
      # Derivatives
      
      if (j == 3 | j == 4) {
        deriv <- gratia::derivatives(mod, interval = "simultaneous", type = "central")
        p <- draw(
          deriv,
          add_change = T,
          change_type = "sizer",
          decrease_col = colours_elements[i],
          increase_col = "#00A4CCFF",
          alpha = 0.2
        )
        
        p <- p[[1]] +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(
            x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)",
            y = paste(
              "<span style='font-size: 10pt;'><i><sup>d</sup>&frasl;<sub>dx</sub></i></span> ",
              y_axes[j],
              elements[i]
            )
          ) +
          theme(
            axis.title.x = element_markdown(),
            axis.title.y = element_markdown(),
            plot.margin = margin(0, 0, 0, 0)
          ) +
          ggtitle("")
        
        
        
        if (j == 3) {
          plots[[j + 2]][[i]] <- p + coord_cartesian(ylim = c(-6, 4))
        } else {
          plots[[j + 2]][[i]] <- p + coord_cartesian(ylim = c(-10, 5))
        }
        ggsave(
          filename = paste(y_plot_names[j + 2], elements[i], "dw_&_msirfw.pdf", sep = ""),
          plot = plots[[j + 2]][[i]],
          device = pdf,
          path = here::here("4_outputs", "2_figures"),
          scale = 1,
          width = 7,
          height = 4,
          units = "in"
        )
      }
    }
  }
  
  
  ######  The complete plot  ######
  
  complete_plots <- vector("list", nb_variables + 2)
  complete_plots_widths <- c(7, 7, 7, 7, 9, 9)
  complete_plots_heights <- c(4, 4, 4, 4, 4, 4)
  
  # Removing the x axis title
  for (i in 1:(nb_variables + 2)) {
    for (j in 1:nb_elements) {
      plots[[i]][[j]] <- plots[[i]][[j]] + ggpubr::rremove("xlab")
    }
  }
  for (i in 1:(nb_variables + 2)) {
    complete_plot <-
      (plots[[i]][[1]] |
         plots[[i]][[2]] |
         plots[[i]][[3]] |
         plots[[i]][[4]]) /
      (plots[[i]][[5]] |
         plots[[i]][[6]] |
         plots[[i]][[7]] |
         plots[[i]][[8]]) + patchwork::plot_annotation(tag_levels = "a")
    
    
    
    complete_plots[[i]] <- wrap_elements(panel = complete_plot) +
      labs(tag = expression(paste(
        "Intake rate", " (", mg[food], " ", mg[body(fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",
      ))) +
      theme(plot.tag = element_text(size = rel(1)),
            plot.tag.position = "bottom")
    
    # Saving the the complete plots
    
    ggsave(
      filename = paste(y_plot_names[i], "alldw_&_msirfw.pdf", sep = ""),
      plot = complete_plots[[i]],
      device = pdf,
      path = here::here("4_outputs", "3_figures_paper"),
      scale = 1,
      width = complete_plots_widths[i],
      height = complete_plots_heights[i],
      units = "in"
    )
  }
  
  
  ## CNP stoichiometry ######
  
  # Making C:N, N:P, and C:P plots for larvae and frass as a function of
  # the mass-specific intake rate
  
  # Set a theme for these stoichiometry figures
  
  ggplot2::theme_set(
    theme_classic() + theme(
      panel.grid.major = element_line(color = "gray95", linetype = 1),
      legend.position = "none",
      axis.title.x = element_blank()
    )
  )
  
  # For the larvae
  
  data_larvae <- subset(data_g, data_g$variable == "larvae")
  data_larvae <- pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  data_larvae$C_N <- data_larvae$C / data_larvae$N
  data_larvae$N_P <- data_larvae$N / (data_larvae$P / (10 ^ 4))
  data_larvae$C_P <- data_larvae$C / (data_larvae$P / (10 ^ 4))
  
  # Larvae CN
  cn_larvae <- ggplot2::ggplot(data_larvae,
                               aes(x = mean_mass_specific_intake_rate_dw, y = C_N)) +
    geom_point() +
    ylim(NA, max(data_larvae$C_N, na.rm = T) + 0.1 * (
      max(data_larvae$C_N, na.rm = T) - min(data_larvae$C_N, na.rm = T)
    )) +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body(fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Larvae C/N") +
    geom_smooth(color = "#6D6DA5", method = lm) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "cnlarvae_&_gmsir.pdf",
    plot = cn_larvae,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Larvae NP
  
  np_larvae <- ggplot2::ggplot(data_larvae,
                               aes(x = mean_mass_specific_intake_rate_dw, y = N_P)) +
    geom_point() +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Larvae N/P") +
    geom_smooth(color = "#A37665", method = lm) +
    scale_y_continuous(limits = c(NA, max(data_larvae$N_P, na.rm = T) + 0.1 * (
      max(data_larvae$N_P, na.rm = T) - min(data_larvae$N_P, na.rm = T)
    ))) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "nplarvae_&_gmsir.pdf",
    plot = np_larvae,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Larvae CP
  
  
  cp_larvae <- ggplot2::ggplot(data_larvae,
                               aes(x = mean_mass_specific_intake_rate_dw, y = C_P)) +
    geom_point() +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Larvae C/P") +
    geom_smooth(color = "#B68940", method = lm) +
    scale_y_continuous(limits = c(NA, max(data_larvae$C_P, na.rm = T) + 0.1 * (
      max(data_larvae$C_P, na.rm = T) - min(data_larvae$C_P, na.rm = T)
    ))) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "cplarvae_&_gmsir.pdf",
    plot = cp_larvae,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # For frass
  
  data_frass <- subset(data_g, data_g$variable == "frass")
  data_frass <- pivot_wider(data_frass, names_from = element, values_from = elemental_value)
  data_frass$C_N <- data_frass$C / data_frass$N
  data_frass$N_P <- data_frass$N / (data_frass$P / (10 ^ 4))
  data_frass$C_P <- data_frass$C / (data_frass$P / (10 ^ 4))
  
  
  # CN_frass = f(msir)
  
  cn_frass <- ggplot2::ggplot(data_frass,
                              aes(x = mean_mass_specific_intake_rate_dw, y = C_N)) +
    geom_point() +
    ylim(NA, max(data_frass$C_N, na.rm = T) + 0.1 * (max(data_frass$C_N, na.rm = T) - min(data_frass$C_N, na.rm = T))) +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Frass C/N") +
    geom_smooth(color = "#6D6DA5", method = lm) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "cnfrass_&_msir.pdf",
    plot = cn_frass,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # NP_frass = f(msir)
  
  np_frass <- ggplot2::ggplot(data_frass,
                              aes(x = mean_mass_specific_intake_rate_dw, y = N_P)) +
    geom_point() +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Frass N/P") +
    geom_smooth(color = "#A37665", method = lm) +
    scale_y_continuous(limits = c(NA, max(data_frass$N_P, na.rm = T) + 0.1 * (
      max(data_frass$N_P, na.rm = T) - min(data_frass$N_P, na.rm = T)
    ))) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "npfrass_&_msir.pdf",
    plot = np_frass,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # CP_frass = f(msir)
  
  cp_frass <- ggplot2::ggplot(data_frass,
                              aes(x = mean_mass_specific_intake_rate_dw, y = C_P)) +
    geom_point() +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food], " ", mg[body] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "Frass C/P") +
    geom_smooth(color = "#B68940", method = lm) +
    scale_y_continuous(limits = c(NA, max(data_frass$C_P, na.rm = T) + 0.1 * (
      max(data_frass$C_P, na.rm = T) - min(data_frass$C_P, na.rm = T)
    ))) +
    ggpubr::stat_cor(
      method = "pearson",
      cor.coef.name = c("rho"),
      label.x.npc = 0,
      label.y.npc = 1
    )
  
  ggsave(
    filename = "cpfrass_&_msir.pdf",
    plot = cp_frass,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ##### The complete plot #####
  
  # Set global options for the ggplot2 plots
  
  complete_stoichiometry <- ggpubr::ggarrange(
    cn_larvae,
    cp_larvae,
    np_larvae,
    cn_frass,
    cp_frass,
    np_frass,
    ncol = 3,
    nrow = 2,
    labels = c("a.", "b.", "c.", "d.", "e.", "f."),
    label.y = 1,
    label.x = 0,
    heights = c(1, 1, 1),
    widths = c(1, 1)
  )
  
  complete_stoichiometry <- ggpubr::annotate_figure(complete_stoichiometry, bottom = ggpubr::text_grob(expression(
    paste("Intake rate", " (", mg[food], " ", mg[body] ^ {
      -1
    }, " ", day ^ {
      -1
    }, ")")
  )))
  
  ggsave(
    filename = paste("frass_larvae_", "stoichiometry", ".pdf", sep = ""),
    plot = complete_stoichiometry,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 5,
    units = "in"
  )
  
  
  ##  Assimilation efficiencies on a single plot  #####
  ggplot2::theme_set(
    theme_classic() + theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray95", linetype = 1)
    )
  )
  
  data_abs <- subset(data_g, data_g$variable == "assimilation_efficiency_dw")
  # Removing 15N and 13C
  data_abs <- data_abs[!(data_abs$element %in% c("15N", "14N", "13C", "12C")), ]
  
  order_elements_legend <- c("K", "Mg", "C", "N", "P", "S", "Ca", "Na")
  ael_dw_msirfw <- ggplot2::ggplot(
    data_abs,
    aes(
      x = mean_mass_specific_intake_rate_dw,
      y = elemental_value,
      colour = element,
      group = element,
      fill = element
    )
  ) +
    geom_point(alpha = 0.1, shape = 16) +
    geom_smooth(
      method = "gam",
      formula = y ~ s(x),
      se = FALSE,
      method.args = list(method = "REML")
    ) +
    scale_color_manual(
      values = colours_elements,
      aesthetics = c("colour", "fill"),
      labels = order_elements_legend,
      limits = c("K", "Mg", "C", "N", "P", "S", "Ca", "Na")
    ) +
    labs(
      x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)",
      y = "Assimilation efficiency",
      fill = "Element",
      color = "Element"
    ) +
    theme(legend.position = "right", axis.title.x = element_markdown())
  
  # Save the plot
  ggsave(
    filename = paste(
      "assimilation_efficiencies",
      "layered",
      "dw_&_msirfw.pdf",
      sep = ""
    ),
    plot = ael_dw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  ##  Retention times on a single plot #####
  
  ggplot2::theme_set(
    theme_classic() + theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray95", linetype = 1)
    )
  )
  
  # Removing 15N and 13C
  data_rt <- subset(data_g, data_g$variable == "retention_time")
  
  data_rt <- data_rt[!(data_rt$element %in% c("15N", "14N", "13C", "12C")), ]
  
  rtl_dw_msirfw <- ggplot2::ggplot(
    data_rt,
    aes(
      x = mean_mass_specific_intake_rate_dw,
      y = elemental_value,
      colour = element,
      group = element,
      fill = element
    )
  ) +
    geom_point(alpha = 0.1) +
    geom_smooth(
      method = mgcv::gam,
      formula = y ~ s(x),
      se = F,
      method.args = list(method = "REML", family = Gamma(link = log))
    ) +
    scale_color_manual(values = colours_elements, aesthetics = c("colour", "fill")) +
    labs(
      x = "Intake rate <br> (mg<sub>food</sub> mg<sub>body</sub><sup>-1</sup> day<sup>-1</sup>)",
      y = "Retention time (days)",
      fill = "Element",
      color = "Element"
    ) +
    theme(legend.position = "right", axis.title.x = element_markdown()) +
    coord_trans(y = "log10") +
    scale_y_continuous(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
                       transform = "identity")
  # Save the plot
  ggsave(
    filename = paste("retention_times", "layered", "dw_&_msirfw.pdf", sep = ""),
    plot = rtl_dw_msirfw,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 3,
    units = "in"
  )
  
  ##  A figure with both Assimilation efficiency and retention time #####
  
  complete_plot <- ggpubr::ggarrange(
    ael_dw_msirfw + ggpubr::rremove("xlab"),
    rtl_dw_msirfw + ggpubr::rremove("xlab"),
    ncol = 2,
    nrow = 1,
    labels = c("a.", "b."),
    label.y = 1.1,
    label.x = 0,
    heights = c(1),
    widths = c(1, 1),
    common.legend = T,
    legend = "right"
  )
  
  complete_plot <- ggpubr::annotate_figure(complete_plot,
                                           bottom = ggpubr::text_grob(expression(
                                             paste("Intake rate", " (", mg[food], " ", mg[body] ^ {
                                               -1
                                             }, " ", day ^ {
                                               -1
                                             }, ")", )
                                           )),
                                           top = "")
  
  # Save the plot
  ggsave(
    filename = paste(
      "retention_times",
      "efficiencies",
      "layered",
      "dw_&_msirfw.pdf",
      sep = ""
    ),
    plot = complete_plot,
    device = pdf,
    path = here::here("4_outputs", "3_figures_paper"),
    scale = 1,
    width = 7,
    height = 3,
    units = "in"
  )
  
  ##  Nutrient co variations in larvae and frass #####
  data_larvae <- subset(data_g, data_g$variable == "larvae")
  # Removing 15N and 13C
  data_larvae <- data_larvae[!(data_larvae$element %in% c("d15N", "d13C")), ]
  test <- pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  pdf(here::here(
    "4_outputs",
    "2_figures",
    "larvae_nutrient_covariations.pdf"
  ))
  plot(test[, 55:62])
  dev.off()
  
  data_larvae <- subset(data_g, data_g$variable == "frass")
  # Removing 15N and 13C
  data_larvae <- data_larvae[!(data_larvae$element %in% c("d15N", "d13C")), ]
  test <- pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  pdf(here::here(
    "4_outputs",
    "2_figures",
    "frass_nutrient_covariations.pdf"
  ))
  p <- plot(test[, 55:62])
  dev.off()
  
  
  # 3. Isotopy figures ##########
  
  ##  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of MSIR ######
  
  ggplot2::theme_set(
    theme_classic() + theme(
      legend.position = "right",
      panel.grid.major = element_line(color = "gray95", linetype = 1)
    )
  )
  
  data_tf <- subset(data_g, data_g$variable == "tf")
  data_tf <- pivot_wider(data_tf, names_from = element, values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_tf, aes(x = mean_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13ctf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf, aes(x = mean_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15ntf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of growth rate ######
  
  
  ctf_gr <- ggplot2::ggplot(data_tf, aes(x = geometric_mean_growth_dw, y = `13C`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = expression(paste("Growth rate")), y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3", method = "lm") +
    ggpubr::stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      method = "pearson",
      # cor.coef.name = c("rho"),
      label.x.npc = 0.2,
      label.y.npc = 1
    ) +
    ylim(NA, max(data_tf$`13C`, na.rm = T) + 0.2 * (abs(
      max(data_tf$`13C`, na.rm = T) - min(data_tf$`13C`, na.rm = T)
    )))
  
  
  ggsave(
    filename = "13ctf_&_gr.pdf",
    plot = ctf_gr,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ntf_gr <- ggplot2::ggplot(data_tf, aes(x = geometric_mean_growth_dw, y = `15N`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = expression(paste("Growth rate")), y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3", method = "lm") +
    ggpubr::stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      method = "pearson",
      # cor.coef.name = c("rho"),
      label.x.npc = 0.2,
      label.y.npc = 1
    ) +
    ylim(NA, max(data_tf$`15N`, na.rm = T) + 0.2 * (abs(
      max(data_tf$`15N`, na.rm = T) - min(data_tf$`15N`, na.rm = T)
    )))
  
  ggsave(
    filename = "15ntf_&_gr.pdf",
    plot = ntf_gr,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of mass-specific growth rate ######
  
  
  p <- ggplot2::ggplot(data_tf, aes(x = geometric_mean_growth_dw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Mass-specific growth rate (mg fw/ day)", y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13ctf_&_msgr.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf, aes(x = geometric_mean_growth_dw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Mass-specific growth rate (mg fw/ day)", y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15ntf_&_msgr.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of growth efficiency ######
  
  
  p <- ggplot2::ggplot(data_tf, aes(x = growth_efficiency_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Growth efficiency", y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13ctf_&_gefw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf, aes(x = growth_efficiency_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Growth efficiency", y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15ntf_&_gefw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic discrimination factor between frass and food (FFDF) according to MSIR ######
  
  data_ffdf <- subset(data_g, data_g$variable == "ffdf")
  data_ffdf <- pivot_wider(data_ffdf, names_from = element, values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = mean_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(paste(
        "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",
      )),
      y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{food}$)')
    ) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13cffdf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = mean_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(paste(
        "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",
      )),
      y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{food}$)')
    ) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15nffdf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic discrimination factor between frass and food (FFDF) according to assimilation efficiency ######
  
  data_ffdf <- subset(data_g, data_g$variable == "ffdf")
  data_ffdf <- pivot_wider(data_ffdf, names_from = element, values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_ffdf, aes(x = assimilation_efficiency_dw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Assimilation efficiency (%)",
         y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{food}$)')) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13cffdf_&_aedw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_ffdf, aes(x = assimilation_efficiency_dw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Assimilation efficiency (%)",
         y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{food}$)')) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15nffdf_&_aedw.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic discrimination factor between frass and larvae (FLDF) according to MSIR ######
  
  data_fldf <- subset(data_g, data_g$variable == "fldf")
  data_fldf <- pivot_wider(data_fldf, names_from = element, values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_fldf,
                       aes(x = mean_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(paste(
        "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",
      )),
      y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{larvae}$)')
    ) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "13cfldf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_fldf,
                       aes(x = mean_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(paste(
        "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",
      )),
      y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{larvae}$)')
    ) +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "15nfldf_&_msir.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ##  Isotopic assimilation efficiency ratio (IAER) according to MSIR ######
  
  data_aer <- subset(data_g, data_g$variable == "iaer")
  data_aer <- pivot_wider(data_aer, names_from = element, values_from = elemental_value)
  
  
  ciaer_msir <- ggplot2::ggplot(data_aer, aes(x = mean_mass_specific_intake_rate_fw, y = `C`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "C IAER") +
    geom_smooth(color = "steelblue3",
                method = "lm",
                span = 0.75) +
    theme(axis.title.x = element_markdown()) +
    ggpubr::stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      method = "pearson",
      # cor.coef.name = c("rho"),
      label.x.npc = 0.2,
      label.y.npc = 1
    ) +
    ylim(NA, max(data_aer$`C`, na.rm = T) + 0.2 * abs(max(data_aer$`C`, na.rm = T) - min(data_aer$`C`, na.rm = T)))
  
  ggsave(
    filename = "ciaer_&_msir.pdf",
    plot = ciaer_msir,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  niaer_msir <- ggplot2::ggplot(data_aer, aes(x = mean_mass_specific_intake_rate_fw, y = `N`)) +
    geom_point(size = 1.5) +
    labs(x = expression(paste(
      "Intake rate", " (", mg[food(fw)], " ", mg[body(fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",
    )), y = "N IAER") +
    geom_smooth(color = "steelblue3", method = "lm") +
    ggpubr::stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      method = "pearson",
      # cor.coef.name = c("rho"),
      label.x.npc = 0.2,
      label.y.npc = 1
    ) +
    ylim(NA, max(data_aer$`N`, na.rm = T) + 0.2 * abs(max(data_aer$`N`, na.rm = T) - min(data_aer$`N`, na.rm = T)))
  
  
  ggsave(
    filename = "niaer_&_msir.pdf",
    plot = niaer_msir,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  
  ##  Isotopic assimilation efficiency ratio (IAER) according to AE ######
  
  
  p <- ggplot2::ggplot(data_aer, aes(x = assimilation_efficiency_dw, y = `C`)) +
    geom_point(size = 1.5) +
    labs(x = "Assimilation efficiency (%)", y = "C IAER") +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "ciaer_&_ae.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_aer, aes(x = assimilation_efficiency_dw, y = `N`)) +
    geom_point(size = 1.5) +
    labs(x = "Assimilation efficiency (%)", y = "N IAER") +
    geom_smooth(color = "steelblue3", method = "lm")
  
  ggsave(
    filename = "niaer_&_ae.pdf",
    plot = p,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  #  Complete figure for isotopy  #####
  # A theme specific to this figure
  ggplot2::theme_set(
    theme_classic() + theme(
      text = element_text(size = 12),
      panel.grid.major = element_line(color = "gray95", linetype = 1),
      legend.position = "none"
    )
  )
  
  plots <- list(msgrfw_msirfw, ciaer_msir, ctf_gr, ntf_gr)
  
  top_part <- ggpubr::annotate_figure(
    ggpubr::ggarrange(
      plots[[1]],
      plots[[2]],
      ncol = 2,
      nrow = 1,
      labels = c("a.", "b."),
      label.y = 1.1,
      label.x = 0,
      heights = 1,
      widths = c(1, 1)
    ),
    top = "",
  )
  
  ggplot2::theme_set(
    theme_classic() + theme(
      text = element_text(size = 12),
      panel.grid.major = element_line(color = "gray95", linetype = 1),
      legend.position = "none",
      axis.title.x = element_blank()
    )
  )
  
  bottom_part <- ggpubr::annotate_figure(
    ggpubr::ggarrange(
      plots[[3]],
      plots[[4]],
      ncol = 2,
      nrow = 1,
      labels = c("c.", "d."),
      label.y = 1.1,
      label.x = 0,
      heights = 1,
      widths = c(1, 1)
    ),
    top = "",
    bottom = ggpubr::text_grob(expression(paste("Growth rate")), size = 12)
  )
  
  
  
  # Creating the complete isotopy figure
  
  
  complete_plot <- ggpubr::ggarrange(top_part, bottom_part, ncol = 1, nrow = 2)
  
  # Saving the the complete plots
  
  ggsave(
    filename = "isotopy_figure.pdf",
    plot = complete_plot,
    device = pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 8,
    height = 6,
    units = "in"
  )
}
