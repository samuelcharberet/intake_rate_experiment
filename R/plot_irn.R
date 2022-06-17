#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_ic, data_gc) {
  # Set global options for the ggplot2 plots
  theme_set(
    theme_classic() + theme(
      text = element_text(size = 14),
      legend.position = 'right',
      aspect.ratio = 0.618,
      panel.grid.major = element_line(
        color = "gray95",
        size = 0.5,
        linetype = 1
      )
    )
  )
  
  
  
  # Effect of week on bodymass at the start
  pdf(
    here::here("4_outputs", "bm_j0_fw_&_week.pdf"),
    width = 6,
    height = 4
  )
  boxplot(
    data = data_ic,
    bodymass_7th_instar_j0_fw ~ seventh_instar_date,
    xlab = "Date of the experiment",
    ylab = "Bodymass at the start of 7th instar (mg fw)"
  )
  dev.off()
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  pdf(
    here::here("4_outputs", "bm_j3_dw_&_food_consumed.pdf"),
    width = 6,
    height = 4
  )
  plot(
    data_ic$bodymass_7th_instar_j3_dw ~ data_ic$food_consumed_collection_days_dw,
    xlab = "Total amount of food consumed (mg dw)",
    ylab = "Bodymass at the end of the 7th instar (mg dw)"
  )
  dev.off()
  
  pdf(
    here::here("4_outputs", "bm_imago_dw_&_food_provided_fw.pdf"),
    width = 6,
    height = 4
  )
  plot(
    data_ic$bodymass_imago_dw ~ data_ic$food_consumed_collection_days_dw,
    xlab = "Total amount of food consumed (mg dw)",
    ylab = "Bodymass of the imago (mg dw)"
  )
  dev.off()
  
  ##########  3. Graphics and figures  ##########
  data_ic$food_provided_fw = as.factor(data_ic$food_provided_fw)
  
  ###### Proportional fluxes ######
  
  ### Absorption efficiency according to mass specific ingestion rate ###
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = absorption_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Absorption efficiency (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "aedw_&_msirfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Growth efficiency ######
  
  # Growth efficiency in fresh weight according to the mass specific ingestion rate in fresh weight
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Growth efficiency (mg fw body / mg fw food)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "gefw_&_msirfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Growth efficiency in dry weight according to the mass specific ingestion rate in dry weights
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg dw indiv)", y = "Growth efficiency (mg dw body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "gedw_&_msirdw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Growth rate in fresh weight according to the mass specific ingestion rate
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate_fw / ((bodymass_7th_instar_j3_fw + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_rate)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Growth rate (mg fw body / day") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "grfw_&_msirfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Growth efficiency in fresh weight according to growth rate
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = growth_rate, y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Growth rate (mg fw / day)", y = "Growth efficiency (mg fw body / mg fw food)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "gefw_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  ###### Growth investment according to mass specific amount absorbed  ######
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = absorbed_mass_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_investment_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific absorbed mass (mg dw/ mg dw)", y = " Growth investment (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "gidw_&_msamdw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  ###### Growth efficiency according to absorption efficiency  ######
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = absorption_efficiency_dw, y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Absoprtion efficiency (mg dw / mg dw)", y = "Growth efficiency (mg dw body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  
  ggsave(
    filename = "gedw_&_aedw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
}