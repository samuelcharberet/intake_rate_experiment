#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_i, data_g) {
  # Set global options for the ggplot2 plots
  ggplot2::theme_set(
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
  
  p <- ggplot2::ggplot(
    data_i,
    aes(x = seventh_instar_date ,
        y = bodymass_7th_instar_j0_fw,
        group = seventh_instar_date)
  ) +
    geom_boxplot(fill = "steelblue3") +
    labs(x = "Date of the experiment", y = "Bodymass at the start of 7th instar (mg fw)")  +
    theme(legend.position = "none")
  
  ggsave(
    filename = "bm_j0_fw_&_week.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = food_consumed_collection_days_dw, y = bodymass_7th_instar_j3_dw)) +
    geom_point(size = 2) +
    labs(x = "Total amount of food consumed (mg dw)", y = "Bodymass at the end of the 7th instar (mg dw)") +
    geom_smooth(
      formula = y ~ x,
      color = "steelblue3",
      span = 0.85,
      method = lm
    )
  
  ggsave(
    filename = "bm_j3_dw_&_food_consumed.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = food_consumed_collection_days_dw, y = bodymass_imago_dw)) +
    geom_point(size = 2) +
    labs(x = "Total amount of food consumed (mg dw)", y = "Bodymass of the imago (mg dw)") +
    geom_smooth(
      formula = y ~ x,
      color = "steelblue3",
      span = 0.85,
      method = lm
    )
  
  ggsave(
    filename = "bm_imago_dw_&_food_provided_fw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  ##########  3. Graphics and figures  ##########
  data_i$food_provided_fw = as.factor(data_i$food_provided_fw)
  
  ###### Proportional fluxes ######
  
  ### Absorption efficiency according to mass specific ingestion rate ###
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = absorption_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Absorption efficiency (% dw)") +
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
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Growth efficiency (% fw)") +
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
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg dw indiv)", y = "Growth efficiency (% dw)") +
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
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_7th_instar_j3_fw + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_rate)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg fw/day / mg fw indiv)", y = "Growth rate (mg fw / day") +
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
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = growth_rate, y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Growth rate (mg fw / day)", y = "Growth efficiency (% fw)") +
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
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = absorbed_mass_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_investment_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific absorbed mass (% dw)", y = " Growth investment (% dw)") +
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
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = absorption_efficiency_dw, y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Absoprtion efficiency (% dw)", y = "Growth efficiency (% dw)") +
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
  
  ###### Element absorption efficiency according to total mass-specific intake rate  ######
  
  data_g = tidyr::pivot_longer(
    data_g,
    cols = c(
      "C_absorption_efficiency_dw",
      "N_absorption_efficiency_dw",
      "P_absorption_efficiency_dw"
    ),
    names_to = "element",
    values_to = "elemental_absorption_efficiency_dw"
  )
  data_g$element = gsub('_absorption_efficiency_dw','',data_g$element)

  p <- ggplot2::ggplot(
    data_g,
    aes(
      x = group_mass_specific_intake_rate_fw,
      y = elemental_absorption_efficiency_dw,
      colour = factor(element),
      fill =  factor(element)
    )
  ) +
    geom_point(size = 2,
               show.legend = T) +
    geom_smooth(span = 0.85,
                show.legend = T) +
    scale_color_manual(
      values = c("C" = "#909090", "N" = "#3050F8", "P" = "#FF8000"),
      aesthetics = c("colour","fill")) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = "Elemental absorption efficiency (%dw)")
  
  
  
  ggsave(
    filename = "eaedw_&_msirfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  
}