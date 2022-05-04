#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_ic, data_gc) {
  # Effect of week on bodymass at the start
  pdf(
    here::here("4_outputs", "bm_j0_ww_&_week.pdf"),
    width = 6,
    height = 4
  )
  boxplot(
    data = data_ic,
    bodymass_7th_instar_j0_ww ~ seventh_instar_date,
    xlab = "Date of the experiment",
    ylab = "Bodymass at the start of 7th instar (mg ww)"
  )
  dev.off()
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  pdf(
    here::here("4_outputs", "bm_j3_dw_&_food_consumed.pdf"),
    width = 6,
    height = 4
  )
  plot(
    data_ic$bodymass_7th_instar_j3_dw ~ data_ic$food_consumed_collection_days,
    xlab = "Total amount of food consumed (mg dw)",
    ylab = "Bodymass at the end of the 7th instar (mg dw)"
  )
  dev.off()
  
  pdf(
    here::here("4_outputs", "bm_imago_dw_&_food_provided_ww.pdf"),
    width = 6,
    height = 4
  )
  plot(
    data_ic$bodymass_imago_dw ~ data_ic$food_consumed_collection_days,
    xlab = "Total amount of food consumed (mg dw)",
    ylab = "Bodymass of the imago (mg dw)"
  )
  dev.off()
  
  ##########  3. Graphics and figures  ##########
  data_ic$food_provided_ww = as.factor(data_ic$food_provided_ww)
  
  ###### Proportional fluxes ######
  
  ### Absorption efficiency according to mass specific ingestion rate ###
  
  pdf(here::here("4_outputs", "ae_&_msir.pdf"),
      width = 6,
      height = 4)
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate / ((bodymass_last_collection_date + bodymass_7th_instar_j0_ww) / 2
                       ), y = absorption_efficiency_dw)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Absorption efficiency (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  print(p)
  dev.off()
  
  
  ###### Growth efficiency ######
  
  # Growth efficiency in fresh weight according to the mass specific ingestion rate
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate / ((bodymass_last_collection_date + bodymass_7th_instar_j0_ww) / 2
                       ), y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg ww indiv)", y = "Growth efficiency (mg ww body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85) +
    theme_minimal()
  
  ggsave(
    filename = "gefw_&_msir.pdf",
    plot = p,
    device = "pdf",
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Growth efficiency in dry weight according to the mass specific ingestion rate
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_ww * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg dw indiv)", y = "Growth efficiency (mg dw body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85) +
    theme_minimal()
  
  ggsave(
    filename = "gedw_&_msir.pdf",
    plot = p,
    device = "pdf",
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # Growth rate in fresh weight according to the mass specific ingestion rate
  
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = ingestion_rate / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_ww * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_rate)) +
    geom_point(size = 2) +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg dw indiv)", y = "Growth rate (mg fw body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85) +
    theme_minimal()
  
  ggsave(
    filename = "grfw_&_msir.pdf",
    plot = p,
    device = "pdf",
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
    labs(x = "Growth rate (mg ww / day)", y = "Growth efficiency (mg ww body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85) +
    theme_minimal()
  
  ggsave(
    filename = "gefw_&_grfwd.pdf",
    plot = p,
    device = "pdf",
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Egestion ingestion ratio according to growth efficiency in fresh weight  ######
  
  pdf(here::here("4_outputs", "eir_&_gefw.pdf"),
      width = 6,
      height = 4)
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = growth_efficiency_fw, y = egestion_ingestion_ratio_dw)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(x = "Growth efficiency (mg fw body / mg dw food)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  print(p)
  dev.off()
  
  ###### Egestion ingestion ratio according to growth efficiency in fresh weight  ######
  
  pdf(here::here("4_outputs", "eir_&_gedw.pdf"),
      width = 6,
      height = 4)
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = growth_efficiency_dw, y = egestion_ingestion_ratio_dw)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(x = "Growth efficiency (mg dw body / mg dw food)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  print(p)
  dev.off()
  
  ###### Growth investment according to mass specific amount absorbed  ######
  
  pdf(here::here("4_outputs", "gi_&_msaa.pdf"),
      width = 6,
      height = 4)
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = absorbed_mass_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_ww * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_investment)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(x = "Mass-specific absorbed mass (mg dw/ mg dw)", y = " Growth investment (mg dw / mg dw)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  print(p)
  dev.off()
  
  
  
  
  
  ###### Absorption efficiency according to Growth efficiency ######
  
  pdf(here::here("4_outputs", "ae_&_gedw.pdf"),
      width = 6,
      height = 4)
  
  p <- ggplot2::ggplot(data_ic,
                       aes(x = 1 - egestion_ingestion_ratio_dw, y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(x = "Absoprtion efficiency (mg dw / mg dw)", y = "Growth efficiency (mg dw body / mg dw food)") +
    geom_smooth(color = "steelblue3", span = 0.85)
  print(p)
  dev.off()
  
}