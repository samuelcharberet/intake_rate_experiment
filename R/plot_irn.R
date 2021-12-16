#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_ic, data_gc){

  # Effect of week on bodymass at the start
  pdf(here::here("4_outputs", "bm_j0_ww_&_week.pdf"), width = 11, height = 8.5)
  boxplot(data = data_ic, bodymass_7th_instar_j0_ww ~ seventh_instar_date, xlab="Date of the experiment", ylab="Bodymass at the start 7th instar (mg ww)")
  dev.off()
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  pdf(here::here("4_outputs", "bm_j3_dw_&_food_consumed.pdf"), width = 11, height = 8.5)
  plot(data_ic$bodymass_7th_instar_j3_dw ~ data_ic$food_consumed_collection_days, xlab="Total amount of food consumed (mg dw)", ylab="Bodymass at the end of the 7th instar (mg dw)")
  dev.off()
  
  pdf(here::here("4_outputs", "bm_imago_dw_&_food_provided_ww.pdf"), width = 11, height = 8.5)
  plot(data_ic$bodymass_imago_dw ~ data_ic$food_consumed_collection_days, xlab="Total amount of food consumed (mg dw)", ylab="Bodymass of the imago (mg dw)")
  dev.off()
  
  ##########  3. Graphics and figures  ##########
  data_ic$food_provided_ww = as.factor(data_ic$food_provided_ww)
  
  
  ###### Growth efficiency ######
  
  # Growth efficiency according to the mass specific ingestion rate
  

  p <- ggplot2::ggplot(data_ic,
                  aes(x = ingestion_rate / ((
                    bodymass_last_collection_date + bodymass_7th_instar_j0_ww
                  ) / 2
                  ), y = growth_efficiency)) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + 
    labs(x = "Mass specific ingestion rate (mg dw/day / mg ww indiv)", y = "Growth efficiency (mg ww body / mg dw food)")+
    geom_smooth(color="steelblue3")

  ggsave(
    filename = "ge_&_msir.pdf",
    plot = p,
    device = "pdf",
    path = here::here("4_outputs"),
    scale = 1,
    width = 11,
    height = 8.5,
    units = "in")
  
  ###### Egestion ingestion ratio according to mass specific ingestion rate ######
  
  pdf(here::here("4_outputs", "eir_&_msir.pdf"), width = 11, height = 8.5)
  
  p <- ggplot2::ggplot(
    data_ic,
    aes(x = ingestion_rate / ((
      bodymass_last_collection_date + bodymass_7th_instar_j0_ww
    ) / 2
    ), y = egestion_ingestion_ratio)
  ) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + ggsci::scale_color_npg() +
    labs(x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
    geom_smooth(color="steelblue3")
  print(p)
  dev.off()
  
}