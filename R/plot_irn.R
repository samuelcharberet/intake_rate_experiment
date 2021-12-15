#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_ic, data_gc){
  
  # Effect of week on bodymass at the start
  jpeg(here::here("4_outputs", "bm_j0_ww_&_week.jpg"), width = 350, height = "350")
  boxplot(data = data_i, bodymass_7th_instar_j0_ww ~ seventh_instar_date)
  dev.off()
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  jpeg(here::here("4_outputs", "bm_j3_dw_&_food_consumed.jpg"), width = 350, height = "350")
  plot(data_i$bodymass_7th_instar_j3_dw ~ data_i$food_consumed_collection_days)
  dev.off()
  
  jpeg(here::here("4_outputs", "bm_imago_dw_&_food_provided_ww.jpg"), width = 350, height = "350")
  boxplot(data = data_i, bodymass_imago_dw ~ food_provided_ww)
  dev.off()
  
  ##########  3. Graphics and figures  ##########
  data_i$food_provided_ww = as.factor(data_i$food_provided_ww)
  
  
  ###### Growth efficiency ######
  
  # Growth efficiency according to the mass specific ingestion rate
  
  jpeg(here::here("4_outputs", "ge_&_msir.jpg"), width = 350, height = "350")
  
  ggplot2::ggplot(data_i,
                  aes(x = ingestion_rate / ((
                    bodymass_last_collection_date + bodymass_7th_instar_j0_ww
                  ) / 2
                  ), y = growth_efficiency)) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + 
    labs(title = "Growth efficiency of S. littoralis according to mass-specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg ww indiv)", y = "Growth efficiency (mg ww body / mg dw food)")+
    geom_smooth(color="steelblue3")
  
  dev.off()
  
  ###### Egestion ingestion ratio according to mass specific ingestion rate ######
  
  jpeg(here::here("4_outputs", "eir_&_msir.jpg"), width = 350, height = "350")
  
  ggplot2::ggplot(
    data_i,
    aes(x = ingestion_rate / ((
      bodymass_last_collection_date + bodymass_7th_instar_j0_ww
    ) / 2
    ), y = egestion_ingestion_ratio)
  ) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + ggsci::scale_color_npg() +
    labs(title = "Egestion / ingestion ratio according to mass specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
    geom_smooth(color="steelblue3")
  
  dev.off()
  
}