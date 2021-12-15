#' plot_irn
#'
#' @return plots in the output folder
#' @export
#'
#' @examples

plot_irn <- function(data_ic, data_gc){
  
  # Dry weight bodymass at the end of the experiment according to food consumed
  jpeg(here::here("4_outputs", "rplot.jpg", width = 350, height = "350"))
  plot(data_intake$bodymass_7th_instar_j3_dw ~ data_intake$food_consumed_collection_days)
  dev.off()
  boxplot(data = data_intake, bodymass_imago_dw ~ food_provided_ww)
  
  ##########  3. Graphics and figures  ##########
  data_intake$food_provided_ww = as.factor(data_intake$food_provided_ww)
  
  ###### Treatment growth curve ######
  data_growth_summary$food_provided_ww = as.factor(data_growth_summary$food_provided_ww)
  p = ggplot2::ggplot(
    data_growth_summary,
    aes(
      x = day,
      y = average_bodymass,
      group = food_provided_ww,
      color = food_provided_ww
    )
  ) +
    geom_line() +
    geom_errorbar(
      aes(ymin = average_bodymass - sd_bodymass, ymax = average_bodymass + sd_bodymass),
      width = 0.2,
      position = position_dodge(0.1)
    ) +  geom_point()
  
  # Finished line plot
  p + labs(title = "Growth curve of S. littoralis according to provided food mass", x = "Day of 7th instar", y = "Bodymass (mg)") +
    theme_classic() +
    scale_x_discrete(breaks = unique(data_growth_summary$day),
                     labels = as.character(c(1:4, 17))) +
    ggsci::scale_color_npg()
  
  ###### Growth efficiency ######
  
  # Growth efficiency according to the mass specific ingestion rate
  ggplot2::ggplot(data_intake,
                  aes(x = ingestion_rate / ((
                    bodymass_last_collection_date + bodymass_7th_instar_j0_ww
                  ) / 2
                  ), y = growth_efficiency)) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + 
    labs(title = "Growth efficiency of S. littoralis according to mass-specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg ww indiv)", y = "Growth efficiency (mg ww body / mg dw food)")+
    geom_smooth(color="steelblue3")
  
  
  ###### Egestion ingestion ratio according to mass specific ingestion rate ######
  
  ggplot2::ggplot(
    data_intake,
    aes(x = ingestion_rate / ((
      bodymass_last_collection_date + bodymass_7th_instar_j0_ww
    ) / 2
    ), y = egestion_ingestion_ratio)
  ) +
    geom_point(size = 3) +
    hrbrthemes::theme_ipsum() + ggsci::scale_color_npg() +
    labs(title = "Egestion / ingestion ratio according to mass specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
    geom_smooth(color="steelblue3")
}