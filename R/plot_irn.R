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
      "P_absorption_efficiency_dw",
      "S_absorption_efficiency_dw",
      "Na_absorption_efficiency_dw",
      "Mg_absorption_efficiency_dw",
      "K_absorption_efficiency_dw",
      "Ca_absorption_efficiency_dw"
    ),
    names_to = "element",
    values_to = "elemental_absorption_efficiency_dw"
  )
  
  data_g$element = gsub('_absorption_efficiency_dw', '', data_g$element)
  
  CNP <- ggplot2::ggplot(
    subset(
      data_g,
      data_g$element == "C" |
        data_g$element == "N" | data_g$element == "P"
    ) ,
    aes(
      x = group_mass_specific_intake_rate_fw,
      y = elemental_absorption_efficiency_dw,
      colour = element,
      fill =  element
    )
  ) +
    geom_point(size = 2) +
    geom_smooth(span = 0.85) +
    scale_color_manual(
      values = c("C" = "#808080", "N" = "#5A5ACA", "P" = "#EC9200"),
      aesthetics = c("colour", "fill")
    ) +
    labs(
      x = "Mass-specific intake rate (mg fw/ day / mg fw)",
      y = "Elemental absorption efficiency (%dw)",
      fill = "Element",
      color = "Element"
    )
  
  
  
  ggsave(
    filename = "CNPaedw_&_msirfw.pdf",
    plot = CNP,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  eae_plots = vector("list", 5)
  names(eae_plots) = c("Na", "Mg", "S", "K", "Ca")
  colours = c(
    "Na" = "#403EFF",
    "Mg" = "#5CC55C",
    "S" = "#D69F09",
    "K" = "#9B4BE1",
    "Ca" = "#DF4F4F"
  )
  
  # A for loop to create the plots of absortpion efficiency according to mass-specific ingestion rate for
  # Na, Mg, S, K, and Ca
  
  for (i in 1:length(eae_plots)) {
    e_name = names(eae_plots)[i]
    eae_plots[[i]] = ggplot2::ggplot(
      subset(data_g, data_g$element == e_name) ,
      aes(
        x = group_mass_specific_intake_rate_fw,
        y = elemental_absorption_efficiency_dw,
        colour = element,
        fill =  element
      )
    ) +
      geom_point(size = 2) +
      geom_smooth(span = 0.85) +
      scale_color_manual(values = colours[i],
                         aesthetics = c("colour", "fill")) +
      labs(
        x = "Mass-specific intake rate (mg fw/ day / mg fw)",
        y = "Elemental absorption efficiency (%dw)",
        fill = "Element",
        color = "Element"
      )
    
    # Save each plot
    ggsave(
      filename = paste(e_name, "aedw_&_msirfw.pdf"),
      plot = eae_plots[[i]],
      device = cairo_pdf,
      path = here::here("4_outputs"),
      scale = 1,
      width = 6,
      height = 4,
      units = "in"
    )
    
  }
  
  # Set a new theme to produce the complete figure
  
  
  ggplot2::theme_set(
    theme_classic() + theme(
      text = element_text(size = 14),
      panel.grid.major = element_line(
        color = "gray95",
        size = 0.5,
        linetype = 1
      ),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  )
  
  # Create the legend plot
  
  legend_colours = c(
    "C" = "#808080",
    "N" = "#5A5ACA",
    "P" = "#EC9200",
    "Na" = "#403EFF",
    "Mg" = "#5CC55C",
    "S" = "#D69F09",
    "K" = "#9B4BE1",
    "Ca" = "#DF4F4F"
  )
  legend = ggplot(
    data_g,
    aes(
      x = group_mass_specific_intake_rate_fw,
      y = elemental_absorption_efficiency_dw,
      color = element,
      fill =  element
    )
  ) +
    geom_point() +
    lims(x = c(0, 0), y = c(0, 0)) +
    theme_void() +
    theme(legend.position = c(0.5, 0.5)) +
    scale_color_manual(values = legend_colours,
                       aesthetics = c("colour", "fill")) +
    guides(colour = guide_legend(override.aes = list(size = 8), ncol=2))
  
  # Creating the complete absorption efficiency plot
  
  ae_all = ggpubr::ggarrange(
    CNP,
    ggpubr::ggarrange(
      eae_plots[[1]],
      eae_plots[[2]],
      eae_plots[[3]],
      eae_plots[[4]],
      legend,
      eae_plots[[5]],
      ncol = 2,
      nrow = 3,
      labels = c("b.", "c.", "d.", "e.", "", "f."),
      label.y= 1.1,
      label.x = 0
    ),
    ncol = 2,
    nrow = 1,
    labels = "a."
  )
  
  # Annotating the complete absorption efficiency plot with axes titles
  
  
  ae_all = ggpubr::annotate_figure(
    ae_all,
    bottom = ggpubr::text_grob("Mass-specific intake rate (mg dw/mg dw)"),
    left = ggpubr::text_grob("Element absorption efficiency (%dw)", rot = 90),
    top= ""
  )
  
  
  # Saving the the complete absorption efficiency plot
  
  ggsave(
    filename = "aealldw_&_msirfw.pdf",
    plot = ae_all,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  
}