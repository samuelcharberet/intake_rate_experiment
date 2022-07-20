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
    geom_smooth(formula = y ~ x,
                color = "steelblue3",
                method = "gam")
  
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
    geom_smooth(formula = y ~ x,
                color = "steelblue3",
                method = "gam")
  
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
    geom_smooth(color = "steelblue3",  method = "gam")
  
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
    geom_smooth(color = "steelblue3",  method = "gam")
  
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
    geom_smooth(color = "steelblue3",  method = "gam")
  
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
    geom_smooth(color = "steelblue3",  method = "gam")
  
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
    geom_smooth(color = "steelblue3",  method = "gam")
  
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
    geom_smooth(color = "steelblue3", method = "gam")
  
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
    geom_smooth(color = "steelblue3", method = "gam")
  
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
  
  nb_matrices = 3
  nb_elements = length(unique(data_g$element)) - 3
  matrices = c("larvae", "egestion", "absorption")
  elements = c("Na", "Mg", "S", "K", "Ca", "13C", "15N")
  colours = c(
    "Na" = "#403EFF",
    "Mg" = "#5CC55C",
    "S" = "#D69F09",
    "K" = "#9B4BE1",
    "Ca" = "#DF4F4F",
    "13C" = "black",
    "15N" = "black"
  ) # The colors used for elements, modified after Jmol
  
  plots = vector("list", nb_matrices)
  names(plots) = matrices
  
  CNP = vector("list", nb_matrices)
  names(CNP) = matrices
  y_axes = c("%dw", "%dw", "Elemental absorption efficiency (%dw)")
  y_plot_names = c("lc", "ec", "eae")
  
  for (i in 1:nb_matrices) {
    data_matrix = subset(
      data_g,
      data_g$matrix == names(CNP)[i] &
        data_g$element == "C" |
        data_g$matrix == names(CNP)[i] &
        data_g$element == "N" |
        data_g$matrix == names(CNP)[i] &
        data_g$element == "P"
    )
    
    y_axis_coef = mean(data_matrix$elemental_value[which(data_matrix$element ==
                                                           "C")], na.rm = T) / (mean(c(
                                                             mean(data_matrix$elemental_value[which(data_matrix$element == "N")], na.rm = T),
                                                             mean(data_matrix$elemental_value[which(data_matrix$element == "P")], na.rm =
                                                                    T)
                                                           )))
    
    if (names(CNP)[i] != "absorption" &
        names(CNP)[i] != "fractionation") {
      data_matrix$elemental_value[which(data_matrix$element ==
                                          "N")] = data_matrix$elemental_value[which(data_matrix$element ==
                                                                                      "N")] *
        y_axis_coef
      data_matrix$elemental_value[which(data_matrix$element ==
                                          "P")] = data_matrix$elemental_value[which(data_matrix$element ==
                                                                                      "P")] *
        y_axis_coef
    }
    CNP[[i]] <- ggplot2::ggplot(
      data_matrix
      ,
      aes(
        x = group_mass_specific_intake_rate_fw,
        y = elemental_value,
        colour = element,
        fill =  element
      )
    ) +
      {
        if (names(CNP)[i] != "absorption" &
            names(CNP)[i] != "fractionation")
          scale_y_continuous(sec.axis = sec_axis(~ . / y_axis_coef))
      } +
      geom_point(size = 2) +
      geom_smooth(method = "gam") +
      scale_color_manual(
        values = c("C" = "#808080", "N" = "#5A5ACA", "P" = "#EC9200"),
        aesthetics = c("colour", "fill")
      ) +
      labs(
        x = "Mass-specific intake rate (mg fw/ day / mg fw)",
        y = y_axes[i],
        fill = "Element",
        color = "Element"
      ) + {
        if (names(CNP)[i] != "absorption" &
            names(CNP)[i] != "fractionation")
          theme(
            axis.text.y = element_text(color = "#808080"),
            axis.line.y = element_line(color = "#808080"),
            axis.ticks.y = element_line(color = "#808080"),
            axis.text.y.right = element_text(color = "#5A5ACA"),
            axis.line.y.right = element_line(color = "#5A5ACA"),
            axis.ticks.y.right = element_line(color = "#5A5ACA")
          )
      }
    
    
    ggsave(
      filename = paste("CNP", y_plot_names[i], "dw_&_msirfw.pdf", sep = ""),
      plot = CNP[[i]],
      device = cairo_pdf,
      path = here::here("4_outputs"),
      scale = 1,
      width = 6,
      height = 4,
      units = "in"
    )
  }
  
  # Creating the list of plots
  
  for (i in 1:nb_matrices) {
    plots[[i]] = vector("list", nb_elements)
    names(plots[[i]]) = c("Na", "Mg", "S", "K", "Ca", "13C", "15N")
  }
  
  
  # A for loop to create the plots of absorption efficiency, larvae content and egestion content
  # according to mass-specific ingestion rate for
  # Na, Mg, S, K, and Ca
  
  
  for (j in 1:nb_matrices) {
    data_matrix = subset(data_g, data_g$matrix == matrices[j])
    for (i in 1:nb_elements) {
      plots[[j]][[i]] = ggplot2::ggplot(
        subset(data_matrix, data_matrix$element == elements[i]) ,
        aes(
          x = group_mass_specific_intake_rate_fw,
          y = elemental_value,
          colour = element,
          fill =  element
        )
      ) +
        geom_point(size = 2) +
        geom_smooth(method = "gam") +
        scale_color_manual(values = colours[i],
                           aesthetics = c("colour", "fill")) +
        labs(
          x = "Mass-specific intake rate (mg fw/ day / mg fw)",
          y = y_axes[j],
          fill = "Element",
          color = "Element"
        )
      
      # Save each plot
      ggsave(
        filename = paste(matrices[j], elements[i], "dw_&_msirfw.pdf", sep = ""),
        plot = plots[[j]][[i]],
        device = cairo_pdf,
        path = here::here("4_outputs"),
        scale = 1,
        width = 6,
        height = 4,
        units = "in"
      )
      
    }
  }
  
  # Making C:N and N:P plots for larvae and egestion as a function of
  # the mass-specific intake rate
  
  # For the larvae
  
  data_larvae = subset(data_g, data_g$matrix == "larvae")
  data_larvae = pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  data_larvae$C_N = data_larvae$C / data_larvae$N
  data_larvae$N_P = data_larvae$N / data_larvae$P
  
  p <- ggplot2::ggplot(data_larvae,
                       aes(x = group_mass_specific_intake_rate_fw, y = C_N)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = "Larvae C/N") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "cnlarvae_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_larvae,
                       aes(x = group_mass_specific_intake_rate_fw, y = N_P)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = "Larvae N/P") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "nplarvae_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # For egestions
  
  data_egestion = subset(data_g, data_g$matrix == "egestion")
  data_egestion = pivot_wider(data_egestion,
                              names_from = element,
                              values_from = elemental_value)
  data_egestion$C_N = data_egestion$C / data_egestion$N
  data_egestion$N_P = data_egestion$N / data_egestion$P
  
  p <- ggplot2::ggplot(data_egestion,
                       aes(x = group_mass_specific_intake_rate_fw, y = C_N)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = "Egestion C/N") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "cnegestion_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_egestion,
                       aes(x = group_mass_specific_intake_rate_fw, y = N_P)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = "Egestion N/P") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "npegestion_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  
  
  # Set a new theme to produce the complete figures
  
  
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
      y = elemental_value,
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
    guides(colour = guide_legend(override.aes = list(size = 8), ncol = 2))
  
  
  # Creating the complete absorption efficiency, larvae and egestion composition plots
  
  complete_plots = vector("list", nb_matrices)
  
  for (i in 1:nb_matrices) {
    complete_plots[[i]] = ggpubr::ggarrange(
      CNP[[i]],
      ggpubr::ggarrange(
        plots[[i]][[1]],
        plots[[i]][[2]],
        NULL,
        NULL,
        plots[[i]][[3]],
        plots[[i]][[4]],
        NULL,
        NULL,
        legend,
        plots[[i]][[5]],
        NULL,
        NULL,
        ncol = 2,
        nrow = 6,
        labels = c("b.", "c.", "", "", "d.", "e.", "", "", "", "f.", "", ""),
        label.y = 1.1,
        label.x = 0,
        heights = c(1, 0.05, 1, 0.05, 1, 0.1),
        widths = c(1, 1)
      ),
      ncol = 2,
      nrow = 1,
      label.y = 1.03,
      label.x = 0,
      labels = c("a.", ""),
      heights = 1,
      widths = c(1, 1)
    )
    
    # Annotating the complete absorption efficiency plot with axes titles
    
    
    complete_plots[[i]] = ggpubr::annotate_figure(
      complete_plots[[i]],
      bottom = ggpubr::text_grob("Mass-specific intake rate (mg dw/mg dw)"),
      left = ggpubr::text_grob(y_axes[i], rot = 90),
      top = ""
    )
    
    # Saving the the complete plots
    
    ggsave(
      filename = paste(y_plot_names[i], "alldw_&_msirfw.pdf", sep = ""),
      plot = complete_plots[[i]],
      device = cairo_pdf,
      path = here::here("4_outputs"),
      scale = 1,
      width = 8,
      height = 5,
      units = "in"
    )
  }
  
  ## Isotopic fractionation of the larvae
  
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
  
  data_fractionation_larvae = subset(data_g, data_g$matrix == "fractionation-larvae")
  data_fractionation_larvae = pivot_wider(data_fractionation_larvae,
                                          names_from = element,
                                          values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_fractionation_larvae,
                       aes(x = group_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = expression(paste("13C TF ( ", delta, ")"))) +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "13ctfl_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_fractionation_larvae,
                       aes(x = group_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = expression(paste("15N TF ( ", delta, ")"))) +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "15ntfl_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ## Isotopic fractionation of the egestion
  
  data_fractionation_egestion = subset(data_g, data_g$matrix == "fractionation-egestion")
  data_fractionation_egestion = pivot_wider(data_fractionation_egestion,
                                            names_from = element,
                                            values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(
    data_fractionation_egestion,
    aes(x = group_mass_specific_intake_rate_fw, y = `13C`)
  ) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = expression(paste("13C EDDF ( ", delta, ")"))) +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "13ceddf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(
    data_fractionation_egestion,
    aes(x = group_mass_specific_intake_rate_fw, y = `15N`)
  ) +
    geom_point(size = 2) +
    labs(x = "Mass-specific intake rate (mg fw/ day / mg fw)", y = expression(paste("15N EDDF ( ", delta, ")"))) +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "15neddf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
}
