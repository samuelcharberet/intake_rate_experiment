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
  
  ########## 0. Preliminary figures ##########
  
  
  ###### Effect of week on bodymass at the start ######
  
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
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Dry weight bodymass at the end of the experiment according to food consumed  ######
  
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
    path = here::here("4_outputs", "2_figures"),
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
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  
  # The food provided daily is a factor rather than a numeric variable
  data_i$food_provided_fw = as.factor(data_i$food_provided_fw)
  
  ########## 1. Mass balance figures ##########
  
  
  ###### Absorption efficiency according to mass specific intake rate ######
  
  
  aedw_msirfw <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = absorption_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Absorption efficiency (% dw)") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "aedw_&_msirfw.pdf",
    plot = aedw_msirfw,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Growth efficiency in fresh weight according to the mass specific intake rate in fresh weight  ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_efficiency_fw)) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency (% fw)") +
    geom_point(size = -1, color = "white") +
    geom_vline(xintercept = 1, color = "steelblue3")
  
  breaks = ggplot_build(p)$layout$panel_params[[1]]$x$breaks
  p <- p + scale_x_continuous(breaks = sort(c(breaks, 1)))
  
  ggsave(
    filename = "gefw_&_msirfw_blankone.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_efficiency_fw)) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency (% fw)") +
    geom_point(size = -1, color = "white") +
    geom_hline(yintercept = 0.5, color = "steelblue3")
  
  ggsave(
    filename = "gefw_&_msirfw_blanktwo.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  gefw_msirfw <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_fw / ((bodymass_last_collection_date + bodymass_7th_instar_j0_fw) / 2
                       ), y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency (% fw)") +
    geom_smooth(color = "steelblue3",  method = "gam") +
    geom_vline(xintercept = 1, color = "steelblue3") +
    geom_hline(yintercept = 0.5, color = "steelblue3")
  
  
  ggsave(
    filename = "gefw_&_msirfw.pdf",
    plot = gefw_msirfw,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Growth efficiency in dry weight according to the mass specific intake rate in dry weight  ######
  
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = ingestion_rate_dw / ((
                         bodymass_7th_instar_j3_dw + bodymass_7th_instar_j0_fw * (1 - larvae_day0_wc)
                       ) / 2
                       ), y = growth_efficiency_dw)) +
    geom_point(size = 2) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)", y = "Growth efficiency (% dw)") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "gedw_&_msirdw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Growth rate in fresh weight according to the mass specific intake rate  ######
  
  grfw_msirfw <- ggplot2::ggplot(data_i,
                                 aes(x = ingestion_rate_fw / ((bodymass_7th_instar_j3_fw + bodymass_7th_instar_j0_fw) / 2
                                 ), y = growth_rate)) +
    geom_point(size = 2) +
    xlim(0, NA) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)",
         y = expression(paste("Growth rate",
                              " (",
                              mg[body (fw)],
                              " ", day ^ {
                                -1
                              },
                              ")", ))) +
    geom_smooth(color = "steelblue3",  method = "gam") +
    theme(axis.title.x = element_markdown())
  
  ggsave(
    filename = "grfw_&_msirfw.pdf",
    plot = grfw_msirfw,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ###### Growth efficiency in fresh weight according to growth rate  in fresh weight ######
  
  p <- ggplot2::ggplot(data_i,
                       aes(x = growth_rate, y = growth_efficiency_fw)) +
    geom_point(size = 2) +
    labs(x = "Growth rate (mg fw / day)", y = "Growth efficiency (% fw)") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "gefw_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
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
    path = here::here("4_outputs", "2_figures"),
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
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### A figure capturing important results in mass budget ######
  
  complete_plot = ggpubr::ggarrange(
    ggpubr::ggarrange(
      grfw_msirfw,
      aedw_msirfw,
      ncol = 1,
      nrow = 2,
      labels = c("a.",
                 "b."),
      label.y = 1.1,
      label.x = 0,
      heights = c(1, 1)
    ),
    gefw_msirfw,
    ncol = 2,
    nrow = 1,
    labels = c("",
               "c."),
    label.y = 1.1,
    label.x = 0,
    widths = c(1, 1)
  )
  
  ggsave(
    filename = paste("total_mass_balance.pdf", sep = ""),
    plot = complete_plot,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 13,
    height = 7,
    units = "in"
  )
  
  ########## 2. Chemical balance figures ##########
  
  
  # Options for the plots
  
  matrices = c("larvae", "egestion", "absorption")
  elements = c("C", "N", "P", "Na", "Mg", "S", "K", "Ca", "15N", "13C")
  nb_matrices = length(matrices)
  nb_elements = length(elements)
  colours = c(
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
  ) # The colors used for elements, modified after Jmol
  
  plots = vector("list", nb_matrices)
  names(plots) = matrices
  
  
  
  ###### Differences in elemental content between the matrices: food, larva, frass ######
  
  elements = c("C", "N", "P", "Na", "Mg", "S", "K", "Ca")
  nb_elements = length(elements)
  units = c("%", "%", "ppm", "ppm", "ppm", "ppm", "ppm", "ppm")
  
  plots_matrices = vector("list", nb_elements)
  
  for (i in 1:nb_elements) {
    data_element = as.data.frame(subset(data_g, data_g$element == elements[i]))
    food_col = which(names(data_element) == paste("food_", elements[i], sep =
                                                    ""))
    
    # Food elemental content
    average_food = mean(data_element[, food_col])
    sd_food = sd(data_element[, food_col])
    
    # Larvae elemental content
    average_larvae = mean(data_element[which(data_element$matrix == "larvae"),]$elemental_value, na.rm =
                            T)
    sd_larvae = sd(data_element[which(data_element$matrix == "larvae"),]$elemental_value, na.rm =
                     T)
    
    # Egestion (frass) elemental content
    average_egestion = mean(data_element[which(data_element$matrix == "egestion"),]$elemental_value, na.rm =
                              T)
    sd_egestion = sd(data_element[which(data_element$matrix == "egestion"),]$elemental_value, na.rm =
                       T)
    data <- data.frame(
      name = c("Food", "Larvae", "Frass"),
      value = c(average_food, average_larvae, average_egestion),
      sd = c(sd_food, sd_larvae, sd_egestion)
    )
    
    # Bar plot + error bar
    p = ggplot(data) +
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
           y = paste(elements[i], " (", units[i], ")", sep = "")) +
      scale_x_discrete(limits = c("Food", "Larvae", "Frass"))
    
    plots_matrices[[i]] = p
    
    # Save each plot
    ggsave(
      filename = paste("matrices_", elements[i], ".pdf", sep = ""),
      plot = p,
      device = cairo_pdf,
      path = here::here("4_outputs", "2_figures"),
      scale = 1,
      width = 3,
      height = 3,
      units = "in"
    )
  }
  
  # Create the complete matrices plots
  complete_matrices = ggpubr::ggarrange(
    plots_matrices[[1]],
    plots_matrices[[2]],
    plots_matrices[[3]],
    plots_matrices[[4]],
    plots_matrices[[5]],
    plots_matrices[[6]],
    plots_matrices[[7]],
    plots_matrices[[8]],
    ncol = 4,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "d.",
               "e.",
               "f.",
               "g.",
               "h."),
    label.y = 1,
    label.x = 0,
    heights = c(1, 1),
    widths = c(1, 1, 1, 1)
  )
  
  # Save the complete plot
  ggsave(
    filename = paste("matrices_", "all_elements", ".pdf", sep = ""),
    plot = complete_matrices,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 13,
    height = 7,
    units = "in"
  )
  
  ##### Growth rate hypothesis #####
  # We check whether the growth rate in positively related to P body content
  # according to the growth rate hypothesis of Elser
  data_larvae = subset(data_g, data_g$matrix == "larvae")
  data_larvae = pivot_wider(data_larvae, names_from = element, values_from = elemental_value)
  
  p <- ggplot2::ggplot(data_larvae,
                       aes(x = growth_rate, y = P)) +
    geom_point(size = 2) +
    labs(x = expression(paste("Growth rate", " (", mg[body (fw)], " ", day ^ {
      -1
    }, ")")), y = "Larvae P (ppm)") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "larvaePdw_&_gr.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  ###### Elements absorption efficiency, larval content, egestion content according to total mass-specific intake rate  ######
  
  
  y_axes = c("Larvae", "Frass", "Absorbed")
  units = cbind(units, units, "%")
  
  y_plot_names = c("larvae_content", "egestion_content", "elemental_abs_eff")
  
  # Creating the list of plots for the other elements
  
  for (i in 1:nb_matrices) {
    plots[[i]] = vector("list", nb_elements)
    names(plots[[i]]) = elements
  }
  
  
  # A for loop to create the plots of absorption efficiency, larvae content and egestion content
  # according to mass-specific intake rate for all elements
  
  
  
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
          x = expression(
            paste(
              "Intake rate",
              " (",
              mg[food(fw)],
              " ",
              mg[body (fw)] ^ {
                -1
              },
              " ",
              day ^ {
                -1
              },
              ")",
            )
          ),
          y = paste(y_axes[j], elements[i], paste("(", units[i, j], ")", sep =
                                                    ""), sep = " ") ,
          fill = "Element",
          color = "Element"
        )
      
      # Save each plot
      ggsave(
        filename = paste(matrices[j], elements[i], "dw_&_msirfw.pdf", sep = ""),
        plot = plots[[j]][[i]],
        device = cairo_pdf,
        path = here::here("4_outputs", "2_figures"),
        scale = 1,
        width = 6,
        height = 4,
        units = "in"
      )
      
      ### Relative absorption efficiency
      
      if (y_axes[j] == "Absorbed") {
        plot = ggplot2::ggplot(
          subset(data_matrix, data_matrix$element == elements[i]) ,
          aes(
            x = group_mass_specific_intake_rate_fw,
            y = elemental_value / absorption_efficiency_dw,
            colour = element,
            fill =  element
          )
        ) +
          geom_point(size = 2) +
          geom_smooth(method = "gam") +
          scale_color_manual(values = colours[i],
                             aesthetics = c("colour", "fill")) +
          labs(
            x = expression(
              paste(
                "Intake rate",
                " (",
                mg[food(fw)],
                " ",
                mg[body (fw)] ^ {
                  -1
                },
                " ",
                day ^ {
                  -1
                },
                ")",
              )
            ),
            y = paste("Relative", elements[i], "absorption"),
            fill = "Element",
            color = "Element"
          )
        
        # Save each plot
        ggsave(
          filename = paste(
            "relative",
            matrices[j],
            elements[i],
            "dw_&_msirfw.pdf",
            sep = ""
          ),
          plot = plot,
          device = cairo_pdf,
          path = here::here("4_outputs", "2_figures"),
          scale = 1,
          width = 6,
          height = 4,
          units = "in"
        )
      }
    }
  }
  
  ######  CNP stoichiometry ######
  
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
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = "Larvae C/N") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "cnlarvae_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_larvae,
                       aes(x = group_mass_specific_intake_rate_fw, y = N_P)) +
    geom_point(size = 2) +
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = "Larvae N/P") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "nplarvae_&_grfw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
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
  
  # CN_egestion = f(msir)
  
  p <- ggplot2::ggplot(data_egestion,
                       aes(x = group_mass_specific_intake_rate_fw, y = C_N)) +
    geom_point(size = 2) +
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = "Egestion C/N") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "cnegestion_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # CN_egestion = f(ge)
  
  p <- ggplot2::ggplot(data_egestion,
                       aes(x = growth_efficiency_fw, y = C_N)) +
    geom_point(size = 2) +
    labs(x = "Growth efficiency (% fw)", y = "Egestion C/N") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "cnegestion_&_ge.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 6,
    height = 4,
    units = "in"
  )
  
  # NP_egestion = f(ge)
  
  
  p <- ggplot2::ggplot(data_egestion,
                       aes(x = group_mass_specific_intake_rate_fw, y = N_P)) +
    geom_point(size = 2) +
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = "Egestion N/P") +
    geom_smooth(color = "steelblue3",  method = "gam")
  
  ggsave(
    filename = "npegestion_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
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
      axis.title.x = element_blank()
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
  
  
  ######  Creating the complete absorption efficiency, larvae and egestion composition plots ######
  
  complete_plots = vector("list", nb_matrices)
  
  for (i in 1:nb_matrices) {
    complete_plots[[i]] = ggpubr::ggarrange(
      plots[[i]][[1]],
      plots[[i]][[2]],
      plots[[i]][[3]],
      plots[[i]][[4]],
      NULL,
      NULL,
      NULL,
      NULL,
      plots[[i]][[5]],
      plots[[i]][[6]],
      plots[[i]][[7]],
      plots[[i]][[8]],
      ncol = 4,
      nrow = 3,
      labels = c("a.",
                 "b.",
                 "c.",
                 "d.",
                 "",
                 "",
                 "",
                 "",
                 "d.",
                 "e.",
                 "f.",
                 "g."),
      label.y = 1.1,
      label.x = 0,
      heights = c(1, 0.05, 1),
      widths = c(1, 1, 1, 1)
    )
    
    # Annotating the complete absorption efficiency plot with axes titles
    
    
    complete_plots[[i]] = ggpubr::annotate_figure(
      complete_plots[[i]],
      bottom = ggpubr::text_grob(expression(
        paste(
          "Intake rate",
          " (",
          mg[food(fw)],
          " ",
          mg[body (fw)] ^ {
            -1
          },
          " ",
          day ^ {
            -1
          },
          ")",
        )
      )),
      left = ggpubr::text_grob(y_axes[i], rot = 90),
      top = ""
    )
    
    # Saving the the complete plots
    
    ggsave(
      filename = paste(y_plot_names[i], "alldw_&_msirfw.pdf", sep = ""),
      plot = complete_plots[[i]],
      device = cairo_pdf,
      path = here::here("4_outputs", "2_figures"),
      scale = 1,
      width = 13,
      height = 5,
      units = "in"
    )
  }
  
  
  
  
  
  ########## 3. Isotopy figures ##########
  
  ######  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of MSIR ######
  
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
  
  data_tf = subset(data_g, data_g$matrix == "tf")
  data_tf = pivot_wider(data_tf,
                        names_from = element,
                        values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13ctf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = expression(
      paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
        -1
      }, " ", day ^ {
        -1
      }, ")",)
    ), y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15ntf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of growth rate ######
  
  
  ctf_gr <- ggplot2::ggplot(data_tf,
                            aes(x = growth_rate, y = `13C`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = expression(paste("Growth rate",
                              " (",
                              mg[body (fw)],
                              " ", day ^ {
                                -1
                              },
                              ")", )), y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13ctf_&_gr.pdf",
    plot = ctf_gr,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ntf_gr <- ggplot2::ggplot(data_tf,
                            aes(x = growth_rate, y = `15N`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = expression(paste("Growth rate",
                              " (",
                              mg[body (fw)],
                              " ", day ^ {
                                -1
                              },
                              ")", )), y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15ntf_&_gr.pdf",
    plot = ntf_gr,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of mass-specific growth rate ######
  
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = growth_rate / ((groupmass_7th_instar_j3_fw +
                                                 groupmass_7th_instar_j0_fw) / 2
                       ), y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Mass-specific growth rate (mg fw/ day)", y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13ctf_&_msgr.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = growth_rate / ((groupmass_7th_instar_j3_fw +
                                                 groupmass_7th_instar_j0_fw) / 2
                       ), y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Mass-specific growth rate (mg fw/ day)", y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15ntf_&_msgr.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic fractionation between the larvae and food (trophic fractionation) as a function of growth efficiency ######
  
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = growth_efficiency_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Growth efficiency (%fw)", y = expression(paste(Delta, "13C"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13ctf_&_gefw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_tf,
                       aes(x = growth_efficiency_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Growth efficiency (%fw)", y = expression(paste(Delta, "15N"))) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15ntf_&_gefw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic discrimination factor between frass and food (FFDF) according to MSIR ######
  
  data_ffdf = subset(data_g, data_g$matrix == "ffdf")
  data_ffdf = pivot_wider(data_ffdf,
                          names_from = element,
                          values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(
        paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",)
      ),
      y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{food}$)')
    ) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13cffdf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(
        paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",)
      ),
      y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{food}$)')
    ) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15nffdf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic discrimination factor between frass and food (FFDF) according to absorption efficiency ######
  
  data_ffdf = subset(data_g, data_g$matrix == "ffdf")
  data_ffdf = pivot_wider(data_ffdf,
                          names_from = element,
                          values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = absorption_efficiency_dw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(x = "Absorption efficiency (%)",
         y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{food}$)')) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13cffdf_&_aedw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_ffdf,
                       aes(x = absorption_efficiency_dw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(x = "Absorption efficiency (%)",
         y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{food}$)')) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15nffdf_&_aedw.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic discrimination factor between frass and larvae (FLDF) according to MSIR ######
  
  data_fldf = subset(data_g, data_g$matrix == "fldf")
  data_fldf = pivot_wider(data_fldf,
                          names_from = element,
                          values_from = elemental_value)
  
  
  p <- ggplot2::ggplot(data_fldf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `13C`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(
        paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",)
      ),
      y = latex2exp::TeX(r'($\delta 13C_{frass}-\delta 13C_{larvae}$)')
    ) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "13cfldf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_fldf,
                       aes(x = group_mass_specific_intake_rate_fw, y = `15N`)) +
    geom_point(size = 1.5) +
    labs(
      x = expression(
        paste("Intake rate", " (", mg[food(fw)], " ", mg[body (fw)] ^ {
          -1
        }, " ", day ^ {
          -1
        }, ")",)
      ),
      y = latex2exp::TeX(r'($\delta 15N_{frass}-\delta 15N_{larvae}$)')
    ) +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "15nfldf_&_msir.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Isotopic absorption efficiency ratio (IAER) according to MSIR ######
  
  data_aer = subset(data_g, data_g$matrix == "iaer")
  data_aer = pivot_wider(data_aer,
                         names_from = element,
                         values_from = elemental_value)
  
  
  ciaer_msir <- ggplot2::ggplot(data_aer,
                                aes(x = group_mass_specific_intake_rate_fw, y = `C`)) +
    geom_point(size = 1.5) +
    xlim(0, NA) +
    labs(x = "Intake rate <br> (mg<sub>food(fw)</sub> mg<sub>body(fw)</sub><sup>-1</sup> day<sup>-1</sup>)",
         y = "C IAER") +
    geom_smooth(color = "steelblue3",  method = "gam") +
    theme(axis.title.x = element_markdown())
  
  ggsave(
    filename = "ciaer_&_msir.pdf",
    plot = ciaer_msir,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  niaer_msir <- ggplot2::ggplot(data_aer,
                                aes(x = group_mass_specific_intake_rate_fw, y = `N`)) +
    geom_point(size = 1.5) +
    labs(x = expression(
      paste(
        "Intake rate",
        " (",
        mg[food (fw)],
        " ",
        mg[body (fw)] ^ {
          -1
        },
        " ",
        day ^ {
          -1
        },
        ")",
      )
    ), y = "N IAER") +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "niaer_&_msir.pdf",
    plot = niaer_msir,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  
  ######  Isotopic absorption efficiency ratio (IAER) according to AE ######
  
  
  p <- ggplot2::ggplot(data_aer,
                       aes(x = absorption_efficiency_dw, y = `C`)) +
    geom_point(size = 1.5) +
    labs(x = "Absorption efficiency (%)", y = "C IAER") +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "ciaer_&_ae.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  p <- ggplot2::ggplot(data_aer,
                       aes(x = absorption_efficiency_dw, y = `N`)) +
    geom_point(size = 1.5) +
    labs(x = "Absorption efficiency (%)", y = "N IAER") +
    geom_smooth(color = "steelblue3",  method = "lm")
  
  ggsave(
    filename = "niaer_&_ae.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 3,
    height = 2,
    units = "in"
  )
  
  ######  Creating the complete figure for isotopy paper  #####
  # A theme specific to this figure
  ggplot2::theme_set(
    theme_classic() + theme(
      text = element_text(size = 12),
      panel.grid.major = element_line(
        color = "gray95",
        size = 0.5,
        linetype = 1
      ),
      legend.position = "none",
    )
  )
  
  plots = list(grfw_msirfw, ciaer_msir, ctf_gr, ntf_gr)
  
  top_part = ggpubr::annotate_figure(
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
      panel.grid.major = element_line(
        color = "gray95",
        size = 0.5,
        linetype = 1
      ),
      legend.position = "none",
      axis.title.x = element_blank()
    )
  )
  
  bottom_part = ggpubr::annotate_figure(
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
    bottom = ggpubr::text_grob(expression(
      paste("Growth rate",
            " (",
            mg[body (fw)],
            " ", day ^ {
              -1
            },
            ")")
    ), size = 12)
  )
  
  
  
  # Creating the complete isotopy figure
  
  
  complete_plot = ggpubr::ggarrange(top_part, bottom_part, ncol = 1, nrow = 2)
  
  # Saving the the complete plots
  
  ggsave(
    filename = "isotopy_figure.pdf",
    plot = complete_plot,
    device = cairo_pdf,
    path = here::here("4_outputs", "2_figures"),
    scale = 1,
    width = 8,
    height = 6,
    units = "in"
  )
  
}
