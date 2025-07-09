#' get_body_nutrient_litereature
#'
#' @return a clean data tibble containing literature data on whole body chemical analysis of
#'  animal specimen with > 30 obs per species.
#'


get_body_nutrient_literature_data = function() {
  # Download data if required
  if (file.exists(here::here(
    "1_data",
    "external_data",
    "Global_heterotroph_stoichio_v5.xlsx"
  )) == F) {
    dir.create(file.path(here::here("1_data", "external_data")))
    download.file(
      "https://figshare.com/ndownloader/files/25757327",
      destfile = here::here(
        "1_data",
        "external_data",
        "Global_heterotroph_stoichio_v5.xlsx"
      ),
      mode = "wb"
    )
  }
  
  data <- readxl::read_excel(here::here(
    "1_data",
    "external_data",
    "Global_heterotroph_stoichio_v5.xlsx"
  ))
  
  # Define variable types
  factor_columns <- c("Diet_full", "Functional_group", "Habitat")
  numeric_columns <- c(
    "Mass_dry_full",
    "Mass_log10",
    "C_mean",
    "N_mean",
    "P_mean",
    "CN_ratio",
    "CP_ratio",
    "NP_ratio"
  )
  
  # Clean data types
  data <- data %>%
    mutate(across(all_of(factor_columns), as.factor)) %>%
    mutate(across(all_of(numeric_columns), as.numeric))
  
  # Filter to relevant subset
  classes <- c(
    "Mammalia",
    "Malacostraca",
    "Insecta",
    "Hexanauplia",
    "Pilidiophora",
    "Gastropoda",
    "Polychaeta",
    "Asteroidea",
    "Holothuroidea",
    "Actinopteri",
    "Thaliacea",
    "Monogononta",
    "Bivalvia",
    "Branchiopoda",
    "Clitellata",
    "Hydrozoa",
    "Nuda",
    "Copepod",
    "Cephalopoda",
    "Ostracoda",
    "Tentaculata",
    "Appendicularia",
    "Polychaete",
    "Sagittoidea",
    "Amphibia",
    "Scyphozoa",
    "Arachnida",
    "Diplopoda",
    "Collembola",
    "Aves",
    "Citellata",
    "Chondrichthyes"
  )
  
  filtered_data <- data %>%
    filter(Class %in% classes,
           Stoichio_analysis == "Whole body",
           Species_binomial != "NA")
  
  # Now: count species separately for each element
  species_C <- filtered_data %>%
    filter(!is.na(C_mean)) %>%
    count(Species_binomial) %>%
    filter(n > 30) %>%
    pull(Species_binomial)
  
  species_N <- filtered_data %>%
    filter(!is.na(N_mean)) %>%
    count(Species_binomial) %>%
    filter(n > 30) %>%
    pull(Species_binomial)
  
  
  # Pivot to long format, then apply separate filters
  nutrients_long <- filtered_data %>%
    select(Species_binomial, C_mean, N_mean, doi) %>%
    pivot_longer(
      cols = c(C_mean, N_mean),
      names_to = "Element",
      values_to = "Concentration"
    ) %>%
    filter(!is.na(Concentration)) %>%
    filter(
      (Element == "C_mean" & Species_binomial %in% species_C) |
        (Element == "N_mean" & Species_binomial %in% species_N)
    )
  
  
  return(nutrients_long)
  
}


# 4. Compare body chemical composition variation to published literature ##########

# 1. Clean and prepare experimental data
filtered_g_clean <- data_g %>%
  filter(element %in% c("C", "N"), variable == "larvae") %>%
  mutate(
    Element = recode(element, "C" = "C_mean", "N" = "N_mean"),
    Species_binomial = "Our experiment",
    doi = NA_character_,
    Concentration = elemental_value
  ) %>%
  select(Species_binomial, doi, Element, Concentration)

# 2. Combine with published data and set factor levels
data_combined <- bind_rows(data_bl, filtered_g_clean) %>%
  filter(!is.na(Concentration)) %>%
  mutate(Species_binomial = factor(Species_binomial, levels = c(
    "Our experiment", setdiff(unique(Species_binomial), "Our experiment")
  )))

# 3. Define plot styling
bold_labels <- function(x) {
  ifelse(x == "Our experiment", paste0("**", x, "**"), x)
}

plot_element <- function(elem, col, title) {
  ggplot(
    filter(data_combined, Element == elem),
    aes(x = Species_binomial, y = Concentration)
  ) +
    geom_jitter(width = 0.2,
                alpha = 0.6,
                color = col) +
    scale_x_discrete(labels = bold_labels) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(title = title)
}

# 4. Generate and combine plots
plot_C <- plot_element("C_mean", "#808080", "Body C (%)")
plot_N <- plot_element("N_mean", "#5A5ACA", "Body N (%)")

# 5. Assemble with unified axis labels

complete <- ggpubr::ggarrange(
  plot_C,
  plot_N,
  ncol = 2,
  nrow = 1,
  labels = c("a.", "b."),
  label.y = 1,
  label.x = 0,
  heights = c(1, 1),
  widths = c(1)
)

complete <- ggpubr::annotate_figure(
  complete,
  bottom = gridtext::richtext_grob(
    "Species",
    hjust = 0.5,
    # Center alignment
    gp = grid::gpar(col = "black")  # Text color
  ),
  top = ""  # Set to an empty string if you don't want a top title
)

# Saving the the complete plots

ggsave(
  filename = "compare_body_var.pdf",
  plot = complete,
  device = pdf,
  path = here::here("4_outputs", "2_figures"),
  scale = 1,
  width = 8,
  height = 6,
  units = "in"
)

####Clearly the issue is that it comes only from one paper and some of the "species" seems to be genera ?

