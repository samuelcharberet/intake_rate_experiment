library(tidyverse)
library(readxl)

# Load data
data <- read_excel("C:/Users/scharber/Desktop/Global_heterotroph_stoichio_v5.xlsx")

# Define variable types
factor_columns <- c("Diet_full", "Functional_group", "Habitat")
numeric_columns <- c("Mass_dry_full", "Mass_log10", "C_mean", "N_mean", "P_mean", "CN_ratio", "CP_ratio", "NP_ratio")

# Clean data types
data <- data %>%
  mutate(across(all_of(factor_columns), as.factor)) %>%
  mutate(across(all_of(numeric_columns), as.numeric))

# Filter to relevant subset
classes <- c(
  "Mammalia", "Malacostraca", "Insecta", "Hexanauplia", "Pilidiophora", "Gastropoda", "Polychaeta",
  "Asteroidea", "Holothuroidea", "Actinopteri", "Thaliacea", "Monogononta", "Bivalvia", "Branchiopoda",
  "Clitellata", "Hydrozoa", "Nuda", "Copepod", "Cephalopoda", "Ostracoda", "Tentaculata", "Appendicularia",
  "Polychaete", "Sagittoidea", "Amphibia", "Scyphozoa", "Arachnida", "Diplopoda", "Collembola",
  "Aves", "Citellata", "Chondrichthyes"
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
  select(Species_binomial, C_mean, N_mean) %>%
  pivot_longer(cols = c(C_mean, N_mean),
               names_to = "Element",
               values_to = "Concentration") %>%
  filter(!is.na(Concentration)) %>%
  filter(
    (Element == "C_mean" & Species_binomial %in% species_C) |
      (Element == "N_mean" & Species_binomial %in% species_N)
  )

# Plot
ggplot(nutrients_long, aes(x = Species_binomial, y = Concentration, color = Element)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  facet_wrap(~Element, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Species", y = "Elemental concentration (%)")
