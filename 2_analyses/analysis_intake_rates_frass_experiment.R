################################ INTAKE RATE AND EGESTION QUALITY in Spodoptera littoralis  ################################

# This script analyses mass balance and chemical composition data and computes interesting intake, digestion, growth and egestion metrics.

library(readxl)
library(stringr)
library(flextable)
library(tibble)
library(openxlsx)
library(hrbrthemes)
library(GGally)
library(viridis)
library(plyr)
library(tidyr)
library(ggsci)
library(lubridate)
library(ggstatsplot)

setwd(
  "C:/Users/Samuel/Documents/7. Doctorat/2. Experiments/2. Spodoptera littoralis/4. Experiment/1. Data"
)

# Load the individual data
data_intake = read_xlsx(
  "C:/Users/Samuel/Documents/7. Doctorat/2. Experiments/2. Spodoptera littoralis/4. Experiment/1. Data/data_intake_rates_frass_experiment_individuals.xlsx"
)

# Load the control data

data_foodcontrol = read_xlsx(
  "C:/Users/Samuel/Documents/7. Doctorat/2. Experiments/2. Spodoptera littoralis/4. Experiment/1. Data/data_food_controls.xlsx"
)


##########  0. Structuration  ##########

str(data_intake)
data_intake$filled_tube_food_mass = as.numeric(data_intake$filled_tube_food_mass)
data_intake$bodymass_7th_instar_j16_ww = as.numeric(data_intake$bodymass_7th_instar_j16_ww)

# Removing individuals that underwent experimental errors

# Individual 38 was believed to undergo pre pupation too soon
data_intake = data_intake[-which(data_intake$individual_ID == "38"),]

# Individual 94 was a L6
data_intake = data_intake[-which(data_intake$individual_ID == "94"),]

##########  1. Filling the table  ##########

##### Last collection day #####

# We automatically define the last collection day based on whether or not the pre pupation occurred during the week

for (i in 1:nrow(data_intake)) {
  if (is.na(data_intake$last_collection_date[i] == T)) {
    if (is.na(data_intake$pre_pupa_date[i] == T)) {
      data_intake$last_collection_date[i] = data_intake$first_collection_date[i] + days(2)
    } else {
      if (is.na(data_intake$last_collection_date[i] == T)) {
        data_intake$last_collection_date[i] = data_intake$pre_pupa_date[i] - days(1)
      }
    }
  }
}

##### Number of collection day #####

data_intake$number_collection_days = as.numeric(data_intake$last_collection_date - data_intake$first_collection_date + 1)

##### Bodymass at the last collection date #####

# We create a column corresponding to the last bodymass measured before pre pupation, that is the bodymass at the last collection date
data_intake$bodymass_before_last_collection_date = NA

for (i in 1:nrow(data_intake)) {
  day_last_collection = as.numeric(data_intake$last_collection_date[i] - data_intake$first_collection_date[i]) +
    2
  data_intake$bodymass_before_last_collection_date[i] = as.numeric(data_intake[i, colnames(data_intake)[grepl("bodymass", colnames(data_intake))][day_last_collection]])
}

##### Growth rate #####

# We compute the growth rate on the 7th instar without prepupation
data_intake$growth_rate = (
  data_intake$bodymass_before_last_collection_date - data_intake$bodymass_7th_instar_j0_ww
) / data_intake$number_collection_days
data_intake$growth_rate_unit = "mg_ww/day"

##### Food water content #####

data_foodcontrol$food_ww = data_foodcontrol$ww_filled_tube_food_control_mass -
  data_foodcontrol$empty_tube_food_control_mass
data_foodcontrol$food_dw = data_foodcontrol$dw_filled_tube_food_control_mass -
  data_foodcontrol$empty_tube_food_control_mass
data_foodcontrol$food_water_content = (data_foodcontrol$food_ww - data_foodcontrol$food_dw) /
  data_foodcontrol$food_ww

##### Food provided in dw #####

data_intake$food_provided_dw = NA

for (i in 1:nrow(data_intake)) {
  seventh_instar_date = data_intake$seventh_instar_date[i]
  week_dates = c(
    seventh_instar_date,
    seventh_instar_date + 24 * 60 * 60,
    seventh_instar_date + 2 * 24 * 60 * 60
  )
  week_indexes = which(data_foodcontrol$date == week_dates)
  data_intake$food_provided_dw[i] = as.numeric(as.character((data_intake$food_provided_ww[i]))) *
    (1 - mean(data_foodcontrol$food_water_content[week_indexes]))
}

##### Food provided complete period #####

data_intake$food_provided_collection_days = data_intake$food_provided_dw *
  data_intake$number_collection_days


##### Food consumed complete period #####

data_intake$food_mass = data_intake$filled_tube_food_mass - data_intake$empty_tube_food_mass
data_intake$food_consumed_collection_days = NA
for (i in 1:nrow(data_intake)) {
  if (is.na(data_intake$food_mass[i]) == T) {
    data_intake$food_consumed_collection_days[i] = data_intake$food_provided_collection_days[i]
  }
  else {
    data_intake$food_consumed_collection_days[i] = data_intake$food_provided_collection_days[i] -
      data_intake$food_mass[i]
  }
}

data_intake$food_consumed_collection_days_unit = "md dw"

##### Ingestion rate #####

data_intake$ingestion_rate = data_intake$food_consumed_collection_days /
  data_intake$number_collection_days
data_intake$ingestion_rate_unit = "mg dw / day"

##### Egested mass consumed complete period #####

data_intake$egestion_mass = data_intake$filled_tube_egestion_mass - data_intake$empty_tube_egestion_mass

##### Egestion - ingestion ratio #####

data_intake$egestion_ingestion_ratio = data_intake$egestion_mass / data_intake$food_consumed_collection_days
##########  2. Check for biases  ##########

# Effect of week on bodymass at the start

boxplot(data = data_intake, bodymass_7th_instar_j0_ww ~ seventh_instar_date)
summary(aov(data = data_intake, bodymass_7th_instar_j0_ww ~ seventh_instar_date)) # There is an effect of week on bodymass at the start

# Effect of intake on sex
list_food_intakes = unique(data_intake$food_provided_ww)

data_sex = data.frame(food_provided_ww = list_food_intakes,
                      male_prop = NA)

for (i in 1:nrow(data_sex)) {
  rows_intake = which(data_intake$food_provided_ww == data_sex$food_provided_ww[i])
  data_sex$male_prop[i] = length(which(data_intake$sex[rows_intake] == "M")) /
    length(which(is.na(data_intake[rows_intake, "sex"]) == F))
}



##########  2. Statistics  ##########
unique(data_intake$seventh_instar_date)
##### Growth  #####

data_growth_long  = pivot_longer(
  data = data_intake[, c(
    "individual_ID",
    "food_provided_ww",
    "bodymass_7th_instar_j0_ww",
    "bodymass_7th_instar_j1_ww" ,
    "bodymass_7th_instar_j2_ww",
    "bodymass_7th_instar_j3_ww",
    "bodymass_7th_instar_j16_ww"
  )],
  cols = c(
    "bodymass_7th_instar_j0_ww",
    "bodymass_7th_instar_j1_ww" ,
    "bodymass_7th_instar_j2_ww",
    "bodymass_7th_instar_j3_ww",
    "bodymass_7th_instar_j16_ww"
  ),
  names_to = "day",
  values_to = "bodymass_mg",
  values_drop_na = TRUE
)



# We create a dataframe summarizing the growth data for each treatment

list_days = unique(data_growth_long$day)
list_food_intakes = unique(data_growth_long$food_provided_ww)
nb_rows_pop_growth = length(list_days) * length(list_food_intakes)

day_col = rep(list_days, each = length(list_food_intakes))
food_intake_col = rep(list_food_intakes, length(list_days))
average_bodymass = rep(NA, nb_rows_pop_growth)
sd_bodymass = rep(NA, nb_rows_pop_growth)
N = rep(NA, nb_rows_pop_growth)
data_growth_summary = data.frame(day = day_col,
                                 food_provided_ww = food_intake_col,
                                 average_bodymass,
                                 sd_bodymass,
                                 N)

# We compute the average and standard deviation of bodymass for each day
for (i in 1:nrow(data_growth_summary)) {
  rows_food_intake = which(data_growth_long$food_provided_ww  == data_growth_summary$food_provided_ww[i])
  rows_day = which(data_growth_long$day == data_growth_summary$day[i])
  rows_food_intake_day = intersect(rows_food_intake, rows_day)
  data_growth_summary$average_bodymass[i] = mean(data_growth_long$bodymass_mg[rows_food_intake_day], na.rm =
                                                   T)
  data_growth_summary$sd_bodymass[i] = sd(data_growth_long$bodymass_mg[rows_food_intake_day])
  data_growth_summary$N[i] = length(data_growth_long$bodymass_mg[rows_food_intake_day]) -
    length(which(is.na(data_growth_long$bodymass_mg[rows_food_intake_day])))
}

data_growth_summary$day = as.factor(rep(c(1:4, 16), each = 5))
##### Food conversion efficiency  #####


# We compute the food conversion efficiency for the 7th instar period without the prepupation

data_intake$growth_efficiency = NA


for (i in 1:nrow(data_intake)) {
  seventh_instar_date = data_intake$seventh_instar_date[i]
  week_dates = c(
    seventh_instar_date,
    seventh_instar_date + 24 * 60 * 60,
    seventh_instar_date + 2 * 24 * 60 * 60
  )
  week_indexes = which(data_foodcontrol$date == week_dates)
  data_intake$growth_efficiency[i] = (
    data_intake$bodymass_before_last_collection_date[i] - data_intake$bodymass_7th_instar_j0_ww[i]
  ) / (data_intake$food_consumed_collection_days[i] / (1 - mean(data_foodcontrol$food_water_content[week_indexes]))) # It is in fresh weight of food
}


##########  3. Graphics and figures  ##########

###### Individual growth curve #####
data_intake$food_provided_ww = as.factor(data_intake$food_provided_ww)
ggparcoord(
  data_intake,
  columns = c(
    "bodymass_7th_instar_j0_ww",
    "bodymass_7th_instar_j1_ww",
    "bodymass_7th_instar_j2_ww",
    "bodymass_7th_instar_j3_ww",
    "bodymass_7th_instar_j16_ww"
  ),
  scale = "globalminmax",
  groupColumn = "food_provided_ww",
  showPoints = TRUE,
  title = "Growth curve of S. littoralis larvae fed different quantities",
  alphaLines = 0.3,
  missing = "exclude",
  order = 7:10
) +
  scale_color_viridis(discrete = T, option = "H") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 10)) +
  ylab("Bodymass (mg)")

###### Treatment growth curve ######
data_growth_summary$food_provided_ww = as.factor(data_growth_summary$food_provided_ww)
p = ggplot(
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

print(p)
# Finished line plot
p + labs(title = "Growth curve of S. littoralis according to provided food mass", x = "Day of 7th instar", y = "Bodymass (mg)") +
  theme_classic() +
  scale_x_discrete(breaks = unique(data_growth_summary$day),
                   labels = as.character(c(1:4, 17))) +
  scale_color_npg()

###### Growth efficiency ######

# Growth efficiency according to the food provided daily

plt <- ggbetweenstats(
  data = data_intake,
  x = food_provided_ww,
  y = growth_efficiency,
  xlab = "Food provided daily (mg ww)",
  ylab = "Growth efficiency (mg ww body / mg ww food)",
  plot.type = "box",
  type = "p",
  conf.level = 0.95,
  package = "ggsci",
  palette = "nrc_npg",
  centrality.plotting = T,
  centrality.point.args = NULL,
  centrality.label.args = NULL,
)

plt

# Growth efficiency according to the ingestion rate

ggplot(data_intake,
       aes(x = ingestion_rate, y = growth_efficiency, color = food_provided_ww)) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Growth efficiency of S. littoralis according to ingestion rate", x = "Ingestion rate (mg dw/day)", y = "Growth efficiency (mg ww body / mg dw food)")

# Growth efficiency according to the mass specific ingestion rate
ggplot(data_intake,
       aes(x = ingestion_rate / ((
         bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
       ) / 2
       ), y = growth_efficiency, color = food_provided_ww)) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Growth efficiency of S. littoralis according to mass-specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg ww indiv)", y = "Growth efficiency (mg ww body / mg dw food)")

###### Functional response ######

ggplot(data_intake,
       aes(x = food_provided_ww, y = ingestion_rate, color = food_provided_ww)) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Functionnal response", x = "Food provided (mg ww /day)", y = "Food consumed (mg dw/day)")

###### Ingestion rate and individual mass ######

ggplot(data_intake,
       aes(
         x = (
           bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
         ) / 2,
         y = ingestion_rate,
         color = food_provided_ww
       )) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Ingestion rate as a function of average individual bodymass", x = "Avergae individual bodymass (mg ww)", y = "Ingestion rate (mg dw/day)")

###### Egestion ingestion ratio ######


ggplot(
  data_intake,
  aes(x = ingestion_rate, y = egestion_ingestion_ratio, color = food_provided_ww)
) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Egestion / ingestion ratio in S. littoralis according to ingestion rate", x = "Ingestion rate (mg dw/day)", y = "Egestion / ingestion ratio (mg dw / mg dw)")


###### Mass specific egestion ingestion ratio according to ingestion rate ######

ggplot(
  data_intake,
  aes(x = ingestion_rate, y = egestion_ingestion_ratio / ((
    bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
  ) / 2
  ), color = food_provided_ww)
) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Mass specific egestion / ingestion ratio according to ingestion rate", x = "Ingestion rate (mg dw/day)", y = "Mass specific egestion / ingestion ratio (mg dw / mg dw mg indiv ww)")



###### Egestion ingestion ratio according to mass specific ingestion rate ######

ggplot(
  data_intake,
  aes(x = ingestion_rate / ((
    bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
  ) / 2
  ), y = egestion_ingestion_ratio , color = food_provided_ww)
) +
  geom_point(size = 3) +
  theme_ipsum() + scale_color_npg() +
  labs(title = "Egestion / ingestion ratio according to mass specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Egestion / ingestion ratio (mg dw / mg dw)")
