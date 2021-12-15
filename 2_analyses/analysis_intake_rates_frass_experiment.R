################################ INTAKE RATE AND EGESTION QUALITY in Spodoptera littoralis  ################################

# This script analyses mass balance and chemical composition data and computes interesting intake, digestion, growth and egestion metrics.


# Load the control data

data_foodcontrol <-
  readxl::read_xlsx(here::here("1_data", "data_irn_food_controls.xlsx"))





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

data_growth_long  = tidyr::pivot_longer(
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
  data_growth_summary$sd_bodymass[i] = stats::sd(data_growth_long$bodymass_mg[rows_food_intake_day])
  data_growth_summary$N[i] = length(data_growth_long$bodymass_mg[rows_food_intake_day]) -
    length(which(is.na(data_growth_long$bodymass_mg[rows_food_intake_day])))
}

data_growth_summary$day = as.factor(rep(c(1:4, 16), each = 5))

# Dry weight bodymass at the end of the experiment

boxplot(data = data_intake, bodymass_7th_instar_j3_dw ~ food_provided_ww)
boxplot(data = data_intake, bodymass_imago_dw ~ food_provided_ww)


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
         bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
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
    bodymass_before_last_collection_date + bodymass_7th_instar_j0_ww
  ) / 2
  ), y = egestion_ingestion_ratio)
) +
  geom_point(size = 3) +
  hrbrthemes::theme_ipsum() + ggsci::scale_color_npg() +
  labs(title = "Egestion / ingestion ratio according to mass specific ingestion rate", x = "Mass specific ingestion rate (mg dw/day / mg indiv)", y = "Egestion / ingestion ratio (mg dw / mg dw)") +
  geom_smooth(color="steelblue3")

