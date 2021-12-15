################################ INTAKE RATE AND EGESTION QUALITY in Spodoptera littoralis  ################################

# This script analyses mass balance and chemical composition data and computes interesting intake, digestion, growth and egestion metrics.


# Load the control data

data_foodcontrol <-
  readxl::read_xlsx(here::here("1_data", "data_irn_food_controls.xlsx"))

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



