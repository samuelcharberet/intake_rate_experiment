################################ NETSTO PROJECT ################################

# This script automatically create a sample code composed of several sample-specific ID
# It also creates a table to facilitate the creation of labels to put on the samples

library(readr)
library(stringr)
library(flextable)
library(tibble)
library(openxlsx)


data_intake = readr::read_csv(here::here("1_data", "data_irn_individuals.csv"))

######### 1. Box codes #########
# 440 boxes with
# Treatment : T1-T5
# Group : G1-G110
# Individual : I1-I440
# CA (chemical analysis) ou EP (emergence percentage)

# We add CA or EP in a dedicated column
data_intake$usage = NA

for (i in 1:nrow(data_intake)) {
  if (data_intake$body_analysis[i] == 0) {
    data_intake$usage[i] = "EP"
  }
  else {
    data_intake$usage[i] = "CA"
  }
}

# We create a box ID

data_intake$box_ID = NA

for (i in 1:nrow(data_intake)) {
  data_intake$box_ID[i] = paste(
    "T",
    data_intake$treatment_ID[i],
    "-G",
    data_intake$group_ID[i],
    "-I",
    data_intake$individual_ID[i],
    "-",
    data_intake$usage[i],
    sep = ""
  )
}


# Automatically write labels for boxes


box_labels = vector(length = nrow(data_intake))

for (i in 1:nrow(data_intake)) {
  box_labels[i] = paste("Date :  ",
                        "\n",
                        "Code : ",
                        data_intake$box_ID[i], sep = "")
}

box_labels = c(box_labels, rep(NA, length.out = (4 - length(box_labels) %% 4)))
box_labels_table = matrix(
  box_labels,
  ncol = 4,
  nrow = length(box_labels) / 4,
  byrow = T
)
box_labels_table = data.frame(box_labels_table)

for (i in c(1, 3, 5)) {
  box_labels_table = add_column(box_labels_table, X = "", .after = i)
}

write.xlsx(
  box_labels_table,
  here::here("0_appendix", "labeling", "box_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)
######### 2. Tubes codes #########

# Date
# Nature : egestion, food, larvae
# Traitement : T1-T5
# Groupe : G1-G110
# Individu(s) : I1-I440

#### 2.1 Individual egestion tubes codes ####
# 440 tubes

# We create an egestion tube ID
data_intake$individual_egestion_tube_ID = NA

for (i in 1:nrow(data_intake)) {
  data_intake$individual_egestion_tube_ID[i] = paste("E-", data_intake$box_ID[i], sep = "")
}

individual_egestion_tube_IDs = data.frame(data_intake$individual_egestion_tube_ID)

write.xlsx(
  individual_egestion_tube_IDs,
  here::here(
    "0_appendix",
    "labeling",
    "individual_egestion_tube_IDs.xlsx"
  ),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)


# Automatically write labels

individual_egestion_tube_labels = vector(length = nrow(data_intake))

for (i in 1:nrow(data_intake)) {
  individual_egestion_tube_labels[i] = paste("Date :  ",
                                             "\n",
                                             "Code : ",
                                             data_intake$individual_egestion_tube_ID[i],
                                             sep = "")
}

individual_egestion_tube_labels = c(individual_egestion_tube_labels, rep(NA, length.out = (
  4 - length(individual_egestion_tube_labels) %% 4
)))
individual_egestion_tube_labels_table = matrix(
  individual_egestion_tube_labels,
  ncol = 4,
  nrow = length(individual_egestion_tube_labels) / 4,
  byrow = T
)
individual_egestion_tube_labels_table = data.frame(individual_egestion_tube_labels_table)

for (i in c(1, 3, 5)) {
  individual_egestion_tube_labels_table = add_column(individual_egestion_tube_labels_table,
                                                     X = "",
                                                     .after = i)
}

write.xlsx(
  individual_egestion_tube_labels_table,
  here::here(
    "0_appendix",
    "labeling",
    "individual_egestion_tube_labels.xlsx"
  ),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

#### 2.2 Individual food tubes codes ####
# 440 tubes

# We create a food tube ID a box ID
data_intake$individual_food_tube_ID = NA

for (i in 1:nrow(data_intake)) {
  data_intake$individual_food_tube_ID[i] = paste("F-", data_intake$box_ID[i], sep = "")
}

individual_food_tube_IDs = data.frame(data_intake$individual_food_tube_ID)

write.xlsx(
  individual_food_tube_IDs,
  here::here("0_appendix", "labeling", "individual_food_tube_IDs.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

# Automatically write labels

individual_food_tube_labels = vector(length = nrow(data_intake))

for (i in 1:nrow(data_intake)) {
  individual_food_tube_labels[i] = paste("Date :  ",
                                         "\n",
                                         "Code : ",
                                         data_intake$individual_food_tube_ID[i],
                                         sep = "")
}

individual_food_tube_labels = c(individual_food_tube_labels, rep(NA, length.out = (
  4 - length(individual_food_tube_labels) %% 4
)))
individual_food_tube_labels_table = matrix(
  individual_food_tube_labels,
  ncol = 4,
  nrow = length(individual_food_tube_labels) / 4,
  byrow = T
)
individual_food_tube_labels_table = data.frame(individual_food_tube_labels_table)

for (i in c(1, 3, 5)) {
  individual_food_tube_labels_table = add_column(individual_food_tube_labels_table,
                                                 X = "",
                                                 .after = i)
}

write.xlsx(
  individual_food_tube_labels_table,
  here::here("0_appendix", "labeling", "individual_food_tube_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

#### 2.3 Larvae tubes codes ####
# 110 tubes

# We create a larvae tube ID
data_intake$larvae_tube_ID = NA

for (i in 1:nrow(data_intake)) {
  data_intake$larvae_tube_ID[i] = paste("L-",
                                        "T",
                                        data_intake$treatment_ID[i],
                                        "-G",
                                        data_intake$group_ID[i],
                                        sep = "")
}


# Automatically write labels
larvae_tube_IDs = unique(data_intake$larvae_tube_ID)
larvae_tube_labels = vector(length = length(larvae_tube_IDs))

for (i in 1:length(larvae_tube_IDs)) {
  larvae_tube_labels[i] = paste("Date :  ",
                                "\n",
                                "Code : ",
                                larvae_tube_IDs[i], sep = "")
}

larvae_tube_labels = c(larvae_tube_labels, rep(NA, length.out = (4 - length(larvae_tube_labels) %% 4)))
larvae_tube_labels_table = matrix(
  larvae_tube_labels,
  ncol = 4,
  nrow = length(larvae_tube_labels) / 4,
  byrow = T
)
larvae_tube_labels_table = data.frame(larvae_tube_labels_table)

for (i in c(1, 3, 5)) {
  larvae_tube_labels_table = add_column(larvae_tube_labels_table, X = "", .after = i)
}

larvae_tube_IDs = unique(data.frame(data_intake$larvae_tube_ID))

write.xlsx(
  larvae_tube_labels_table,
  here::here("0_appendix", "labeling", "larvae_tube_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

write.xlsx(
  larvae_tube_IDs,
  here::here("0_appendix", "labeling", "larvae_tube_IDs.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

#### 2.4 Group egestion tubes codes ####
# 110 tubes


# We create a group egestion tubes ID
data_intake$group_egestion_tubes_ID = NA

for (i in 1:nrow(data_intake)) {
  data_intake$group_egestion_tubes_ID[i] = paste("E-",
                                                 "T",
                                                 data_intake$treatment_ID[i],
                                                 "-G",
                                                 data_intake$group_ID[i],
                                                 sep = "")
}

group_egestion_tube_IDs = unique(data.frame(data_intake$group_egestion_tubes_ID))

write.xlsx(
  group_egestion_tube_IDs,
  here::here("0_appendix", "labeling", "group_egestion_tube_IDs.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)



# Automatically write labels
group_egestion_tubes_IDs = unique(data_intake$group_egestion_tubes_ID)
group_egestion_tubes_labels = vector(length = length(group_egestion_tubes_IDs))

for (i in 1:length(group_egestion_tubes_IDs)) {
  group_egestion_tubes_labels[i] = paste("Date :  ",
                                         "\n",
                                         "Code : ",
                                         group_egestion_tubes_IDs[i], sep = "")
}

group_egestion_tubes_labels = c(group_egestion_tubes_labels, rep(NA, length.out = (
  4 - length(group_egestion_tubes_labels) %% 4
)))
group_egestion_tubes_labels_table = matrix(
  group_egestion_tubes_labels,
  ncol = 4,
  nrow = length(group_egestion_tubes_labels) / 4,
  byrow = T
)
group_egestion_tubes_labels_table = data.frame(group_egestion_tubes_labels_table)

for (i in c(1, 3, 5)) {
  group_egestion_tubes_labels_table = add_column(group_egestion_tubes_labels_table,
                                                 X = "",
                                                 .after = i)
}

write.xlsx(
  group_egestion_tubes_labels_table,
  here::here("0_appendix", "labeling", "group_egestion_tubes_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)


######### 3. Food control codes #########

#### 3.1 Daily food control codes ####

# 33 tubes

days = 1:3
weeks = 1:11

daily_food_control_IDs = rep(NA, 30)
days_IDS = rep(days, 10)
weeks_IDS = rep(weeks, each = 3)

for (i in 1:length(daily_food_control_IDs)) {
  daily_food_control_IDs[i] = paste("FC-",
                                    "w",
                                    weeks_IDS[i],
                                    "-d",
                                    days_IDS[i],
                                    sep = "")
}

daily_food_control_IDs = data.frame(daily_food_control_IDs)


write.xlsx(
  daily_food_control_IDs,
  here::here("0_appendix", "labeling", "daily_food_control_IDs.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

# Automatically write labels

daily_food_control_labels = vector(length = nrow(daily_food_control_IDs))

for (i in 1:nrow(daily_food_control_IDs)) {
  daily_food_control_labels[i] = paste("Date :  ",
                                       "\n",
                                       "Code : ",
                                       daily_food_control_IDs[i, 1], sep = "")
}

daily_food_control_labels = c(daily_food_control_labels, rep(NA, length.out = (4 - length(
  daily_food_control_labels
) %% 4)))
daily_food_control_table = matrix(
  daily_food_control_labels,
  ncol = 4,
  nrow = length(daily_food_control_labels) / 4,
  byrow = T
)
daily_food_control_table = data.frame(daily_food_control_table)

for (i in c(1, 3, 5)) {
  daily_food_control_table = add_column(daily_food_control_table,
                                        X = "",
                                        .after = i)
}

write.xlsx(
  daily_food_control_table,
  here::here("0_appendix", "labeling", "daily_food_control_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)


#### 3.2. Weekly food control codes ####

# 11 tubes


weekly_food_control_IDs = rep(NA, length(weeks))

for (i in 1:length(weekly_food_control_IDs)) {
  weekly_food_control_IDs[i] = paste("FC-",
                                     "w",
                                     weeks[i],
                                     sep = "")
}

weekly_food_control_IDs = unique(data.frame(weekly_food_control_IDs))

write.xlsx(
  weekly_food_control_IDs,
  here::here("0_appendix", "labeling", "weekly_food_control_IDs.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)

# Automatically write labels

weekly_food_control_labels = vector(length = nrow(weekly_food_control_IDs))

for (i in 1:nrow(weekly_food_control_IDs)) {
  weekly_food_control_labels[i] = paste("Date :  ",
                                        "\n",
                                        "Code : ",
                                        weekly_food_control_IDs[i, 1], sep = "")
}

weekly_food_control_labels = c(weekly_food_control_labels, rep(NA, length.out = (
  4 - length(weekly_food_control_labels) %% 4
)))
weekly_food_control_table = matrix(
  weekly_food_control_labels,
  ncol = 4,
  nrow = length(weekly_food_control_labels) / 4,
  byrow = T
)
weekly_food_control_table = data.frame(weekly_food_control_table)

for (i in c(1, 3, 5)) {
  weekly_food_control_table = add_column(weekly_food_control_table,
                                         X = "",
                                         .after = i)
}

write.xlsx(
  weekly_food_control_table,
  here::here("0_appendix", "labeling", "weekly_food_control_labels.xlsx"),
  sheetName = "Sheet1",
  colNames = F,
  rowNames = F,
  append = FALSE,
  overwrite = T
)
