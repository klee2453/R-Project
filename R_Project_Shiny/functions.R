setwd("/Users/kailalee/Downloads/R Project")

# Load individual datasets
load_patient_params_athero <- function() {
  return(read.csv("DietHealthDataset/Patient Parameters/Anonymized_Patient_Parameters_Atherosclerosis.csv"))
}

load_patient_params_htn <- function() {
  return(read.csv("DietHealthDataset/Patient Parameters/Anonymized_Patient_Parameters_Hypertension.csv"))
}

load_patient_params_diab <- function() {
  return(read.csv("DietHealthDataset/Patient Parameters/Anonymized_Patient_Parameters_Type_2_Diabetes.csv"))
}

load_nutritional_values_athero <- function() {
  return(read.csv("DietHealthDataset/Nutritional values/Nutritional_Values_Applied_Diet_Atherosclerosis.csv"))
}

load_nutritional_values_htn <- function() {
  return(read.csv("DietHealthDataset/Nutritional values/Nutritional_Values_Applied_Diet_Hypertension.csv"))
}

load_nutritional_values_diab <- function() {
  return(read.csv("DietHealthDataset/Nutritional values/Nutritional_Values_Applied_Diet_Type_2_Diabetes.csv"))
}

load_test_results_athero <- function() {
  return(read.csv("DietHealthDataset/Test Results/Anonymized_Test_Results_Atherosclerosis.csv"))
}

load_test_results_htn <- function() {
  return(read.csv("DietHealthDataset/Test Results/Anonymized_Test_Results_Hypertension.csv"))
}

load_test_results_diab <- function() {
  return(read.csv("DietHealthDataset/Test Results/Anonymized_Test_Results_Type_2_Diabetes.csv"))
}
test_results_athero <- read.csv("Test Results/Anonymized_Test_Results_Atherosclerosis.csv")
test_results_htn <- read.csv("Test Results/Anonymized_Test_Results_Hypertension.csv")
test_results_diab <- read.csv("Test Results/Anonymized_Test_Results_Type_2_Diabetes.csv")

# Combine datasets
load_combined_patient_params <- function() {
  patient_params_athero <- load_patient_params_athero()
  patient_params_htn <- load_patient_params_htn()
  patient_params_diab <- load_patient_params_diab()
  
  return(rbind(patient_params_athero, patient_params_htn, patient_params_diab))
}

load_combined_nutritional_values <- function() {
  nutritional_values_athero <- load_nutritional_values_athero()
  nutritional_values_htn <- load_nutritional_values_htn()
  nutritional_values_diab <- load_nutritional_values_diab()
  
  return(rbind(nutritional_values_athero, nutritional_values_htn, nutritional_values_diab))
}

# Function to prepare combined data
prepare_combined_data <- function() {
  combined_data <- load_combined_patient_params() %>%
    left_join(load_combined_nutritional_values(), by = "No") %>%
    slice(-c(1:3)) %>%
    mutate(
      Height = as.numeric(as.character(Height)),
      Weight = as.numeric(as.character(Weight)),
      BMI = Weight / ((Height / 100) ^ 2)
    ) %>%
    select(-`Physical.Activity`, -`Calories.Summary`) %>%
    mutate(
      BMI_category = cut(BMI, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
                         labels = c("Underweight", "Normal weight", "Overweight", "Obese")),
      Age = as.numeric(Age),
      AgeGroup = cut(Age, breaks = c(0, 18, 35, 50, Inf), 
                     labels = c("18-24", "25-34", "35-49", "50+"))
    ) %>%
    filter(!is.na(BMI_category))
  
  return(combined_data)
}

# Function for nutrient processing and correlation calculation
calculate_correlations <- function(combined_data) {
  combined_data <- combined_data %>%
    mutate(across(starts_with("Protein"):starts_with("Dietary_Fiber"), 
                  ~ as.numeric(str_replace_all(.x, "%", "")),
                  .names = "{col}_percent")) %>%
    group_by(BMI_category) %>%
    summarise(
      count_Type_of_Diet = n(),
      across(ends_with("_percent"), mean, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(combined_data)
}

# Function to prepare long format data for Shiny
prepare_long_diet_data <- function(combined_data) {
  long_diet <- combined_data %>%
    select(BMI_category, 
           Number.of.Meals.per.Day, 
           Protein_percent, 
           Fat_percent, 
           Carbohydrates_percent, 
           Iron_mg, 
           Sodium_mg, 
           Calcium_mg, 
           Phosphorus_mg, 
           Potassium_mg, 
           Magnesium_mg, 
           Zinc_mg, 
           Iodine_µg, 
           Selenium_µg, 
           Copper_mg, 
           Manganese_mg, 
           Omega_3, 
           Vitamin_A_µg, 
           Vitamin_D_µg, 
           Vitamin_E_mg, 
           Vitamin_B1_mg, 
           Vitamin_B2_mg, 
           Vitamin_B3_mg, 
           Vitamin_B5_mg, 
           Vitamin_B6_mg, 
           Vitamin_B7_µg, 
           Vitamin_B12_µg, 
           Vitamin_C_mg, 
           Vitamin_K_µg, 
           Omega_6_g, 
           Cholesterol_mg_day, 
           Dietary_Fiber_g) %>%
    pivot_longer(cols = everything(), names_to = "Nutrient", values_to = "Value")
  
  return(long_diet)
}

# Categorize Cholesterol Levels
cholesterol <- test_results_athero %>%
  mutate(Cholesterol_Category = case_when(
    as.numeric(Cholesterol.LDL) < 100 ~ "Normal LDL",
    as.numeric(Cholesterol.LDL) >= 100 & as.numeric(Cholesterol.LDL) <= 129 ~ "Near-Optimal LDL",
    as.numeric(Cholesterol.LDL) >= 130 & as.numeric(Cholesterol.LDL) <= 159 ~ "Borderline High LDL",
    as.numeric(Cholesterol.LDL) >= 160 & as.numeric(Cholesterol.LDL) <= 189 ~ "High LDL",
    as.numeric(Cholesterol.LDL) >= 190 ~ "Very High LDL",
    TRUE ~ "Unknown"
  ))

# Join with nutritional values
atherosclerosis <- left_join(cholesterol, nutritional_values_athero, by = "No")

# Convert nutritional values to numeric
atherosclerosis <- atherosclerosis %>%
  mutate(
    Protein_percent = as.numeric(str_replace_all(Protein...., "%", "")),
    Fat_percent = as.numeric(str_replace_all(Fat...., "%", "")),
    Carbohydrates_percent = as.numeric(str_replace_all(Carbohydrates...., "%", "")),
    Iron_mg = as.numeric(str_replace_all(str_replace_all(Iron..mg., "mg", ""), ",", ".")),
    Sodium_mg = as.numeric(str_replace_all(str_replace_all(Sodium..mg., "mg", ""), ",", ".")),
    Calcium_mg = as.numeric(str_replace_all(str_replace_all(Calcium..mg., "mg", ""), ",", ".")),
    Phosphorus_mg = as.numeric(str_replace_all(str_replace_all(Phosphorus..mg., "mg", ""), ",", ".")),
    Potassium_mg = as.numeric(str_replace_all(str_replace_all(Potassium..mg., "mg", ""), ",", ".")),
    Magnesium_mg = as.numeric(str_replace_all(str_replace_all(Magnesium..mg., "mg", ""), ",", ".")),
    Zinc_mg = as.numeric(str_replace_all(str_replace_all(Zinc..mg., "mg", ""), ",", ".")),
    Iodine_µg = as.numeric(str_replace_all(str_replace_all(Iodine..µg., "µg", ""), ",", ".")),
    Selenium_µg = as.numeric(str_replace_all(str_replace_all(Selenium..µg., "µg", ""), ",", ".")),
    Copper_mg = as.numeric(str_replace_all(str_replace_all(Copper..mg., "mg", ""), ",", ".")),
    Manganese_mg = as.numeric(str_replace_all(str_replace_all(Manganese..mg., "mg", ""), ",", ".")),
    Omega_3 = as.numeric(str_replace_all(str_replace_all(omega.3, "g", ""), ",", ".")),
    Vitamin_A_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.A..µg., "µg", ""), ",", ".")),
    Vitamin_D_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.D..µg., "µg", ""), ",", ".")),
    Vitamin_E_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.E..mg., "mg", ""), ",", ".")),
    Vitamin_B1_mg = as.numeric(str_replace_all(str_replace_all(wit..B1...Tiamina, "mg", ""), ",", ".")),
    Vitamin_B2_mg = as.numeric(str_replace_all(str_replace_all(wit..B2...Riboflavin, "mg", ""), ",", ".")),
    Vitamin_B3_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B3..mg., "mg", ""), ",", ".")),
    Vitamin_B5_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B5..mg., "mg", ""), ",", ".")),
    Vitamin_B6_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B6..mg., "mg", ""), ",", ".")),
    Vitamin_B7_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B7..µg., "µg", ""), ",", ".")),
    Vitamin_B12_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B12..µg., "µg", ""), ",", ".")),
    Vitamin_C_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.C..mg., "mg", ""), ",", ".")),
    Vitamin_K_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.K..µg., "µg", ""), ",", ".")),
    Omega_6_g = as.numeric(str_replace_all(str_replace_all(Omega.6..g., "g", ""), ",", ".")),
    Cholesterol_mg_day = as.numeric(str_replace_all(str_replace_all(Cholesterol..mg.day., "mg/day", ""), ",", ".")),
    Dietary_Fiber_g = as.numeric(str_replace_all(str_replace_all(Dietary.Fiber..g., "g", ""), ",", "."))
  )


# Long format for nutrients
# selecting only the numeric columns for pivoting
nutrient_cols <- c("Protein_percent", "Fat_percent", "Carbohydrates_percent", 
                   "Iron_mg", "Sodium_mg", "Calcium_mg", "Phosphorus_mg", 
                   "Potassium_mg", "Magnesium_mg", "Zinc_mg", "Iodine_µg", 
                   "Selenium_µg", "Copper_mg", "Manganese_mg", "Omega_3", 
                   "Vitamin_A_µg", "Vitamin_D_µg", "Vitamin_E_mg", 
                   "Vitamin_B1_mg", "Vitamin_B2_mg", "Vitamin_B3_mg", 
                   "Vitamin_B5_mg", "Vitamin_B6_mg", "Vitamin_B7_µg", 
                   "Vitamin_B12_µg", "Vitamin_C_mg", "Vitamin_K_µg", 
                   "Omega_6_g", "Cholesterol_mg_day", "Dietary_Fiber_g")

# Now perform the selection and pivoting
long_athero <- atherosclerosis %>%
  select(Cholesterol_Category, Type.of.Diet, Number.of.Meals.per.Day, all_of(nutrient_cols)) %>%
  pivot_longer(cols = all_of(nutrient_cols), 
               names_to = "Nutrient", 
               values_to = "Value")


# Convert Value to numeric and filter out unknown categories
long_athero <- long_athero %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(Cholesterol_Category != "Unknown")

# Categorize Blood Pressure Levels
BP <- test_results_htn %>%
  mutate(
    Systolic.Pressure = as.numeric(Systolic.Pressure),
    Diastolic.Pressure = as.numeric(Diastolic.Pressure),
    BP_Category = case_when(
      Systolic.Pressure < 120 & Diastolic.Pressure < 80 ~ "Normal",
      Systolic.Pressure >= 120 & Systolic.Pressure <= 129 & Diastolic.Pressure < 80 ~ "Elevated",
      (Systolic.Pressure >= 130 & Systolic.Pressure <= 139) | (Diastolic.Pressure >= 80 & Diastolic.Pressure <= 89) ~ "Stage 1 Hypertension",
      Systolic.Pressure >= 140 | Diastolic.Pressure >= 90 ~ "Stage 2 Hypertension",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(BP_Category != "Unknown")

# Join with nutritional values
hypertension <- left_join(BP, nutritional_values_htn, by = "No")

# Convert nutritional values to numeric
hypertension <- hypertension %>%
  mutate(
    Protein_percent = as.numeric(str_replace_all(Protein...., "%", "")),
    Fat_percent = as.numeric(str_replace_all(Fat...., "%", "")),
    Carbohydrates_percent = as.numeric(str_replace_all(Carbohydrates...., "%", "")),
    Iron_mg = as.numeric(str_replace_all(str_replace_all(Iron..mg., "mg", ""), ",", ".")),
    Sodium_mg = as.numeric(str_replace_all(str_replace_all(Sodium..mg., "mg", ""), ",", ".")),
    Calcium_mg = as.numeric(str_replace_all(str_replace_all(Calcium..mg., "mg", ""), ",", ".")),
    Phosphorus_mg = as.numeric(str_replace_all(str_replace_all(Phosphorus..mg., "mg", ""), ",", ".")),
    Potassium_mg = as.numeric(str_replace_all(str_replace_all(Potassium..mg., "mg", ""), ",", ".")),
    Magnesium_mg = as.numeric(str_replace_all(str_replace_all(Magnesium..mg., "mg", ""), ",", ".")),
    Zinc_mg = as.numeric(str_replace_all(str_replace_all(Zinc..mg., "mg", ""), ",", ".")),
    Iodine_µg = as.numeric(str_replace_all(str_replace_all(Iodine..µg., "mg", ""), ",", ".")),
    Selenium_µg = as.numeric(str_replace_all(str_replace_all(Selenium..µg., "mg", ""), ",", ".")),
    Copper_mg = as.numeric(str_replace_all(str_replace_all(Copper..mg., "mg", ""), ",", ".")),
    Manganese_mg = as.numeric(str_replace_all(str_replace_all(Manganese..mg., "mg", ""), ",", ".")),
    Omega_3 = as.numeric(str_replace_all(str_replace_all(omega.3, "g", ""), ",", ".")),
    Vitamin_A_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.A..µg., "µg", ""), ",", ".")),
    Vitamin_D_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.D..µg., "µg", ""), ",", ".")),
    Vitamin_E_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.E..mg., "mg", ""), ",", ".")),
    Vitamin_B1_mg = as.numeric(str_replace_all(str_replace_all(wit..B1...Tiamina, "mg", ""), ",", ".")),
    Vitamin_B2_mg = as.numeric(str_replace_all(str_replace_all(wit..B2...Riboflavin, "mg", ""), ",", ".")),
    Vitamin_B3_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B3..mg., "mg", ""), ",", ".")),
    Vitamin_B5_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B5..mg., "mg", ""), ",", ".")),
    Vitamin_B6_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B6..mg., "mg", ""), ",", ".")),
    Vitamin_B7_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B7..µg., "µg", ""), ",", ".")),
    Vitamin_B12_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B12..µg., "µg", ""), ",", ".")),
    Vitamin_C_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.C..mg., "mg", ""), ",", ".")),
    Vitamin_K_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.K..µg., "µg", ""), ",", ".")),
    Omega_6_g = as.numeric(str_replace_all(str_replace_all(Omega.6..g., "g", ""), ",", ".")),
    Cholesterol_mg_day = as.numeric(str_replace_all(str_replace_all(Cholesterol..mg.day., "mg/day", ""), ",", ".")),
    Dietary_Fiber_g = as.numeric(str_replace_all(str_replace_all(Dietary.Fiber..g., "g", ""), ",", "."))
  )

# Long format for nutrients
long_htn <- hypertension %>%
  select(BP_Category, Type.of.Diet, Number.of.Meals.per.Day, everything()) %>%
  pivot_longer(cols = starts_with("Protein_") | starts_with("Fat_") | starts_with("Carbohydrates_") | starts_with("Iron_") | starts_with("Sodium_") | starts_with("Calcium_") | starts_with("Phosphorus_") | starts_with("Potassium_") | starts_with("Magnesium_") | starts_with("Zinc_") | starts_with("Iodine_") | starts_with("Selenium_") | starts_with("Copper_") | starts_with("Manganese_") | starts_with("Omega_") | starts_with("Vitamin_"),
               names_to = "Nutrient",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value))

diabetes <- left_join(test_results_diab, nutritional_values_diab, by = "No")

diabetes <- diabetes[-1, ]

diabetes <- diabetes %>%
  mutate(
    Protein_percent = as.numeric(str_replace_all(Protein...., "%", "")),
    Fat_percent = as.numeric(str_replace_all(Fat...., "%", "")),
    Carbohydrates_percent = as.numeric(str_replace_all(Carbohydrates...., "%", "")),
    Iron_mg = as.numeric(str_replace_all(str_replace_all(Iron..mg., "mg", ""), ",", ".")),
    Sodium_mg = as.numeric(str_replace_all(str_replace_all(Sodium..mg., "mg", ""), ",", ".")),
    Calcium_mg = as.numeric(str_replace_all(str_replace_all(Calcium..mg., "mg", ""), ",", ".")),
    Phosphorus_mg = as.numeric(str_replace_all(str_replace_all(Phosphorus..mg., "mg", ""), ",", ".")),
    Potassium_mg = as.numeric(str_replace_all(str_replace_all(Potassium..mg., "mg", ""), ",", ".")),
    Magnesium_mg = as.numeric(str_replace_all(str_replace_all(Magnesium..mg., "mg", ""), ",", ".")),
    Zinc_mg = as.numeric(str_replace_all(str_replace_all(Zinc..mg., "mg", ""), ",", ".")),
    Iodine_µg = as.numeric(str_replace_all(str_replace_all(Iodine..µg., "mg", ""), ",", ".")),
    Selenium_µg = as.numeric(str_replace_all(str_replace_all(Selenium..µg., "mg", ""), ",", ".")),
    Copper_mg = as.numeric(str_replace_all(str_replace_all(Copper..mg., "mg", ""), ",", ".")),
    Manganese_mg = as.numeric(str_replace_all(str_replace_all(Manganese..mg., "mg", ""), ",", ".")),
    Omega_3 = as.numeric(str_replace_all(str_replace_all(omega.3, "g", ""), ",", ".")),
    Vitamin_A_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.A..µg., "µg", ""), ",", ".")),
    Vitamin_D_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.D..µg., "µg", ""), ",", ".")),
    Vitamin_E_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.E..mg., "mg", ""), ",", ".")),
    Vitamin_B1_mg = as.numeric(str_replace_all(str_replace_all(wit..B1...Tiamina, "mg", ""), ",", ".")),
    Vitamin_B2_mg = as.numeric(str_replace_all(str_replace_all(wit..B2...Riboflavin, "mg", ""), ",", ".")),
    Vitamin_B3_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B3..mg., "mg", ""), ",", ".")),
    Vitamin_B5_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B5..mg., "mg", ""), ",", ".")),
    Vitamin_B6_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.B6..mg., "mg", ""), ",", ".")),
    Vitamin_B7_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B7..µg., "µg", ""), ",", ".")),
    Vitamin_B12_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.B12..µg., "µg", ""), ",", ".")),
    Vitamin_C_mg = as.numeric(str_replace_all(str_replace_all(Vitamin.C..mg., "mg", ""), ",", ".")),
    Vitamin_K_µg = as.numeric(str_replace_all(str_replace_all(Vitamin.K..µg., "µg", ""), ",", ".")),
    Omega_6_g = as.numeric(str_replace_all(str_replace_all(Omega.6..g., "g", ""), ",", ".")),
    Cholesterol_mg_day = as.numeric(str_replace_all(str_replace_all(Cholesterol..mg.day., "mg/day", ""), ",", ".")),
    Dietary_Fiber_g = as.numeric(str_replace_all(str_replace_all(Dietary.Fiber..g., "g", ""), ",", ".")),
    Fasting.Glucose.Concentration = as.numeric(Fasting.Glucose.Concentration)
  )

long_diab <- diabetes %>%
  select(Fasting.Glucose.Concentration, Type.of.Diet, Number.of.Meals.per.Day, 
         starts_with("Protein"), starts_with("Fat"), starts_with("Carbohydrates")) %>%
  mutate(across(-c(Fasting.Glucose.Concentration, Type.of.Diet, Number.of.Meals.per.Day), 
                ~ as.numeric(as.character(.)))) %>%
  pivot_longer(-c(Fasting.Glucose.Concentration, Type.of.Diet, Number.of.Meals.per.Day),
               names_to = "Nutrient",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) # Ensure Value is numeric after pivoting


long_diab_clean <- long_diab %>%
  group_by(Fasting.Glucose.Concentration, Nutrient) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop')

wide_diab <- long_diab_clean %>%
  pivot_wider(names_from = Nutrient, values_from = Value)

wide_diab_numeric <- wide_diab %>%
  mutate(across(-Fasting.Glucose.Concentration, as.numeric)) %>%
  drop_na()

long_diab_numeric <- wide_diab_numeric %>%
  pivot_longer(-Fasting.Glucose.Concentration, names_to = "Nutrient", values_to = "Value")

n <- 30
color_palette <- colorRampPalette(brewer.pal(8, "Set3"))(n)

# Check if long_diab is a dataframe before performing mutate
if (is.data.frame(long_diab)) {
  long_diab <- long_diab %>%
    mutate(Glucose.Bin = cut(Fasting.Glucose.Concentration, 
                             breaks = seq(min(Fasting.Glucose.Concentration, na.rm = TRUE), 
                                          max(Fasting.Glucose.Concentration, na.rm = TRUE), 
                                          by = 5), 
                             include.lowest = TRUE, 
                             right = FALSE))
} else {
  warning("long_diab is not a dataframe. Current class: ", class(long_diab))
}

