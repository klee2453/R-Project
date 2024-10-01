#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)  # For interactive tables if needed
library(tidyr)
library(stringr)
library(RColorBrewer) 

# Define server logic
server <- function(input, output, session) {
  
  # Load the functions
  source("functions.R")
  
  # Load individual datasets
  patient_params_athero <- load_patient_params_athero()
  patient_params_htn <- load_patient_params_htn()
  patient_params_diab <- load_patient_params_diab()
  
  nutritional_values_athero <- load_nutritional_values_athero()
  nutritional_values_htn <- load_nutritional_values_htn()
  nutritional_values_diab <- load_nutritional_values_diab()
  
  test_results_athero <- load_test_results_athero()
  test_results_htn <- load_test_results_htn()
  test_results_diab <- load_test_results_diab()
  
  # Load combined datasets
  patient_params <- load_combined_patient_params()
  nutritional_values <- load_combined_nutritional_values()

  # Load and prepare the data
  combined_data <- prepare_combined_data()
  long_diet <- prepare_long_diet_data(combined_data)
  
  # Populate the nutrient choices
  nutrients <- unique(long_diet$Nutrient)
  updateSelectInput(session, "selected_nutrient", choices = nutrients)
  updateSelectInput(session, "selected_nutrient_athero", choices = nutrients)
  updateSelectInput(session, "selected_nutrient_htn", choices = nutrients)
  updateSelectInput(session, "selected_nutrient_diab", choices = nutrients)
  
  # Filter datasets for each chronic condition
  long_athero_filtered <- long_diet %>% filter(Disease == "Atherosclerosis")
  long_htn <- long_diet %>% filter(Disease == "Hypertension")
  long_diab_numeric <- long_diet %>% filter(Disease == "Type 2 Diabetes", !is.na(Fasting.Glucose.Concentration))
  long_diab <- long_diet %>% filter(Disease == "Type 2 Diabetes")
  
  # 1. Dietary Factors and BMI
  output$bmiPlot <- renderPlot({
    nutrient <- input$selected_nutrient
    data_subset <- long_diet %>% filter(Nutrient == nutrient)
    
    ggplot(data_subset, aes(x = BMI_category, y = Value, fill = BMI_category)) +
      geom_boxplot() +
      labs(title = paste("Dietary Factor:", nutrient),
           x = "BMI Category",
           y = "Value",
           fill = "BMI Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Heatmap of Mean BMI by Age Group, Gender, and Type of Diet
  output$dietTypeDist <- renderPlot({
    heatmap_data_long <- combined_data %>%
      group_by(AgeGroup, Gender, Type.of.Diet) %>%
      summarise(mean_BMI = mean(BMI, na.rm = TRUE), .groups = 'drop')
    
    ggplot(heatmap_data_long, aes(x = AgeGroup, y = Gender, fill = mean_BMI)) +
      geom_tile() +
      facet_wrap(~ Type.of.Diet) +
      labs(title = "Heatmap of Mean BMI by Age Group, Gender, and Type of Diet",
           x = "Age Group", y = "Gender", fill = "Mean BMI") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
  
  # 2. Atherosclerosis: Boxplots by Cholesterol Category
  output$atheroBoxPlot <- renderPlot({
    nutrient <- input$selected_nutrient_athero
    
    if (!is.null(nutrient)) {
      p <- ggplot(filter(long_athero, Nutrient == nutrient), aes(x = Cholesterol_Category, y = Value)) +
        geom_boxplot() +
        labs(title = paste("Dietary Factors by Cholesterol Category -", nutrient),
             x = "Cholesterol Category",
             y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_minimal() 
      
      print(p)
    } else {
      print("Please select a nutrient.")
    }
  })
  
  # Atherosclerosis: Distribution of Type of Diet by Cholesterol Category
  output$atheroDietDist <- renderPlot({
    ggplot(long_athero, aes(x = Cholesterol_Category, fill = Type.of.Diet)) +
      geom_bar(position = "dodge", stat = "count") +
      labs(title = "Distribution of Type of Diet by Cholesterol Category",
           x = "Cholesterol Category",
           y = "Count",
           fill = "Type of Diet") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  })
  
  # 3. Hypertension: Boxplots by Blood Pressure Category
  output$htnBoxPlot <- renderPlot({
    nutrient <- input$selected_nutrient_htn
    
    if (!is.null(nutrient) && nutrient %in% unique(long_htn$Nutrient)) {
      q <- ggplot(filter(long_htn, Nutrient == nutrient), aes(x = BP_Category, y = Value)) +
        geom_boxplot() +
        labs(title = paste("Dietary Factors by Blood Pressure Category -", nutrient),
             x = "Blood Pressure Category",
             y = "Value") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme_minimal() 
      
      print(q)
    } else {
      print("Please select a valid nutrient.")
    }
  })
  
  # Hypertension: Distribution of Type of Diet by Blood Pressure Category
  output$htnDietDist <- renderPlot({
    ggplot(long_htn, aes(x = BP_Category, fill = Type.of.Diet)) +
      geom_bar(position = "dodge", stat = "count") +
      labs(title = "Distribution of Type of Diet by Blood Pressure Category",
           x = "Blood Pressure Category",
           y = "Count",
           fill = "Type of Diet") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  })
  
  # 4. Type 2 Diabetes: Bubble Plot by Fasting Glucose Concentration
  output$diabBubblePlot <- renderPlot({
    ggplot(long_diab_numeric, aes(x = Fasting.Glucose.Concentration, y = Nutrient, size = Value, color = Nutrient)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(name = "Nutrient Value") +
      scale_color_manual(values = brewer.pal(n = length(unique(long_diab_numeric$Nutrient)), name = "Set3")) +
      labs(title = "Bubble Plot of Nutrients by Fasting Glucose Concentration",
           x = "Fasting Glucose Concentration",
           y = "Nutrient",
           color = "Nutrient") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Type 2 Diabetes: Distribution of Type of Diet by Fasting Glucose Concentrations
  output$diabDietDist <- renderPlot({
    ggplot(long_diab %>% filter(Type.of.Diet), aes(x = Glucose.Bin, fill = Type.of.Diet)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = brewer.pal(n = length(unique(long_diab$Type.of.Diet)), name = "Set3")) +
      labs(title = "Distribution of Type of Diet by Fasting Glucose Concentrations for Type 2 Diabetes",
           x = "Fasting Glucose Concentrations (binned)",
           y = "Count",
           fill = "Type of Diet") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  })
}


