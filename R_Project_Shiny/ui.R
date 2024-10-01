#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(RColorBrewer)  # For color palettes

# Define UI
ui <- fluidPage(
  titlePanel("Dietary Interventions and Health Outcomes by Kaila Lee"),
  
  tags$head(
    tags$style(HTML("
      .container-fluid {
        background-color: #f5f5f5;
      }
      .navbar { 
        background-color: #007bff; 
      }
      .navbar-nav > li > a {
        color: #ffffff;
      }
      .navbar-brand {
        color: #ffffff;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_nutrient", 
                  "Select Nutrient for BMI Plots:", 
                  choices = NULL),  # Choices will be populated in server
      selectInput("selected_nutrient_athero", 
                  "Select Nutrient for Atherosclerosis:", 
                  choices = NULL),  # Choices will be populated in server
      selectInput("selected_nutrient_htn", 
                  "Select Nutrient for Hypertension:", 
                  choices = NULL),  # Choices will be populated in server
      selectInput("selected_nutrient_diab", 
                  "Select Nutrient for Type 2 Diabetes:", 
                  choices = NULL)   # Choices will be populated in server
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", 
                 h3("Dataset Overview"), 
                 p("The original dataset used in this project was sourced from Kaggle. The data was a Comma Separated Values (CSV) file. This dataset explores the relationship between diet and health outcomes, specifically focusing on three chronic conditions: Atherosclerosis, Hypertension, and Type 2 Diabetes.")
        ),
        tabPanel("Dataset Contents", 
                 h3("Patient Parameters"),
                 p("Anonymized_Patient_Parameters_Atherosclerosis.csv: Contains anonymized demographic and physical parameters of patients with Atherosclerosis."),
                 p("Anonymized_Patient_Parameters_Hypertension.csv: Contains anonymized demographic and physical parameters of patients with Hypertension."),
                 p("Anonymized_Patient_Parameters_Type_2_Diabetes.csv: Contains anonymized demographic and physical parameters of patients with Type 2 Diabetes."),
                 h3("Test Results"),
                 p("Anonymized_Test_Results_Atherosclerosis.csv: Contains anonymized medical test results for patients with Atherosclerosis."),
                 p("Anonymized_Test_Results_Hypertension.csv: Contains anonymized medical test results for patients with Hypertension."),
                 p("Anonymized_Test_Results_Type_2_Diabetes.csv: Contains anonymized medical test results for patients with Type 2 Diabetes."),
                 h3("Nutritional Values"),
                 p("Nutritional_Values_Applied_Diet_Atherosclerosis.csv: Provides the nutritional values of the diets applied to patients with Atherosclerosis."),
                 p("Nutritional_Values_Applied_Diet_Hypertension.csv: Provides the nutritional values of the diets applied to patients with Hypertension."),
                 p("Nutritional_Values_Applied_Diet_Type_2_Diabetes.csv: Provides the nutritional values of the diets applied to patients with Type 2 Diabetes.")
        ),
        tabPanel("Background", 
                 h3("1. Body Mass Index (BMI)"),
                 tags$ul(
                   tags$li("Caloric Intake: High caloric intake from diets rich in sugars, fats, and processed foods is strongly correlated with higher BMI and obesity. Conversely, diets lower in calories but rich in nutrients (such as whole grains, vegetables, and fruits) are associated with lower BMI."),
                   tags$li("Macronutrient Composition: Diets high in refined carbohydrates and saturated fats contribute to increased BMI. Diets high in protein and fiber, especially from whole foods, are linked to better weight management."),
                   tags$li("Meal Frequency and Portion Control: Frequent small meals or controlled portion sizes can help in managing BMI by avoiding overeating.")
                 ),
                 h3("2. Atherosclerosis"),
                 tags$ul(
                   tags$li("Saturated and Trans Fats: Diets high in saturated fats (found in red meat and full-fat dairy) and trans fats (in processed foods) are linked to increased levels of LDL cholesterol and inflammation, which contribute to atherosclerosis."),
                   tags$li("Omega-3 Fatty Acids: Consuming omega-3 fatty acids (found in fatty fish, flaxseeds, and walnuts) can help reduce inflammation and improve lipid profiles, potentially reducing the risk of atherosclerosis."),
                   tags$li("Antioxidants: Diets rich in fruits, vegetables, and whole grains, which provide antioxidants like vitamins C and E, can protect against oxidative stress and endothelial damage involved in atherosclerosis.")
                 ),
                 h3("3. Hypertension"),
                 tags$ul(
                   tags$li("Sodium Intake: High sodium intake is directly correlated with increased blood pressure. Diets low in sodium, such as the DASH (Dietary Approaches to Stop Hypertension) diet, are effective in managing and reducing hypertension."),
                   tags$li("Potassium and Magnesium: Higher intake of potassium (from fruits and vegetables) and magnesium (from nuts, seeds, and whole grains) is associated with lower blood pressure levels."),
                   tags$li("Overall Diet Quality: A diet that emphasizes whole foods and minimizes processed foods can help manage blood pressure.")
                 ),
                 h3("4. Type 2 Diabetes"),
                 tags$ul(
                   tags$li("Glycemic Index and Load: Diets high in refined carbohydrates and high glycemic index foods (like white bread and sugary snacks) can lead to insulin resistance and higher blood sugar levels. Conversely, low glycemic index foods (like legumes, whole grains, and vegetables) are better for blood sugar control."),
                   tags$li("Fiber: High fiber intake, particularly soluble fiber (found in oats, beans, and fruits), helps regulate blood sugar levels and improve insulin sensitivity."),
                   tags$li("Fats and Proteins: Diets rich in healthy fats (like those from avocados, nuts, and olive oil) and lean proteins can help manage blood glucose levels and reduce the risk of developing type 2 diabetes.")
                 )
        ),
        tabPanel("Key Research Questions", 
                 h3("1. Which specific dietary factors are most strongly correlated with variations in BMI categories (e.g., underweight, normal weight, overweight, obese)?"),
                 h3("2. How do demographic factors such as age and gender affect the effectiveness of various dietary interventions in improving health outcomes?"),
                 h3("3. For individuals with common chronic diseases (e.g., atherosclerosis, hypertension, diabetes), which dietary factors or dietary patterns are most prevalent compared to those observed in individuals with normal health metrics?")
        ),
        tabPanel("Hypotheses", 
                 h3("1. Hypothesis for Dietary Factors and BMI:"),
                 p("Specific dietary factors such as high sugar intake, low fiber consumption, and the frequency of processed food consumption are positively correlated with higher BMI categories (overweight and obese), while high consumption of fruits, vegetables, and whole grains is associated with lower BMI categories (underweight and normal weight)."),
                 h3("2. Hypothesis for Demographic Factors and Dietary Interventions:"),
                 p("Age and gender significantly influence the effectiveness of dietary interventions, with younger individuals and women showing greater improvements in health outcomes due to dietary changes compared to older individuals and men, due to differences in metabolism, lifestyle, and compliance with dietary recommendations."),
                 h3("3. Hypothesis for Dietary Patterns in Chronic Diseases:"),
                 p("Individuals with common chronic diseases (atherosclerosis, hypertension, diabetes) exhibit a higher prevalence of dietary patterns characterized by high saturated fat intake, low fruit and vegetable consumption, and higher overall caloric intake compared to individuals with normal health metrics, reflecting a significant difference in dietary choices and quality.")
        ),
        tabPanel("Exploratory Data Analysis", 
                 tabsetPanel(
                   tabPanel("Dietary Factors and BMI",
                            plotOutput("bmiPlot"),
                            plotOutput("dietTypeDist")  # Heatmap plot
                   ),
                   tabPanel("Atherosclerosis",
                            plotOutput("atheroBoxPlot"),
                            plotOutput("atheroDietDist")
                   ),
                   tabPanel("Hypertension",
                            plotOutput("htnBoxPlot"),
                            plotOutput("htnDietDist")
                   ),
                   tabPanel("Type 2 Diabetes",
                            plotOutput("diabBubblePlot"),
                            plotOutput("diabDietDist")
                   )
                 )
        )
      )
    )
  )
)

