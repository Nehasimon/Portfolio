library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #D6EAF8; 
      }
      .titlePanel {
        background-color: #EAFAF1; 
      }
      .predictionTitle {
        background-color: #3498DB; 
        color: white; 
        padding: 10px;
        margin-bottom: 10px; 
        font-size: 24px;
        border-radius: 10px;
      }
      .predictionContainer {
        padding: 20px; 
        background-color: #EBF5FB; 
        border-radius: 10px;
      }
    "))
  ),
  titlePanel("Stroke Prediction"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age:", min = 0, max = 100, value = 50),
      radioButtons("hypertension", "Hypertension:", choices = c("Yes", "No"), selected = "No"),
      radioButtons("heart_disease", "Heart Disease:", choices = c("Yes", "No"), selected = "No"),
      sliderInput("avg_glucose_level", "Average Glucose Level:", min = 50, max = 300, value = 100),
      numericInput("height_feet", "Height (Feet):", value = NULL),
      numericInput("height_inches", "Height (Inches):", value = NULL),
      numericInput("weight_lbs", "Weight (lbs):", value = NULL),
      radioButtons("gender", "Gender:", choices = c("Female", "Male"), selected = "Male"),
      radioButtons("ever_married", "Marital status:", choices = c("Yes", "No"), selected = "No"),
      selectInput("work_type", "Work Type:",
                  choices = c("Private", "Self-employed", "Govt_job", "children", "Other"), selected = "Private"),
      radioButtons("Residence_type", "Residence Type:", choices = c("Rural", "Urban"), selected = "Urban"),
      selectInput("smoking_status", "Smoking Status:",
                  choices = c("never smoked", "formerly smoked", "smokes", "Unknown"), selected = "Unknown"),
      actionButton("predict_btn", "Predict"),
      hidden(div(id = "loading", class = "loader", "Calculating..."))
    ),
    mainPanel(
      div(class = "predictionContainer",
          div("Stroke Prediction Results", class = "predictionTitle"),
          conditionalPanel(condition = "output.loading == 1",
                           div(id = "loading", class = "loader", "Calculating...")),
          textOutput("prediction") 
      )
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  pred <- readRDS("LR_model.RDS")
  
  # Function to calculate BMI from height in feet and inches and weight in pounds
  calculate_bmi <- function(height_feet, height_inches, weight_lbs) {
    height <- (height_feet * 12) + height_inches
    bmi <- (weight_lbs / (height^2)) * 703
    return(bmi)
  }
  
  observe({
    req(input$height_feet, input$height_inches, input$weight_lbs)
    
    if (input$height_feet == 0 || input$height_inches == 0 || input$weight_lbs == 0) {
      disable("predict_btn")  # Disable predict button
    } else {
      enable("predict_btn")   # Enable predict button
    }
  })
  
  # Observer to monitor changes in input fields
  observeEvent(input$predict_btn, {
    output$loading <- renderUI({
      1
    })
    
    age <- input$age
    hypertension <- ifelse(input$hypertension == "Yes", 1, 0)
    heart_disease <- ifelse(input$heart_disease == "Yes", 1, 0)
    avg_glucose_level <- input$avg_glucose_level
    height_feet <- input$height_feet
    height_inches <- input$height_inches
    weight_lbs <- input$weight_lbs
    bmi <- calculate_bmi(height_feet, height_inches, weight_lbs)
    gender <- ifelse(input$gender == "Female", 0, 1)
    ever_married <- ifelse(input$ever_married == "Yes", 1, 0)
    work_type <- ifelse(input$work_type == "Private", 0, 
                        ifelse(input$work_type == "Self-employed", 1,
                               ifelse(input$work_type == "Govt_job", 2, 
                                      ifelse(input$work_type == "children", 3, 4))))
    Residence_type <- ifelse(input$Residence_type == "Rural", 0, 1)
    smoking_status <- ifelse(input$smoking_status == "never smoked", 0, 
                             ifelse(input$smoking_status == "formerly smoked", 1, 
                                    ifelse(input$smoking_status == "smokes", 2, 3)))
    
    # Make predictions using the loaded model
    prediction <- predict(pred, newdata = data.frame(age = age,
                                                     hypertension = hypertension,
                                                     heart_disease = heart_disease,
                                                     avg_glucose_level = avg_glucose_level,
                                                     bmi = bmi,
                                                     gender = gender,
                                                     ever_married = ever_married,
                                                     work_type = work_type,
                                                     Residence_type = Residence_type,
                                                     smoking_status = smoking_status),
                          type = 'response')
    
    # Display the prediction percentage
    output$prediction <- renderText({
      prediction_percent <- round(prediction * 100, digits = 1)
      paste("Your Stroke prediction Percentage:", prediction_percent, "%")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
