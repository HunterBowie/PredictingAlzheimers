library(shiny)
library(bslib)
library(tidymodels)

model <- readRDS("predictor.rds")

ui <- fluidPage(
    theme = bs_theme(version = 5),
    title = "Alzheimers Predictor",
    h1("Alzheimers Early Warning Predictor", style = "text-align: center; margin: 20px;"),
    layout_columns(
        card(
            card_header("Patient Data Entry", style = "font-size: 18px;"),
            numericInput("year", "Year of Birth", value = 1950, min = 1900, max = 2025),
            numericInput("nwbv", "Normalized Brain Volume", value = .8, step = 0.01, min = 0, max = 1),
            selectInput("educ", "More than 4 Years of Post-Secondary Education", choices = c("Yes", "No"), selected = "No"),
            actionButton("predict", "Process"),
        ),
        card(
            card_header("Predictor Results", style = "font-size: 18px;"),
            verbatimTextOutput("prediction"),
        ),
    ),
    # Reference bar just below
    tags$div(
        style = "
      margin-top: 15px;
      padding: 8px 15px;
      background-color: #f8f9fa;
      border: 1px solid #ccc;
      border-radius: 8px;
      text-align: center;
      font-size: 14px;
      color: #555;
    ",
        "Data source: OASIS-1 and OASIS-2 MRI datasets"
    )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
    # eventReactive triggers only when button is pressed
    pred <- eventReactive(input$predict, {
        new_data <- data.frame(
            Age = 2025 - input$year,
            nWBV = input$nwbv,
            HighlyEducated = input$educ == "Yes"
        )

        # Make prediction
        result <- predict(model, new_data)$.pred_class
        if (result == "TRUE") {
            conf <- predict(model, new_data, type = "prob")$.pred_TRUE * 100
            return(sprintf("Detected with %.1f%% Confidence", conf))
        }
        conf <- predict(model, new_data, type = "prob")$.pred_FALSE * 100
        return(sprintf("Not Detected with %.1f%% Confidence", conf))
    })

    # Render the prediction
    output$prediction <- renderPrint({
        pred() # only updates when button pressed
    })
}

shinyApp(ui = ui, server = server)
