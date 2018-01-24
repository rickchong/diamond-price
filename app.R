# Regression example from: https://rstudio-pubs-static.s3.amazonaws.com/94067_d1fdfafd20b14725a2578647031760c2.html
# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(tidyverse)
library(scales)

# Load data ----
dataDiamond <- diamonds

# Source helper functions -----
source("helpers.R")

# Run regression
regression <-
  lm(formula = I(log10(price)) ~ I(carat ^ (1 / 3)) + carat + cut + color + clarity,
     data = diamonds)
usd <- dollar_format(prefix = "USD ")

# User interface ----
ui <- fluidPage(titlePanel("Diamond price analysis"),
                tabsetPanel(
                  tabPanel("Carat vs Price",
                           sidebarLayout(
                             sidebarPanel(fluidRow(
                               column(
                                 4,
                                 checkboxGroupInput(
                                   "inputColor",
                                   h5("Select color"),
                                   choices = list(
                                     "D" = "D",
                                     "E" = "E",
                                     "F" = "F",
                                     "G" = "G",
                                     "H" = "H",
                                     "I" = "I",
                                     "J" = "J"
                                   ),
                                   selected = list(
                                     "D" = "D",
                                     "E" = "E",
                                     "F" = "F",
                                     "G" = "G",
                                     "H" = "H",
                                     "I" = "I",
                                     "J" = "J"
                                   )
                                 )
                               ),
                               column(
                                 8,
                                 checkboxGroupInput(
                                   "inputClarity",
                                   h5("Select clarity"),
                                   choices = list(
                                     "IF" = "IF",
                                     "VVS1" = "VVS1",
                                     "VVS2" = "VVS2",
                                     "VS1" = "VS1",
                                     "VS2" = "VS2",
                                     "SI1" = "SI1",
                                     "SI2" = "SI2",
                                     "I1" = "I1"
                                   ),
                                   selected = list(
                                     "IF" = "IF",
                                     "VVS1" = "VVS1",
                                     "VVS2" = "VVS2",
                                     "VS1" = "VS1",
                                     "VS2" = "VS2",
                                     "SI1" = "SI1",
                                     "SI2" = "SI2",
                                     "I1" = "I1"
                                   )
                                 )
                               )
                             ),
                             fluidRow(
                               column(
                                 4,
                                 checkboxGroupInput(
                                   "inputCut",
                                   h5("Select cut"),
                                   choices = list(
                                     "Ideal" = "Ideal",
                                     "Premium" = "Premium",
                                     "Very Good" = "Very Good",
                                     "Good" = "Good",
                                     "Fair" = "Fair"
                                   ),
                                   selected = list(
                                     "Ideal" = "Ideal",
                                     "Premium" = "Premium",
                                     "Very Good" = "Very Good",
                                     "Good" = "Good",
                                     "Fair" = "Fair"
                                   )
                                 )
                               ),
                               column(
                                 8,
                                 sliderInput(
                                   "inputCarat",
                                   h5("Select carat:"),
                                   min = 0,
                                   max = 5,
                                   value = c(0, 5),
                                   step = 0.01
                                 )
                               )
                             )),
                             mainPanel(plotOutput("dist"))
                             
                           )),
                  tabPanel(
                    "Estimate Price",
                    column(
                      6,
                      br(),
                      numericInput("predictCarat", label = "carat", value =
                                     "enter Carat"),
                      selectInput(
                        "predictColor",
                        label = "color",
                        choices = list(
                          "D" = "D",
                          "E" = "E",
                          "F" = "F",
                          "G" = "G",
                          "H" = "H",
                          "I" = "I",
                          "J" = "J"
                        )
                      ),
                      selectInput(
                        "predictClarity",
                        label = "clarity",
                        choices = list(
                          "IF" = "IF",
                          "VVS1" = "VVS1",
                          "VVS2" = "VVS2",
                          "VS1" = "VS1",
                          "VS2" = "VS2",
                          "SI1" = "SI1",
                          "SI2" = "SI2",
                          "I1" = "I1"
                        )
                      ),
                      selectInput(
                        "predictCut",
                        label = "cut",
                        choices = list(
                          "Ideal" = "Ideal",
                          "Premium" = "Premium",
                          "Very Good" = "Very Good",
                          "Good" = "Good",
                          "Fair" = "Fair"
                        )
                      ),
                      actionButton("executeCalculation", "Predict")
                    ),
                    column(6,
                           htmlOutput("outputPrice"))
                  )
                  
                  
                  
                ))


# Server logic ----
server <- function(input, output) {
  output$dist <- renderPlot({
    colorRow <- which(dataDiamond$color %in% c(input$inputColor))
    cutRow <- which(dataDiamond$cut %in% c(input$inputCut))
    clarityRow <-
      which(dataDiamond$clarity %in% c(input$inputClarity))
    caratRow <-
      which(dataDiamond$carat >= input$inputCarat[1] &
              dataDiamond$carat <= input$inputCarat[2])
    combineRow <-
      Reduce(intersect, list(colorRow, caratRow, cutRow, clarityRow))
    data <- dataDiamond[combineRow, ]
    
    ggplot(data, aes(x = data$price, y = data$carat)) + geom_point(aes(
      color = factor(data$clarity),
      shape = factor(data$cut)
    ))
  })
  #define event listener
  calculateAction <- eventReactive(input$executeCalculation, {
    calcInput <-
      data.frame(
        carat = input$predictCarat,
        cut = input$predictCut,
        color = input$predictColor,
        clarity = input$predictClarity
      )
    modelEstimate <- predict(
      regression,
      newdata = calcInput,
      interval = "prediction",
      level = .95
    )
    return(usd(round(10 ^ modelEstimate[1], digits = 0)))
  })
  output$outputPrice <- renderUI({
    HTML(paste(br(), "Your estimated price is:", h2(calculateAction())))
  })
}
# Run app ----
shinyApp(ui, server)
