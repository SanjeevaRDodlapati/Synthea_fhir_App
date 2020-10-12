#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)

data <- read.csv('./patients.csv', stringsAsFactors = FALSE)
dataset <- data %>% select(HEALTHCARE_EXPENSES,HEALTHCARE_COVERAGE, BIRTHDATE, COUNTY, CITY)

medications <- read.csv('./medications.csv', stringsAsFactors = FALSE)
procedures <- read.csv('./procedures.csv', stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
  tags$div(
  
  tags$h1("Synthea FHIR App")
  
  ),
  
  hr(),
  fluidRow(
    column(12, tags$div(title = "Massachusett State Synthea FHIR Data County-wise Expenses vs Coverage in USD"))),
  
  
  fluidRow(
    
    column(3,
           h3("Massachusetts FHIR Data"),
           br(),
           # checkboxInput('jitter', 'Jitter'),
           # checkboxInput('smooth', 'Smooth')
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),

    ),
    column(4,
           selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
           selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    )
  ),
  br(),
  mainPanel(
    tabsetPanel(
      tabPanel("Expenses vs Coverage", plotOutput("distPlot")),
      tabPanel("Top 10 expensive conditions", tableOutput("table")),
      tabPanel("Top 10 medications", tableOutput("table2")),
      tabPanel("Top 10 expensive procedures", tableOutput("table3"))
      
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  data_county <- data %>% group_by(COUNTY) %>% select(CITY, HEALTHCARE_EXPENSES, HEALTHCARE_COVERAGE)

    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        ggplot() + geom_point(aes(x = data_county$HEALTHCARE_EXPENSES, y = data_county$HEALTHCARE_COVERAGE, col = data_county$COUNTY)) +
          scale_x_log10() + scale_y_log10()
    })
    
    x <- medications %>% group_by(REASONDESCRIPTION) %>% summarize(County_Total_USD = sum(TOTALCOST)) %>% top_n(10, County_Total_USD)
    y <- medications %>% group_by(DESCRIPTION) %>% summarize(Count = length(DESCRIPTION)) %>% top_n(10, Count) %>% arrange(desc(Count))
    z <- procedures %>% group_by(REASONDESCRIPTION) %>% summarize(Count = length(REASONDESCRIPTION)) %>% top_n(10, Count) %>% arrange(desc(Count))
    output$table <- renderTable(x)
    output$table2 <- renderTable(y)
    output$table3 <- renderTable(z)
}

# Run the application 
shinyApp(ui = ui, server = server)
