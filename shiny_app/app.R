
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Read model output
fits <- readRDS(file.path(datadir, "model_fits.Rds"))

# Read data for testing
data <- read.csv(file.path(datadir, "test_data.csv")) %>% 
  mutate(date=lubridate::mdy(date))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))



# User interface
################################################################################

# User interface
ui <- fluidPage(
  
  # Title
  titlePanel("Domoic acid depuration forecast tool"),
  
  # Upload data
  p("The uploaded data file must be a CSV with the following columns: event_id, date, domoic_ppm_max"),
  fileInput(inputId="inFile", 
            label="Upload data file", 
            multiple = FALSE, 
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # Radio buttons
  radioButtons(inputId = "state", 
               label="Select region:",
               choices = c("C. California", "N. California", "Oregon", "Washington"),
               inline = T),
  
  # # Plot fit
  # plotOutput(outputId = "plot_data", width=750, height=500), 
  
  # Plot historical comparison
  plotOutput(outputId = "plot_data_hist", width=750, height=500)

  
)


# Server
################################################################################

# Server
server <- function(input, output, session){
  
  # Read file
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header)
  })
  
  # # Plot data
  # output$plot_data <- renderPlot({
  #   g <- plot_data(data = data)
  #   g
  # })
  
  # Plot data
  output$plot_data_hist <- renderPlot({
    g <- plot_data_hist(data = data, fits = fits)
    g
  })
  
}

shinyApp(ui = ui, server = server)
