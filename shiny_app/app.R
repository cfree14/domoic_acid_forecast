
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
data1 <- read.csv(file.path(datadir, "example_data1.csv")) %>% 
  mutate(date=lubridate::mdy(date))
data2 <- read.csv(file.path(datadir, "example_data2.csv")) %>% 
  mutate(date=lubridate::mdy(date))
data3 <- read.csv(file.path(datadir, "example_data3.csv")) %>% 
  mutate(date=lubridate::mdy(date))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))



# User interface
################################################################################

# User interface
ui <- fluidPage(
  
  # Title
  titlePanel("Domoic acid depuration forecast tool"),
  
  # Background
  h3("Background"),
  
  # Download example data
  p("Download and try out one of the following example files:"),
  a("Example 1", href="./example_data1.csv"),
  a("Example 2", href="./example_data1.csv"),
  a("Example 3", href="./example_data1.csv"),
  br(),
  br(),
  
  # Upload data
  h3("Upload data"),
  p("The uploaded data file must be a CSV with the following columns: event_id, date, domoic_ppm_max"),
  fileInput(inputId="inFile", 
            label="Upload data file", 
            multiple = FALSE, 
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # Data visualizer
  h3("Forecast depuration timeline"),
  
  # Plot historical? - 
  checkboxInput(inputId = "hist_yn",
                label = "Overlay historical depuration trajectories?",
                value=F),
  
  # State? - radio buttons
  radioButtons(inputId = "state", 
               label="Select region:",
               choices = c("C. California", "N. California", "Oregon", "Washington"),
               inline = T),
  
  # # Plot fit
  # plotOutput(outputId = "plot_data", width=750, height=500), 
  
  # Plot historical comparison
  plotOutput(outputId = "plot_data_hist", width=750, height=500),
  
  # Citation
  h3("Citation"),
  p("Please site this tool using the following paper:"),
  p("Free CM, Moore SM, Holland D, Shelton A. Predicting domoic acid depuration timelines in the US West Coast Dungeness crab fishery. In preparation for Harmful Algae."),
  br(),
  br()

  
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
  
  # Read file
  df = reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df)
  })
  
  # # Plot data
  # output$plot_data <- renderPlot({
  #   g <- plot_data(data = data)
  #   g
  # })
  
  # Plot data
  output$plot_data_hist <- renderPlot({
    g <- plot_data_hist(data = data1, 
                        fits = fits, 
                        state = input$state,
                        hist_yn = input$hist_yn)
    g
  })
  
  # Download example data
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
}

shinyApp(ui = ui, server = server)
