# Install and load necessary packages
install.packages("shiny")
install.packages("ggplot2")
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Olympic Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file"),
      selectInput("plotType", "Choose a plot type:",
                  choices = c("Bar Plot", "Pie Chart", "Box Plot", "Scatter Plot", "Line Plot", "Histogram"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observe({
    req(input$file)
    data <- read.csv(input$file$datapath, header = TRUE)
    
    output$plot <- renderPlot({
      # Based on user input, generate the selected visualization
      if (input$plotType == "Bar Plot") {
        ggplot(data, aes(x = Sport, fill = Season)) +
          geom_bar(position = "dodge") +
          labs(title = "Number of Medals by Sport and Season")
      } else if (input$plotType == "Pie Chart") {
        ggplot(data, aes(x = "", fill = Season)) +
          geom_bar(width = 1) +
          coord_polar("y") +
          labs(title = "Proportion of Medals by Season")
      } else if (input$plotType == "Box Plot") {
        ggplot(data, aes(x = Season, y = Age, fill = Season)) +
          geom_boxplot() +
          labs(title = "Distribution of Athlete Age by Season")
      } else if (input$plotType == "Scatter Plot") {
        ggplot(data, aes(x = Age, y = Medal, color = Season)) +
          geom_point() +
          labs(title = "Scatter Plot of Age vs. Medal Count")
      } else if (input$plotType == "Line Plot") {
        ggplot(data, aes(x = Year, y = Medal, color = Season)) +
          geom_line() +
          labs(title = "Medal Count Over Time")
      } else if (input$plotType == "Histogram") {
        ggplot(data, aes(x = Age, fill = Season)) +
          geom_histogram(binwidth = 5, position = "dodge", alpha = 0.5) +
          labs(title = "Distribution of Athlete Age")
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
