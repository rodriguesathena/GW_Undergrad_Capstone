library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load your data
# Make sure to adjust the path to where your actual data is located
combined_map <- read_csv("combined_map.csv")

ui <- fluidPage(
  titlePanel("World Maps with Year Slider"),
  sidebarLayout(
    sidebarPanel(
      # Slider for year selection
      sliderInput("yearInput",
                  "Year:",
                  min = min(combined_map$year),
                  max = max(combined_map$year),
                  value = 2000, # Default initial year
                  step = 1)
    ),
    mainPanel(
      # Tabs to display each map
      tabsetPanel(type = "tabs",
                  tabPanel("Categories World", plotOutput("categoriesWorldOutput")),
                  tabPanel("Voting World", plotOutput("votingWorldOutput"))
      )
    )
  )
)

server <- function(input, output) {
  # Render Categories World Map
  output$categoriesWorldOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$yearInput)
    ggplot(data = filtered_data, aes(x = long, y = lat, group = group, fill = category)) +
      geom_polygon() +
      theme_classic() +
      labs(title = paste("Empowerment Category in the World -", input$yearInput), fill = "Category")
  })
  
  # Render Voting World Map
  output$votingWorldOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$yearInput)
    ggplot(data = filtered_data, aes(x = long, y = lat, group = group, fill = voting)) +
      geom_polygon() +
      theme_classic() +
      labs(title = paste("Voting in the World -", input$yearInput), fill = "Voting")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
