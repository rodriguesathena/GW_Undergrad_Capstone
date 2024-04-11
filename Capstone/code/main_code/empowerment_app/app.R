library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(DT)


combined_map <- read_csv("combined_map.csv", show_col_types = FALSE)
combined <- read_csv("combined.csv", show_col_types = FALSE)
continent_choices <- c("NA" = "Please select...", unique(combined_map$continent))

ui <- navbarPage("Tracking Women's Empowerment",
                 tabPanel("Information",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Overview", textOutput("overviewText")),
                                      tabPanel("What is the WPEI?", textOutput("wpeiText")),
                                      tabPanel("Calculating Empowerment", textOutput("empowermentLevelsText")),
                                      tabPanel("Insights and Analysis", textOutput("analysisText")),
                                      tabPanel("Further Research Potential", textOutput("furtherText"))
                          )
                 ),
                 tabPanel("Whole World",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Levels of Empowerment",
                                               sliderInput("yearInput", "Year:", 
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("levelsworldMapDescription"),
                                               plotOutput("levelsworldMapOutput")
                                      ), 
                                      tabPanel("Right to Vote",
                                               sliderInput("voteYearInput", "Year:",  
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("voteworldMapDescription"),
                                               plotOutput("voteworldMapOutput")
                                      ), 
                                      tabPanel("Empowerment and Voting",
                                               sliderInput("allYearInput", "Year:",  
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("allworldMapDescription"),
                                               plotOutput("allworldMapOutput")
                                      ),
                                      tabPanel("World Data",
                                               DTOutput("worldDataTable"),
                                               downloadButton("downloadWorldData", "Download World Data")
                                      )
                          )
                 ),
                 tabPanel("By Continent",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Levels of Empowerment",
                                               selectInput("continentSelection", "Select Continent:",
                                                           choices = c("Americas", "Europe", "Africa", "Asia", "Oceania")),
                                               sliderInput("continentYearInput", "Year:",  
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("levelscontinentMapDescription"),
                                               plotOutput("levelscontinentMapOutput")
                                      ), 
                                      tabPanel("Right to Vote",
                                               selectInput("voteContinentSelection", "Select Continent:", 
                                                           choices = c("Americas", "Europe", "Africa", "Asia", "Oceania")),
                                               sliderInput("voteContinentYearInput", "Year:", # 
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("votecontinentMapDescription"),
                                               plotOutput("votecontinentMapOutput")
                                      ), 
                                      tabPanel("Empowerment and Voting",
                                               selectInput("allContinentSelection", "Select Continent:", 
                                                           choices = c("Americas", "Europe", "Africa", "Asia", "Oceania")),
                                               sliderInput("allContinentYearInput", "Year:", # 
                                                           min = min(combined_map$year), max = max(combined_map$year), value = 1900, step = 1),
                                               textOutput("allcontinentDescription"),
                                               plotOutput("allcontinentOutput")
                                      ),
                                      tabPanel("Continent Data",
                                               DTOutput("continentDataTable"),
                                               downloadButton("downloadContinentData", "Download Continent Data")
                                      )
                          )
                 ),
                 tabPanel("By Country",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Country Map",
                                               selectInput("continentSelect", "Select Continent:", choices = continent_choices),
                                               uiOutput("countrySelectUI"),
                                               sliderInput("yearInput", "Select Year:", min = min(combined_map$year), max = max(combined_map$year), value = 1900),
                                               plotOutput("countryMapOutput")
                                      ),
                                      tabPanel("Country Data", 
                                               DTOutput("countryDataTable"),
                                               downloadButton("downloadCountryData", "Download Country Data")
                                      )
                          
                          )
                 )
)


server <- function(input, output, session) {
# INFORMATION TAB
  output$overviewText <- renderText({ "Overview content goes here." })
  output$wpeiText <- renderText({ "Information about WPEI." })
  output$empowermentLevelsText <- renderText({ "Calculating empowerment levels." })    
  output$analysisText <- renderText({ "Insights and analysis content." })
  output$furtherText <- renderText({ "Further research potential discussion." })
# WORLD TAB - Levels Map
  output$levelsworldMapDescription <- renderText({
    "This map shows the level of women's empowerment around the world for the selected year."    
  })
  output$levelsworldMapOutput <- renderPlot({
    filtered_data <- combined_map %>% filter(year == input$yearInput)
    ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = category)) +
      geom_polygon() +
      theme_void() +
      scale_fill_manual(values = c("nascent" = "#f2ab9b", "developing" = "#3b5e8c", "emerging" = "#f2b872", "established" = "#957DAD"), drop = FALSE) +
      labs(title = paste("Year:", input$yearInput)) +
      coord_quickmap()
  })  
# WORLD TAB - Suffrage Map
  output$voteworldMapDescription <- renderText({
    "This map shows women's suffrage globally, indicating whether women had the right to vote in the selected year."
  })
  output$voteworldMapOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$voteYearInput)
    
    ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = factor(voting))) +
      geom_polygon() +
      scale_fill_manual(values = c("0" = "black", "1" = "#806E88"),
                        labels = c("0" = "No Voting", "1" = "Voting")) +
      theme_void() +
      labs(title = paste("Global Voting Rights in", input$voteYearInput),
           fill = "Voting Status") +
      theme(legend.position = "right")
  })
# WORLD TAB - Combined Map
  output$allworldMapDescription <- renderText({
    "This map combines information on women's empowerment and voting rights around the world for the selected year."
  })
  output$allworldMapOutput <- renderPlot({
    filtered_data <- combined_map %>% filter(year == input$allYearInput)
    ggplot(filtered_data, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = category)) +
      geom_polygon(data = filtered_data %>% filter(voting == 1), aes(color = "Yes"), fill = NA, size = 0.25, linetype = "dashed") +
      geom_polygon(data = filtered_data %>% filter(voting == 0), aes(color = "No"), fill = NA, size = 0.25) +
      scale_fill_manual(values = c("nascent" = "#f2ab9b", "developing" = "#3b5e8c", "emerging" = "#f2b872", "established" = "#957DAD"), drop = FALSE) +
      scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
      theme_void() +
      labs(title = paste("Year:", input$allYearInput)) +
      coord_quickmap()
  })
# WORLD TAB - Data Table
  output$worldDataTable <- renderDT({
    world_data <- combined %>%
      select(region, year, wpei, voting, continent)
    
    datatable(world_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$downloadWorldData <- downloadHandler(
    filename = function() {
      paste("World-Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      world_data <- combined %>%
        select(year, voting, wpei, continent, country)
      write.csv(world_data, file, row.names = FALSE)
    }
  )
# CONTINENT TAB - Levels Map
  output$levelscontinentMapDescription <- renderText({
    "This map shows the level of women's empowerment for the selected continent and year."
  })
  output$levelscontinentMapOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$continentYearInput, continent == input$continentSelection)
    ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = category)) +
      geom_polygon() +
      theme_void() +
      scale_fill_manual(values = c("nascent" = "#f2ab9b", "developing" = "#3b5e8c", "emerging" = "#f2b872", "established" = "#957DAD"), drop = FALSE) +
      labs(title = paste("Continent, Year:", input$continentSelection, ",", input$continentYearInput)) +
      coord_quickmap()
  })
# CONTINENT TAB - Suffrage Map
  output$votecontinentMapDescription <- renderText({
    "This map shows women's suffrage for the selected continent and year."
  })
  output$votecontinentMapOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$voteContinentYearInput, continent == input$voteContinentSelection)
    
    ggplot(filtered_data, aes(x = long, y = lat, group = group, fill = factor(voting))) +
      geom_polygon() +
      scale_fill_manual(values = c("0" = "black", "1" = "#806E88"),
                        labels = c("0" = "No Voting", "1" = "Voting")) +
      theme_void() +
      labs(title = paste("Voting Rights in", input$voteContinentSelection, "in", input$voteContinentYearInput),
           fill = "Voting Status") +
      theme(legend.position = "right")
  })
# CONTINENT TAB - Combined Map
  output$allcontinentDescription <- renderText({
    "This map combines information on women's empowerment and voting rights for the selected continent and year."
  })
  output$allcontinentOutput <- renderPlot({
    filtered_data <- combined_map %>%
      filter(year == input$allContinentYearInput, continent == input$allContinentSelection)
    ggplot(filtered_data, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = category)) +
      geom_polygon(data = filtered_data %>% filter(voting == 1), aes(color = "Yes"), fill = NA, size = 0.25, linetype = "dashed") +
      geom_polygon(data = filtered_data %>% filter(voting == 0), aes(color = "No"), fill = NA, size = 0.25) +
      scale_fill_manual(values = c("nascent" = "#f2ab9b", "developing" = "#3b5e8c", "emerging" = "#f2b872", "established" = "#957DAD"), drop = FALSE) +
      scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
      theme_void() +
      labs(title = paste("Continent, Year", input$allContinentSelection, ",", input$allContinentYearInput)) +
      coord_quickmap()
  })
# CONTINENT TAB - Data Table
  output$continentDataTable <- renderDT({
    # Ensure a valid continent is selected (not "NA" or "Please select...")
    req(input$continentSelection != "NA", input$continentSelection != "")
    
    filtered_data <- combined %>%
      filter(continent == input$continentSelection) %>%
      select(region, year, wpei, voting)
    
    datatable(filtered_data, options = list(pageLength = 10, scrollX = TRUE))
  })  
  output$downloadContinentData <- downloadHandler(
    filename = function() {
      paste("Continent-Data-", input$continentSelection, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(input$continentSelection)
      filtered_data <- combined %>%
        filter(continent == input$continentSelection) %>%
        select(country, year, voting, wpei)
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
# COUNTRY TAB - Combined Map
  output$countrySelectUI <- renderUI({
    selected_continent <- input$continentSelect
    if(selected_continent == "NA" || is.null(selected_continent)) {
      country_choices <- c("NA" = "Please select...")
    } else {
      countries <- unique(combined_map$region[combined_map$continent == selected_continent])
      country_choices <- c("NA" = "Please select...", sort(countries))
    }
    selectInput("countrySelect", "Select Country:", choices = country_choices)
  })
  output$countryDescription <- renderText({
    "This map combines information on women's empowerment and voting rights for the selected country and year."
  })
  output$countryMapOutput <- renderPlot({
    selected_year <- input$yearInput
    selected_country <- input$countrySelect
    country_data <- combined_map %>%
      filter(year == selected_year, region == selected_country)
    ggplot(data = country_data, aes(x = long, y = lat, group = group, fill = category)) +
      geom_polygon() +
      geom_polygon(data = country_data %>% filter(voting == 1), aes(color = "Yes"), fill = NA, size = 0.25, linetype = "dashed") +
      geom_polygon(data = country_data %>% filter(voting == 0), aes(color = "No"), fill = NA, size = 0.25) +
      scale_fill_manual(values = c("nascent" = "#f2ab9b", "developing" = "#3b5e8c", "emerging" = "#f2b872", "established" = "#957DAD"), drop = FALSE) +
      scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
      theme_void() +
      labs(title = paste("Empowerment Category in", selected_country, ",", selected_year), fill = "Category") +
      coord_quickmap()
  })
# COUNTRY TAB - Data Table
  output$countryDataTable <- renderDT({
    selected_country <- input$countrySelect
    if(is.null(selected_country) || selected_country == "NA") {
      return(data.frame())
    }
    country_data <- combined %>%
      filter(region == selected_country, year >= 1900, year <= 2000) %>%
      select(year, voting, wpei, category)
    datatable(country_data, options = list(pageLength = 10))
  })
  output$downloadCountryData <- downloadHandler(
    filename = function() {
      paste("Country-Data-", input$countrySelect, ".csv", sep="")
    },
    content = function(file) {
      selected_country <- input$countrySelect
      country_data <- combined_map %>%
        filter(region == selected_country, year >= 1900, year <= 2000) %>%
        select(year, voting, wpei, category)
      write.csv(country_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)