library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(DT)


combined_map <- read_csv("combined_map.csv", show_col_types = FALSE)
combined <- read_csv("combined.csv", show_col_types = FALSE)
continent_choices <- c("NA" = "Please select...", unique(combined_map$continent))

ui <- fluidPage(
  tags$style(HTML("
        body {
            font-family: Georgia , serif;
        }
    ")),
  navbarPage("Tracking Women's Empowerment",
                 tabPanel("Overview",
                          tabsetPanel(type = "tabs",
                                      tabPanel("Background", uiOutput("infoText")),
                                      tabPanel("Women's Political Empowerment Index", uiOutput("wpeiText")),
                                      tabPanel("Calculating Empowerment", uiOutput("empowermentLevelsText")),
                                      tabPanel("Insights and Analysis", uiOutput("analysisText")),
                                      tabPanel("Further Research", uiOutput("furtherText"))
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
)


server <- function(input, output, session) {
  output$infoText <- renderUI({
    HTML('
        <h3 style="text-align: center; font-weight: bold; text-decoration: underline;">Background</h3>

        <h4 style="font-weight: bold; font-style: italic;">SMART Questions</h4>
          <p style="text-indent: 20px;">1. Can women&#39;s political empowerment be categorized and reflect the changes in nation-state building, modernization, and gender equality over the 20th Century&#63; </p>
          <p style="text-indent: 20px;">2. What trends emerge from the mapping of these categories and how do changes in women&#39;s suffrage influence these categorizations&#63;</p>
          <p style="text-indent: 20px;">3. Does this mapping method showcase any regional patterns in women&#39;s political empowerment or suffrage&#63;</p>

        <h4 style="font-weight: bold; font-style: italic;">Why create this project&#63;</h4>
          <p style="text-indent: 20px;">The goal of this project is to analyze women&#39;s progression internationally in the political realm over 100 years. This will aid in understanding how, over time, women&#39;s access to civil liberties, participation in society, and representation in politics have changed. This study of women&#39;s political empowerment could showcase trends in increased empowerment and suffrage showcasing the divides between certain areas&#39; cultural identities or societal thoughts. The modern women&#39;s rights movement has seen international recognition and improvements in the everyday lives of women. While the dream of a woman head of state or representative continues to become a reality it is important to understand the struggle, spread, and growth that brought greater political empowerment.</p>
          
        <h4 style="font-weight: bold; font-style: italic;">Why 1900 - 2000&#63;</h4>
          <p style="text-indent: 20px;">The 20th Century is considered the era of nation-states and modernization which greatly impacted the geopolitical landscape of the time. The fall of empires such as the Ottomans and the Austro-Hungarians, the decolonization of the Global South, and the fall of the Soviet Union were just some of the events of the century that saw the creation of over 100 new nation-states. New sovereign states require the establishment of institutions that determine new rights, along with who deserves to be granted these liberties. While many new nations found themselves liberated, many women found their freedoms still limited although independence had occurred. This 100-year time span saw a prioritization of the new cultural and social identities while forgetting about equality and freedoms for all humans. The feminist movement, which originated from the Suffragettes of the early 1900s, adapted to the post-colonial world of the 1950s, radicalized in the 1970s, and modernized in the 1990s continues to evolve to expand its reach and influence. Modern women&#39;s suffrage and empowerment has been a century-long journey that must continue to be studied to better understand and appreciate gender rights progression into the new millennium.</p>
          
        <h4 style="font-weight: bold; font-style: italic;">Why include Suffrage&#63;</h4>
          <p style="text-indent: 20px;">In the United States, when asked when women received suffrage, or the right to vote, most people will answer 1920, however, this is not reality. We are taught that American women received suffrage through the 19th Amendment, but what is left out is the crucial detail that this only granted White women suffrage. Indigenous, Black, Hispanic, and Asian women were separately granted suffrage around 30 to 40 years later, and, these rights were not even guaranteed until the expansion of the Voting Rights Act in 1975! Even though suffrage is considered a known right for all American citizens regardless, of gender, native language, or ethnicity, it is important to remember and acknowledge the historical fight for suffrage both domestic and abroad. This fight, for many areas, resulted in a more empowered female population who took this victory and continued to push barriers and encourage women&#39;s political leadership and influence into the present day. Integrating suffrage for better context in the building and overall analysis of tracking women&#39;s empowerment is important.</p>
          
        <h4 style="font-weight: bold; font-style: italic;">How do I use this App&#63;</h4>
          <p style="text-indent: 20px;">The Overview Panel contains more information on the women&#39;s Political Empowerment Index, how the Empowerment Levels and Periods were formed, insights and analysis of the data set, and potential further research questions and challenges.</p>
          <p style="text-indent: 20px;">The three other panels in this app will allow users to view maps related to World, Continent, and Country data. These maps show levels of empowerment, suffrage, or combined empowerment and suffrage for the year selected using the slider at the top of the page. Further selections can be made to specify Continent or Country choices in their respective panels. The data used for all maps are available to download in the Data Tab of each panel.</p>

        <h4 style="font-weight: bold; font-style: italic;">Acknowledgments</h4>
          <p style="text-indent: 20px;">The Women&#39;s Political Empowerment Index and Suffrage data for this project was taken from the V-Dem Institute while the geographic data came from various R Studio packages. This app was created using Shiny.</p>
          <p style="text-indent: 20px;">This project was completed for the George Washington University Undergraduate Data Science Capstone by Athena Rodrigues with help from Professor Edwin Lo.</p>

    ')
  })
  output$wpeiText <- renderUI({     
      HTML('
        <h3 style="text-align: center; font-weight: bold; text-decoration: underline;">Women&#39;s Political Empowerment Index</h3>
            <p style="text-indent: 20px;">Women&#39;s Political Empowerment is defined as <q>a process of increasing capacity for women, leading to greater choice, agency, and participation in societal decision-making</q></p>
        <h4 style="font-weight: bold; font-style: italic;">Academic Research</h4>
          <p style="text-indent: 20px;">Created by Aksel Sundstron, Pamela Paxton, Yi-Ting Wang, and Staffan I. Lindberg for the V-Dem Institute, the Women&#39;s Political Empowerment Index combines information from three calculated indices about women&#39;s political and civil society progress and combines them into one index that measure&#39;s overall political empowerment. With data spanning over 100 years (1900-2012), an index of three varieties, and over 170 countries, this index provides the most comprehensive and best-covering measure of empowerment. This Index takes into account newly formed countries, changing central concepts of women&#39;s rights, and gives a better representation of the Global South. For further information on the creation of this index and its sub-dimensions, the group&#39;s paper <q>Women&#39;s Political Empowerment: A New Global Index, 1900-2012</q> can be referenced.</p>
          <img src="WPEI_animation.gif" style="display: block; margin: auto;">
        <h4 style="font-weight: bold; font-style: italic;">The Three Indices</h4>
          <p style="text-indent: 20px;">The WPEI is created by averaging the three indices listed below. Each of these indices refers to one of the three indicators (Choice, Agency, and Participation)  mentioned in the definition of Women&#39;s Political Empowerment representing a holistic representation for both Western and Nonwestern contexts.</p>
          <h5 style="font-weight: bold; font-style: italic;text-indent: 20px;">	Women&#39;s Civil Liberties Index</h5>
            <p style="text-indent: 40px;"> Takes into Account: Freedom of domestic movement for women, Freedom from forced labor for women, Property rights for women, Access to justice for women</p>
            <img src="liberties_animation.gif" style="display: block; margin: auto;">
          <h5 style="font-weight: bold; font-style: italic;text-indent: 20px;">Women&#39;s Civil Society Participation Index</h5>
            <p style="text-indent: 40px;"> Takes into Account: Freedom of discussion for women, Women&#39;s Participation in Civil Service Organizations, Percentage of female journalists</p>
            <img src="society_animation.gif" style="display: block; margin: auto;">
         <h5 style="font-weight: bold; font-style: italic;text-indent: 20px;">	Women&#39;s Political Participation Index</h5>
            <p style="text-indent: 40px;">Takes into Account: Power distribution by gender, Political position representation, Presence of women in legislature</p>
            <img src="participation_animation.gif" style="display: block; margin: auto;">
    ') })
  output$empowermentLevelsText <- renderUI({"Empowerment Levels."})    
  output$analysisText <- renderUI({ "Insights and analysis content." })
  output$furtherText <- renderUI({
    HTML('
        <h3 style="text-align: center; font-weight: bold; text-decoration: underline;">Three Different Research Paths</h3>
          <p style="text-indent: 20px;">Still seen today, there has always been a significant divide between the “First and Second World” of North America and Europe and the “Third World” of Africa, Asia, and South America. Much of this can be traced back to the age of colonization as these “Third World” countries were exploited and placed under the prior&#39;s control resulting in large discrepancies for research purposes due to lacking efforts in these areas. Further research would explore the impact of this divide on a country&#39;s suffrage and empowerment scores.</p>
        <h4 style="font-weight: bold; font-style: italic;">1. Pre-decolonization vs. Post-decolonization</h4>
          <p style="text-indent: 20px;">The 1940s and 1950s saw the greatest influx of new nation-states. A comparison of these levels and the actual indices for the 1900-1945 period and the 1945-2000 period may give further insight into the impact of colonization. Furthermore, these periods align with pre-World War and post-World War years so this could also be integrated into the study with a separate division and analysis of the World War years&#39; data.</p>
        <h4 style="font-weight: bold; font-style: italic;">2. Global South vs. Global North</h4>
        <p style="text-indent: 20px;">This continental research divide encompasses the Global North and the Global South (divided essentially by the equator when not including Australia and New Zealand). Although Sundstrom et al. cover this bias it would be interesting to compare the WPEI measures with other measurements known for their misrepresentation of the Global South to better track and analyse the differences.</p>
        <h4 style="font-weight: bold; font-style: italic;">3. Impact of Historical Events</h4>
        <p style="text-indent: 20px;">Attempting to track significant historical events, whether this be the passage of gender equality acts or natural disasters, would also give better background to the reason why a certain area is at the empowerment level it has reached. Similarly, this would help in understanding why some countries see fluctuations and also track regime/democratic changes the location faced.</p>
         ')
  })
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