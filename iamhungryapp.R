library(shiny); library(tidyverse); library(stringr); library(sf)
library(tidycensus); library(httr); library(jsonlite); library(RColorBrewer)

# Load data
source("data_prep.R")
Geo.CD <- GeoData_Loader()
Mosaic.CD <- MosaicData_Loader()
crime_groups_choices <- Geo.CD$Crm.Cd.Group %>% unique() %>% sort()
time_choices <- c("morning", "afternoon", "night", "late_night")
AREA_NAME <- Mosaic.CD$`AREA NAME` %>% unique()
zipcodes_final

# Add periods to the CD dataset
CD <- Geo.CD %>%
  mutate(period = case_when(
    hour >= 6 & hour < 12 ~ "morning",
    hour >= 12 & hour < 18 ~ "afternoon",
    hour >= 18 & hour < 24 ~ "night",
    (hour >= 0 & hour < 6) | hour == 24 ~ "late_night"
  ))

# UI
ui <- fluidPage(
  titlePanel("Crime Analysis and Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Crime Pie Chart",
                 checkboxGroupInput(
                   inputId = "selected_crime_groups",
                   label = "Select Crime Groups:",
                   choices = crime_groups_choices,
                   selected = crime_groups_choices[1:3]
                 )
        ),
        tabPanel("Crime Proportions by Time",
                 checkboxGroupInput(
                   inputId = "selected_periods",
                   label = "Select Time Period(s):",
                   choices = time_choices,
                   selected = time_choices
                 ),
                 selectInput(
                   inputId = "crime_for_time",
                   label = "Select Crime Group:",
                   choices = crime_groups_choices,
                   selected = crime_groups_choices[1]
                 )
        ),
        tabPanel("Highlight Map",
                 selectInput(
                   inputId = "highlight_crime",
                   label = "Select Crime Group to Highlight:",
                   choices = crime_groups_choices,
                   selected = crime_groups_choices[1]
                 )
        ),
        tabPanel("Crime and Socioeconomic Factors",
                 selectInput(
                   inputId = "factor_choice",
                   label = "Select Factor for Comparison:",
                   choices = c("Median Income" = "median_household_income",
                               "Unemployment Rate" = "unemployment",
                               "Poverty Gap" = "gap")
                 )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Crime Pie Chart", plotOutput("crimePieChart")),
        tabPanel("Crime Proportions by Time", plotOutput("timeBarChart"), plotOutput("crimeByTimeChart")),
        tabPanel("Crime Per Population Map", plotOutput("crimeMap")),
        tabPanel("Highlight Map", plotOutput("highlightMap")),
        tabPanel("Crime and Socioeconomic Factors", plotOutput("socioeconomicPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Crime Pie Chart
  output$crimePieChart <- renderPlot({
    req(input$selected_crime_groups)
    
    pie_data <- CD %>%
      filter(Crm.Cd.Group %in% input$selected_crime_groups) %>%
      group_by(Crm.Cd.Group) %>%
      count(name = "n") %>%
      ungroup() %>%
      mutate(proportion = n / sum(n))
    
    ggplot(pie_data, aes(x = "", y = proportion, fill = Crm.Cd.Group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Crime Proportions by Group", fill = "Crime Group") +
      theme_void()
  })
  
  # Crime Proportions by Time: Bar Plot
  output$timeBarChart <- renderPlot({
    req(input$selected_periods)
    
    total_count <- CD %>%
      group_by(Crm.Cd.Group) %>%
      count(name = "total_n")
    
    time_comp <- CD %>%
      filter(period %in% input$selected_periods) %>%
      group_by(Crm.Cd.Group) %>%
      count(name = "n") %>%
      left_join(total_count, by = "Crm.Cd.Group") %>%
      mutate(prop = n / total_n)
    
    ggplot(time_comp, aes(x = Crm.Cd.Group, y = prop, fill = Crm.Cd.Group)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = "Proportions by Selected Time Periods",
        x = "Crime Group",
        y = "Proportion"
      ) +
      theme_minimal()
  })
  
  # Crime Proportions by Time: Period Composition
  output$crimeByTimeChart <- renderPlot({
    req(input$crime_for_time)
    
    crime_time_data <- CD %>%
      filter(Crm.Cd.Group == input$crime_for_time) %>%
      group_by(period) %>%
      count(name = "n") %>%
      ungroup() %>%
      mutate(prop = n / sum(n))
    
    ggplot(crime_time_data, aes(x = "", y = prop, fill = period)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      labs(
        title = paste("Crime Composition by Time: ", input$crime_for_time),
        fill = "Time Period"
      ) +
      theme_void()
  })
  
  # Crime Per Population Map
  output$crimeMap <- renderPlot({
    area_crime <- CD %>%
      group_by(`AREA NAME`) %>%
      count(name = "n") %>%
      left_join(zipcodes_final, by = "AREA NAME") %>%
      mutate(CrimePerPopulation = n / Total_population)
    
    boundary <- boundary %>%
      left_join(area_crime, by = c("APREC" = "AREA NAME"))
    
    ggplot(boundary) +
      geom_sf(aes(fill = CrimePerPopulation)) +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Crime Per Population", fill = "Crime Rate") +
      theme_minimal()
  })
  
  # Highlight Map
  output$highlightMap <- renderPlot({
    req(input$highlight_crime)
    
    grouped_data <- CD %>%
      group_by(`AREA NAME`, Crm.Cd.Group) %>%
      count() %>%
      ungroup()
    
    proportional_data <- grouped_data %>%
      group_by(`AREA NAME`) %>%
      mutate(proportion = n / sum(n)) %>%
      ungroup()
    
    highlight_prob <- proportional_data %>%
      filter(Crm.Cd.Group == input$highlight_crime) %>%
      arrange(desc(proportion)) %>%
      head(5) %>%
      pull(`AREA NAME`)
    
    highlight_count <- grouped_data %>%
      filter(Crm.Cd.Group == input$highlight_crime) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(`AREA NAME`)
    
    boundary <- boundary %>%
      mutate(highlight_cat = case_when(
        APREC %in% highlight_prob & APREC %in% highlight_count ~ "BOTH",
        APREC %in% highlight_count ~ "COUNT",
        APREC %in% highlight_prob ~ "PROB",
        TRUE ~ "OTHERS"
      ))
    
    ggplot(boundary) +
      geom_sf(aes(fill = highlight_cat)) +
      scale_fill_manual(values = c("BOTH" = "red", "COUNT" = "green", "PROB" = "blue", "OTHERS" = "grey")) +
      labs(title = paste("Highlight Map:", input$highlight_crime), fill = "Category") +
      theme_minimal()
  })
  
  # Crime and Socioeconomic Factors
  output$socioeconomicPlot <- renderPlot({
    req(input$factor_choice)
    
    area_crime <- CD %>%
      group_by(`AREA NAME`) %>%
      count(name = "n") %>%
      left_join(zipcodes_final, by = "AREA NAME") %>%
      mutate(CrimePerPopulation = n / Total_population,
             unemployment = Total_unemployed / (Total_employed + Total_unemployed),
             gap = (poverty_under_50 + poverty_50_to_99 + poverty_100_to_124) / poverty_200_and_over)
    
    ggplot(area_crime, aes_string(x = input$factor_choice, y = "CrimePerPopulation")) +
      geom_point() +
      geom_smooth(method = "loess", span = 1, se = FALSE) +
      labs(
        title = paste("Crime Per Population vs", input$factor_choice),
        x = input$factor_choice,
        y = "Crime Per Population"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
