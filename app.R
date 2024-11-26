library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)

# Read in crime data
Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")

Cd <- Crime_Data %>%
  mutate(
    `DATE OCC` = as.Date(`DATE OCC`, format = "%m/%d/%Y"),
    `Date Rptd` = as.Date(`Date Rptd`, format = "%m/%d/%Y"),
    OCC_year = year(`DATE OCC`),
    `Dur Rptd` = as.numeric(`Date Rptd` - `DATE OCC`)
  ) %>%
  filter(
    OCC_year == 2023,
    LAT != 0,
    LON != 0,
    `Dur Rptd` > 0
  )

# Read in LAPD division boundaries
boundary <- st_read("LAPD_Div/LAPD_Divisions.shp")
# Transform boundary CRS to match crime data CRS
boundary <- st_transform(boundary, crs = 4326)

ui <- fluidPage(
  titlePanel("Analysis of Crime Data Based on Duration Reported Range"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "start_dur",
        label = "Start Point (Minimum Value of Duration Reported (Unit: days)):",
        value = 7,
        min = 1,
        step = 7
      ),
      checkboxInput(
        inputId = "use_end",
        label = "Select Option (End Point)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.use_end == true",
        numericInput(
          inputId = "end_dur",
          label = "End Point (Maximum Value of Duration Reported (Unit: days)):",
          value = 21,
          min = 1,
          step = 7
        )
      ),
      checkboxInput(
        inputId = "heatmap",
        label = "Show Heatmap",
        value = FALSE
      )
    ),
    mainPanel(
      plotOutput("crimePlot", width = "100%", height = "600px"),
      textOutput("crimeStats"),
      plotOutput("areaBarPlot")
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (input$use_end && input$end_dur < input$start_dur) {
      updateNumericInput(session, "end_dur", value = input$start_dur)
    }
  })
  
  filteredData <- reactive({
    start_value <- input$start_dur
    end_value <- if (input$use_end) input$end_dur else max(Cd$`Dur Rptd`, na.rm = TRUE)
    
    Cd %>%
      filter(`Dur Rptd` >= start_value & `Dur Rptd` <= end_value)
  })
  
  output$crimePlot <- renderPlot({
    Cd_filtered <- filteredData()
    
    base_plot <- ggplot() +
      labs(
        title = paste0(
          "Scatterplot of Crime Locations for Duration Reported ",
          input$start_dur, 
          if (input$use_end) paste0(" to ", input$end_dur, " days") else " days and above"
        ),
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      coord_sf(expand = FALSE)
    
    if (input$heatmap) {
      # Compute counts per area from filtered data
      area_counts <- Cd_filtered %>%
        group_by(`AREA NAME`) %>%
        summarise(count = n())
      
      # Join counts to boundary data
      boundary_counts <- left_join(boundary, area_counts, by = c("AREA_NAME" = "AREA NAME"))
      
      # Plot boundaries with fill based on counts
      base_plot <- base_plot +
        geom_sf(data = boundary_counts, aes(fill = count), color = NA) +
        scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
        guides(fill = guide_colorbar(title = "Crime Count"))
    } else {
      # Overlay boundaries without fill
      base_plot <- base_plot +
        geom_sf(data = boundary, fill = NA, color = "black")
    }
    
    # Add crime locations
    base_plot <- base_plot +
      geom_point(data = Cd_filtered, aes(x = LON, y = LAT), alpha = 0.5, size = 0.5)
    
    print(base_plot)
  })
  
  output$crimeStats <- renderText({
    Cd_filtered <- filteredData()
    
    total_crimes <- nrow(Cd)
    selected_crimes <- nrow(Cd_filtered)
    percentage <- round((selected_crimes / total_crimes) * 100, 2)
    
    paste0("Crime Count in Selected Interval: ", selected_crimes, "\n",
           "Percentage of Total Crimes: ", percentage, "%")
  })
  
  output$areaBarPlot <- renderPlot({
    Cd_filtered <- filteredData()
    
    total_area_counts <- Cd %>%
      group_by(`AREA NAME`) %>%
      summarise(total_count = n())
    
    selected_area_counts <- Cd_filtered %>%
      group_by(`AREA NAME`) %>%
      summarise(selected_count = n())
    
    area_data <- full_join(total_area_counts, selected_area_counts, by = "AREA NAME")
    area_data[is.na(area_data)] <- 0
    
    total_crimes <- sum(total_area_counts$total_count)
    selected_crimes <- sum(selected_area_counts$selected_count)
    
    area_data <- area_data %>%
      mutate(
        selected_percentage = (selected_count / selected_crimes) * 100,
        area_percentage = (selected_count / total_count) * 100
      )
    
    area_data_long <- area_data %>%
      select(`AREA NAME`, selected_percentage, area_percentage) %>%
      gather(key = "Metric", value = "Value", -`AREA NAME`)
    
    area_data_long$Metric <- recode(area_data_long$Metric,
                                    "selected_percentage" = "Percentage of Selected Crimes",
                                    "area_percentage" = "Percentage within Area")
    
    ggplot(area_data_long, aes(x = reorder(`AREA NAME`, -Value), y = Value, fill = Metric)) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(
        title = "Crime Percentages by Area",
        x = "Area Name",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)
