library(shiny)
source("~/EDA_final_proj/data_prep.R")

Geo.CD <- GeoData_Loader()
Mosaic.CD <- MosaicData_Loader()
crime_groups_choices <- Geo.CD$Crm.Cd.Group %>% unique() %>% sort()
AREA_NAME <- Mosaic.CD$`AREA NAME` %>% unique()

ui <- navbarPage(
  "Analysis of Crime Data in 2023, LA",
  # Page 1
  tabPanel(
    "Page 1",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "crime_types",
          label = "Select Crime Types:",
          choices = crime_groups_choices,
          selected = crime_groups_choices,
          multiple = TRUE
        ),
        selectInput(
          inputId = "weapon_usage",
          label = "Weapon Used:",
          choices = c("Yes", "No"),
          selected = c("Yes", "No"),
          multiple = TRUE
        ),
        selectInput(
          inputId = "severity",
          label = "Severity:",
          choices = c("Severe", "Less Severe"),
          selected = c("Severe", "Less Severe"),
          multiple = TRUE
        ),
        numericInput(
          inputId = "start_dur",
          label = "Start Point (Minimum Value of Duration Reported (Unit: days)):",
          value = 7,
          min = 0,
          step = 7
        ),
        checkboxInput(
          inputId = "use_end",
          label = "Select Option (End point)",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.use_end == true",
          numericInput(
            inputId = "end_dur",
            label = "End Point (Maximum Value of Duration Reported (Unit: days)):",
            value = 21,
            min = 0,
            step = 7
          )
        ),
        checkboxInput(
          inputId = "heatmap",
          label = "Show Heatmap",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.heatmap == true",
          radioButtons(inputId="choice", 
                       label="What would you like to see?", 
                       choices=c("Show Crime Count",
                                 "Show Crime Levels")
          )
        ),
        checkboxInput(
          inputId = "crime_percentage_plot_show",
          label = "Crime Percentages by Area",
          value = TRUE
        ),
        checkboxInput(
          inputId = "crime_dist_plot_show",
          label = "Distance Between Crime Location and Nearest Police Station",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.crime_dist_plot_show == true",
          div(
            style = "margin-left: 20px;",
            checkboxInput(
              inputId = "metric",
              label = "Calculate distance by L2 Metric (Default: L1 Metric)",
              value = FALSE
            )
          )
        )
      ),
      mainPanel(
        plotOutput("crimePlot", width = "100%", height = "600px"),
        textOutput("crimeStats"),
        plotOutput("optionPlot")
      )
    )
  ),
  
  # Page 2
  tabPanel(
    "Page 2",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "additionalplots",
          label = "Select Plots",
          choices = list(
            "Mosaic Plot" = "mosaic",
            "Piechart" = "pie"
          )
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'mosaic'",
          checkboxGroupInput(
            inputId = "selected_areas", 
            label = "Select Area(s):", 
            choices = AREA_NAME,
            selected = AREA_NAME
          ),
          actionButton("select_all", "Select All Areas"),
          actionButton("clear_selection", "Clear Selection")
        )
      ),
      mainPanel(
        plotOutput("additionalPlot1"),
        plotOutput("additionalPlot2"),
        plotOutput("additionalPlot3")
      )
    )
  ),
  
  # Page 3
  tabPanel(
    "Page 3",
    h3("Page 3 Content Goes Here"),
    # Add your content for Page 3 here
  )
)

server <- function(input, output, session) {
  ## PAGE 1
  observe({
    if (input$use_end && input$end_dur < input$start_dur) {
      updateNumericInput(session, "end_dur", value = input$start_dur)
    }
  })
  
  observeEvent(input$crime_percentage_plot_show, {
    if (input$crime_percentage_plot_show) {
      updateCheckboxInput(session, "crime_dist_plot_show", value = FALSE)
    }
  })
  
  observeEvent(input$crime_dist_plot_show, {
    if (input$crime_dist_plot_show) {
      updateCheckboxInput(session, "crime_percentage_plot_show", value = FALSE)
    }
  })
  
  filteredData <- reactive({
    start_value <- input$start_dur
    end_value <- if (input$use_end) input$end_dur else max(CD$`Dur Rptd`, na.rm = TRUE)
    
    Geo.CD %>%
      filter(`Dur Rptd` >= start_value & `Dur Rptd` <= end_value,
             Crm.Cd.Group %in% input$crime_types,
             `weapon_usage` %in% input$weapon_usage,
             Severity %in% input$severity)
  })
  
  output$crimePlot <- renderPlot({
    Cd_filtered <- filteredData()
    
    Title <- ifelse(input$heatmap, "Heatmap", "Scatterplot")
    
    base_plot <- ggplot() +
      labs(
        title = paste0(
          Title,
          " of Crime Locations for Duration Reported ",
          input$start_dur,
          if (input$use_end) paste0(" to ", input$end_dur, " Days") else " Days and Above"
        ),
        x = "Longitude",
        y = "Latitude",
      ) +
      theme_minimal() +
      coord_sf(expand = FALSE)
    
    if (input$heatmap) {
      if (input$choice == "Show Crime Count") {
        Total_count <- Cd_filtered %>% group_by(`AREA NAME`) %>% summarise(total_count = n())
        
        bb <- left_join(boundary, Total_count, by = c("APREC" = "AREA NAME"))
        base_plot <- base_plot +
          geom_sf(data = bb, aes(fill = total_count), color = "black") +
          scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
          guides(fill = guide_colorbar(title = "Crime Count"))
        
      } else if (input$choice == "Show Crime Levels") {
        Total_count <- Cd_filtered %>% 
          group_by(`AREA NAME`) %>% 
          summarise(total_count = n()) %>% .[order(-.$total_count), ] %>%
          mutate(group = rep(c("High", "Moderate", "Low"), each = 7))
        
        lev_colors <- brewer.pal(3, "Set1")
        names(lev_colors) <- unique(Total_count$group)
        
        bb <- left_join(boundary, Total_count, by = c("APREC" = "AREA NAME"))
        
        base_plot <- base_plot +
          geom_sf(data = bb, aes(fill = group), color = "black") +
          scale_fill_manual(name = "Group", values = lev_colors)
          
      }
      print(base_plot)
      
    } else {
      base_plot <- base_plot +
        geom_sf(data = boundary, fill = NA, color = "black") +
        geom_point(data = Cd_filtered, aes(x = LON, y = LAT), alpha = 0.5, size = 0.5)
      print(base_plot)
    }
  })
  
  output$crimeStats <- renderText({
    Cd_filtered <- filteredData()
    
    total_crimes <- nrow(CD)
    selected_crimes <- nrow(Cd_filtered)
    percentage <- round((selected_crimes / total_crimes) * 100, 2)
    
    paste0(
      "Crime Count in Selected Interval: ", selected_crimes, "\n\n",
      "Percentage of Total Crimes: ", percentage, "%"
    )
  })
  
  output$optionPlot <- renderPlot({
    Cd_filtered <- filteredData()
    
    if (input$crime_percentage_plot_show) {
      total_area_counts <- CD %>%
        group_by(`AREA NAME`) %>%
        summarise(total_count = n())
      
      selected_area_counts <- Cd_filtered %>%
        group_by(`AREA NAME`) %>%
        summarise(selected_count = n())
      
      area_data <- full_join(total_area_counts, selected_area_counts, by = "AREA NAME")
      area_data[is.na(area_data)] <- 0
      
      summed <- apply(area_data %>% .[, c(2, 3)], 2, sum)
      
      total_crimes <- summed[1]
      selected_crimes <- summed[2]
      
      area_data <- area_data %>%
        mutate(
          selected_percentage = (selected_count / selected_crimes) * 100,
          area_percentage = (selected_count / total_count) * 100
        )
      
      if (input$start_dur == 0) {area_data$area_percentage =0}
      
      area_data_long <- area_data %>%
        select(`AREA NAME`, selected_percentage, area_percentage) %>%
        gather(key = "Metric", value = "Value", -`AREA NAME`)
      
      area_data_long$Metric <- recode(
        area_data_long$Metric,
        "selected_percentage" = "Percentage of Selected Crimes",
        "area_percentage" = "Percentage within Area"
      )
      areaplot <- ggplot(area_data_long, aes(x = reorder(`AREA NAME`, -Value), y = Value, fill = Metric)) +
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
      print(areaplot)
    } else {
      if (input$metric) {
        Distplot <- Cd_filtered %>% ggplot(., aes(x = L2_dist)) +
          geom_density()
      } else {
        Distplot <- Cd_filtered %>% ggplot(., aes(x = L1_dist)) +
          geom_density()
      }
      print(Distplot)
    }
  })
  
  ## PAGE 2 ##
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_areas", 
                             selected = AREA_NAME)
  })
  
  observeEvent(input$clear_selection, {
    updateCheckboxGroupInput(session, "selected_areas", 
                             selected = character(0))
  })
  filteredData.Page2 <- reactive({
    if (length(input$selected_areas) == 0) {
      return(Mosaic.CD)
    } else {
      return(Mosaic.CD %>% data.table %>% .[`AREA NAME` %in% input$selected_areas])
    }
  })
  
  output$additionalPlot1 <- renderPlot({
    if (input$additionalplots == "mosaic") {
      vict_sex_data <- filteredData.Page2()[!is.na(vict_sex)] %>% .[, vict_sex := as.factor(vict_sex)]
      
      plot <- ggplot(data = vict_sex_data) +
        geom_mosaic(aes(x = product(Severity, vict_sex), 
                        fill = Severity)) +
        scale_fill_manual(values = c("turquoise3", "orange2")) +
        labs(x = "Victim Sex", 
             y = "Severity", 
             title = ifelse(
               length(input$selected_areas) == length(AREA_NAME), 
               "Victim Sex vs Severity in All Areas", 
               paste("Victim Sex vs Severity in", paste(input$selected_areas, collapse = ", "))
             )) +
        theme_mosaic()
    }
    print(plot)
  })
  
  output$additionalPlot2 <- renderPlot({
    plot <- ggplot()
    if (input$additionalplots == "mosaic") {
      weapon_usage_data <- filteredData.Page2() %>% .[, weapon_usage := as.factor(weapon_usage)]
      
      plot <- ggplot(data = weapon_usage_data) +
        geom_mosaic(aes(x = product(Severity, weapon_usage), 
                        fill = Severity)) +
        scale_fill_manual(values = c("turquoise3", "orange2")) +
        labs(x = "Weapon Usage", 
             y = "Severity", 
             title = ifelse(
               length(input$selected_areas) == length(AREA_NAME), 
               "Weapon Usage vs Severity in All Areas", 
               paste("Weapon Usage vs Severity in", paste(input$selected_areas, collapse = ", "))
             )) +
        theme_mosaic()
    }
    print(plot)
  })
  
  output$additionalPlot3 <- renderPlot({
    plot <- ggplot()
    if (input$additionalplots == "mosaic") {
      crime_status_data <- filteredData.Page2()[!is.na(crime_status)] %>% .[, crime_status := as.factor(crime_status)]
      
      plot <- ggplot(data = crime_status_data) +
        geom_mosaic(aes(x = product(Severity, crime_status), 
                        fill = Severity)) +
        scale_fill_manual(values = c("turquoise3", "orange2")) +
        labs(x = "Crime Status", 
             y = "Severity", 
             title = ifelse(
               length(input$selected_areas) == length(AREA_NAME), 
               "Crime Status vs Severity in All Areas", 
               paste("Crime Status vs Severity in", paste(input$selected_areas, collapse = ", "))
             )) +
        theme_mosaic()
      
    }
    print(plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
