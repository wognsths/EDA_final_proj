library(shiny)
source("~/EDA_final_proj/data_prep.R")

Geo.CD <- GeoData_Loader()
Mosaic.CD <- MosaicData_Loader()
crime_groups_choices <- Geo.CD$Crm.Cd.Group %>% unique() %>% sort()
AREA_NAME <- Mosaic.CD$`AREA NAME` %>% unique()
DEC <- Geo.CD$`vict_descent` %>% unique() %>% .[-1]

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
          inputId = "select_descent",
          label = "Select Descent:",
          choices = DEC,
          selected = DEC,
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
        conditionalPanel(
          condition = "input.crime_percentage_plot_show == true",
          div(
            style = "margin-left: 20px;",
            checkboxInput(
              inputId = "refuse_area",
              label = "I don't want to see percentage within area",
              value = FALSE
            )
          )
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
        ),
        br(),
        actionButton("capture", "Capture"),
        actionButton("reset", "Reset")
      ),
      mainPanel(
        uiOutput("crimePlots", width = "100%", height = "600px"),
        htmlOutput("crimeStats"),
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
            "rptd dur vs distance by crime" = "crime",
            "Density Plot (Duration of Reported)" = "density"
          )
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'mosaic'",
          selectInput(
            inputId = "selected_areas",
            label = "Select Area(s):",
            choices = AREA_NAME,
            selected = AREA_NAME,
            multiple = TRUE
          ),
          selectInput(
            inputId = "selected_plot",
            label = "Select Plot:",
            choices = list(
              "Victim Sex vs Severity" = "plot1",
              "Weapon Usage vs Severity" = "plot2",
              "Crime Status vs Severity" = "plot3",
              "Victim Age Group vs Severity" = "plot4",
              "Victim Descent vs Severity" = "plot5",
              "Duration of Reported vs Severity" = "plot6"
            ),
            selected = "plot1"
          )
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'crime'",
          selectInput(
            inputId = "select_crime",
            label = "Select Crime Type",
            choices = crime_groups_choices,
            selected = crime_groups_choices[1],
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'density'",
          selectInput(
            inputId = "dist_metric",
            label = "Select Distance Metric",
            choices = list("L1 distance" = "L1_dist",
                           "L2 distance" = "L2_dist"),
            selected = "L1 distance",
            multiple = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'density'",
          selectInput(
            inputId = "selected_areas",
            label = "Select Area(s):",
            choices = AREA_NAME,
            selected = AREA_NAME,
            multiple = TRUE
          ),
          numericInput(
            inputId = "start_point",
            label = "Start Point (Minimum Value of Duration Reported (Unit: days)):",
            value = 7,
            min = 0,
            step = 7
          ),
          numericInput(
            inputId = "end_point",
            label = "End Point (Maximum Value of Duration Reported (Unit: days)):",
            value = 21,
            min = 0,
            step = 7
          ),
          checkboxInput(
            inputId = "log_scale",
            label = "Scale by Log (10)",
            value = FALSE
          ),
          selectInput(
            inputId = "selected_category",
            label = "Select Categories",
            choices = list(
              "Victim Sex" = "sex",
              "Victim Descent" = "descent",
              "Victim Age Group" = "age",
              "Area" = "area"
            )
          ),
          conditionalPanel(
            condition = "input.selected_category == 'sex'",
            selectInput(
              inputId = "subset_sex",
              label = "Select Categories",
              choices = list(
                "Male" = "M",
                "Female" = "F"
              ),
              selected = list(
                "Male" = "M",
                "Female" = "F"
              ),
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.selected_category == 'descent'",
            selectInput(
              inputId = "subset_descent",
              label = "Select Categories",
              choices = Mosaic.CD$vict_descent %>% unique() %>% .[-1],
              selected = Mosaic.CD$vict_descent %>% unique() %>% .[-1],
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.selected_category == 'age'",
            selectInput(
              inputId = "subset_age",
              label = "Select Categories",
              choices = c("Youth (0~14)",
                          "Adult (15~64)",
                          "Elderly (65~)"),
              selected = c("Youth (0~14)",
                           "Adult (15~64)",
                           "Elderly (65~)"),
              multiple = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.selected_category == 'area'"
          ),
          selectInput(
            inputId = "select_metric",
            label = "Select Metric",
            choices = list(
              "Density Plot" = "density",
              "Ecdf" = "ecdf",
              "Histogram (Position: fill)" = "hist_fill"
            )
          )
        ),
      ),
      mainPanel(
        plotOutput("additionalPlot")
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
  
  capturedPlot <- reactiveVal(NULL)
  
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
             Severity %in% input$severity,
             vict_descent %in% input$select_descent)
  })
  
  currentPlot <- reactive({
    Cd_filtered <- filteredData()
    
    base_plot <- ggplot() +
      labs(
        title = paste0(
          "Crime Locations for Duration Reported ",
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
    } else {
      base_plot <- base_plot +
        geom_sf(data = boundary, fill = NA, color = "black") +
        geom_point(data = Cd_filtered, aes(x = LON, y = LAT), alpha = 0.5, size = 0.5)
    }
    
    base_plot
  })
  
  output$crimePlot <- renderPlot({
    print(currentPlot())
  })
  
  observeEvent(input$capture, {
    capturedPlot(currentPlot())
    showNotification("Current plot has been captured", type = "message")
  })
  
  output$crimePlots <- renderUI({
    if (!is.null(capturedPlot())) {
      tagList(
        column(6, 
               tags$h4(style = "text-align: center; font-weight: bold;", "Previous Plot"),
               plotOutput("capturedCrimePlot", height = "600px", width = "100%")),
        column(6,
               tags$h4(style = "text-align: center; font-weight: bold;", "Current Plot"),
               plotOutput("crimePlot", height = "600px", width = "100%"))
      )
    } else {
      tagList(
        tags$h4(style = "text-align: center; font-weight: bold;", "Current Plot"),
        plotOutput("crimePlot", height = "600px", width = "100%")
              )
    }
  })
  
  output$capturedCrimePlot <- renderPlot({
    req(capturedPlot())
    print(capturedPlot())
  })
  
  observeEvent(input$reset, {
    capturedPlot(NULL)
    showNotification("Captured plot has been deleted", type = "warning")
  })
  
  output$crimeStats <- renderUI({
    Cd_filtered <- filteredData()
    
    total_crimes <- nrow(CD)
    selected_crimes <- nrow(Cd_filtered)
    percentage <- round((selected_crimes / total_crimes) * 100, 2)
    
    HTML(paste0(
      "Crime Count in Selected Interval: ", selected_crimes, "<br>",
      "Percentage of Total Crimes: ", percentage, "%"
    ))
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
      
      if (input$refuse_area == TRUE) {area_data$area_percentage =0}
      
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
  
  filteredData.Page2 <- reactive({
    data <- Mosaic.CD %>% data.table
    if (length(input$selected_areas) > 0) {
      data <- data[`AREA NAME` %in% input$selected_areas]
    }
    return(data)
  })
  output$additionalPlot <- renderPlot({
    if (input$additionalplots == "mosaic") {
      data <- filteredData.Page2()
      plot <- NULL
      
      if (input$selected_plot == "plot1") {
        vict_sex_data <- data[!is.na(vict_sex)]
        vict_sex_data[, vict_sex := as.factor(vict_sex)]
        
        plot <- ggplot(data = vict_sex_data) +
          geom_mosaic(aes(x = product(Severity, vict_sex), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Sex", y = "Severity", title = "Victim Sex vs Severity") +
          theme_mosaic()
        
      } else if (input$selected_plot == "plot2") {
        weapon_usage_data <- data
        weapon_usage_data[, weapon_usage := as.factor(weapon_usage)]
        
        plot <- ggplot(data = weapon_usage_data) +
          geom_mosaic(aes(x = product(Severity, weapon_usage), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Weapon Usage", y = "Severity", title = "Weapon Usage vs Severity") +
          theme_mosaic()
        
      } else if (input$selected_plot == "plot3") {
        crime_status_data <- data[!is.na(crime_status)]
        crime_status_data[, crime_status := as.factor(crime_status)]
        
        plot <- ggplot(data = crime_status_data) +
          geom_mosaic(aes(x = product(Severity, crime_status), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Crime Status", y = "Severity", title = "Crime Status vs Severity") +
          theme_mosaic()
        
      } else if (input$selected_plot == "plot4") {
        vict_age_data <- data[!is.na(vict_age)]
        vict_age_data[, vict_age := factor(vict_age, levels = c("Youth (0~14)", "Adult (15~64)", "Elderly (65~)"))]
        
        plot <- ggplot(data = vict_age_data) +
          geom_mosaic(aes(x = product(Severity, vict_age), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Age Group", y = "Severity", title = "Victim Age Group vs Crime Severity") +
          theme_mosaic()
        
      } else if (input$selected_plot == "plot5") {
        vict_descent_data <- data[!is.na(vict_descent)]
        vict_descent_data[, vict_descent := as.factor(vict_descent)]
        
        plot <- ggplot(data = vict_descent_data) +
          geom_mosaic(aes(x = product(Severity, vict_descent), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Descent", y = "Severity", title = "Victim Descent vs Crime Severity") +
          theme_mosaic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if (input$selected_plot == "plot6") {
        cat_dur_rptd_data <- data[!is.na(cat_dur_rptd)]
        cat_dur_rptd_data[, cat_dur_rptd := as.factor(cat_dur_rptd)]
        
        plot <- ggplot(data = cat_dur_rptd_data) +
          geom_mosaic(aes(x = product(Severity, cat_dur_rptd), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Duration of Reported", y = "Severity", title = "Duration of Reported vs Crime Severity") +
          theme_mosaic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      if (!is.null(plot)) {
        print(plot)
      }
      
    } else if (input$additionalplots == "crime") {
      data <- filteredData() %>%
        select(., c(Crm.Cd.Group, `Dur Rptd`, input$dist_metric)) %>%
        filter(., Crm.Cd.Group == input$select_crime)
      
      plot <- ggplot(data, aes(x = .data[[input$dist_metric]], y = `Dur Rptd`)) +
        geom_point(alpha = 0.6, size = 0.7)
      
      if (!is.null(plot)) {
        print(plot)
      }
      
    } else if (input$additionalplots == "density") {
      data <- filteredData.Page2()
      
      data <- data[`Dur Rptd` >= input$start_point]
      data <- data[`Dur Rptd` <= input$end_point]
      
      if (input$log_scale) {
        data <- data %>%
          mutate(`Dur Rptd` = `Dur Rptd` + 1) %>%
          mutate(`Dur Rptd` = log10(`Dur Rptd`))
      }
      
      if (input$selected_category == "sex") {
        data <- data[vict_sex %in% input$subset_sex]
      } else if (input$selected_category == "descent") {
        data <- data[vict_descent %in% input$subset_descent]
      } else if (input$selected_category == "age") {
        data <- data[vict_age %in% input$subset_age] 
      } else if (input$selected_category == "area") {
        ### No need subset
      }
      
      category_var <- switch(input$selected_category,
                             'sex' = 'vict_sex',
                             'descent' = 'vict_descent',
                             'age' = 'vict_age',
                             'area' = 'AREA NAME')
      if (!category_var %in% names(data)) {
        stop("Invalid category variable selected.")
      }
      
      data[, category := factor(get(category_var))]
      
      if (input$select_metric == 'density') {
        plot <- ggplot(data, aes(x = `Dur Rptd`, color = category)) +
          geom_density() +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'Density',
               color = input$selected_category,
               title = 'Density Plot of Reporting Delay by Category') +
          theme_minimal()
      } else if (input$select_metric == 'ecdf') {
        plot <- ggplot(data, aes(x = `Dur Rptd`, color = category)) +
          stat_ecdf() +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'ECDF',
               color = input$selected_category,
               title = 'ECDF of Reporting Delay by Category') +
          theme_minimal()
      } else if (input$select_metric == "hist_fill") {
        plot <- ggplot(data, aes(x = `Dur Rptd`, fill = category)) +
          geom_histogram(position = "fill") +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'Ratio',
               color = input$selected_category,
               title = 'Ratio of Reporting Delay by Category') +
          theme_minimal()
      }
      print(plot)
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)