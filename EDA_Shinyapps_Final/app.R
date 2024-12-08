library(shiny);library(plotly);library(randomForest);library(xgboost);library(nnet);library(caret)
source("~/EDA_final_proj/data_prep.R")

Geo.CD <- GeoData_Loader()
Mosaic.CD <- MosaicData_Loader()
crime_groups_choices <- Geo.CD$Crm.Cd.Group %>% unique() %>% sort()
AREA_NAME <- Mosaic.CD$`AREA NAME` %>% unique()
DEC <- Geo.CD$`vict_descent` %>% unique()

time_choices <- c("Morning", "Afternoon", "Night", "Late Night")
vict_age_choices <- c("Youth (0~14)", "Adult (15~64)", "Elderly (65~)")
vict_sex_choices <- c("M", "F")

multi_logit_model <- readRDS("multi_logit_model.rds")
xgb_model <- readRDS("xgb_model.rds")
rf_model <- readRDS("rf_model.rds")
train_data <- CD %>%
  select(Crm.Cd.Group, `AREA NAME`, vict_age, vict_sex, vict_descent, period) %>%
  rename(AREA_NAME = `AREA NAME`)
train_data$AREA_NAME <- as.factor(train_data$AREA_NAME)
train_data$vict_age <- as.factor(train_data$vict_age)
train_data$vict_sex <- as.factor(train_data$vict_sex)
train_data$vict_descent <- as.factor(train_data$vict_descent)
train_data$period <- as.factor(train_data$period)
train_data$Crm.Cd.Group <- as.factor(train_data$Crm.Cd.Group)

factor_choices <- list(
  "Median Income" = "median_household_income",
  "Unemployment Rate" = "unemployment",
  "Poverty Gap" = "gap"
)

ui <- navbarPage(
  "Analysis of Crime Data in 2023, LA",
  # Page 1
  tabPanel(
    "Geospatial Crime Explorer 🔍",
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
          value = 0,
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
            ),
            checkboxInput(
              inputId = "refuse_selected",
              label = "I don't want to see percentage of selected crimes",
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
        checkboxInput(inputId = "dist_summary",
                      label = "Duration of Reported vs. Distance to Police Station",
                      value = FALSE),
        br(),
        actionButton("capture", "Capture"),
        actionButton("reset", "Reset")
      ),
      mainPanel(
        tags$h3("Explore Crime Across LA"),
        tags$p("Use the filters on the left to visualize crime occurrence, intensity, and distribution 
                across different neighborhoods and conditions."),
        uiOutput("crimePlots", width = "100%", height = "600px"),
        htmlOutput("crimeStats"),
        plotOutput("optionPlot"),
        br(),
        conditionalPanel(
          condition = "input.dist_summary == true",
          tableOutput("dist_summary_table")
        )
      )
    )
  ),
  
  # Page 2
  tabPanel(
    "In-depth Analytics & Patterns 📊",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "additionalplots",
          label = "Select Plots",
          choices = list(
            "Mosaic Plot" = "mosaic",
            "Duration of Reported vs Distance Between Police Station" = "crime",
            "Time-Based Crime Analysis" = "time_crime",
            "Density Plot (Duration of Reported)" = "density",
            "Crime Proportions by Time" = "crime_proportion",
            "Crime and Socioeconomic Factors" = "crime_socioeco"
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
          condition = "input.additionalplots == 'time_crime'",
          radioButtons(
            inputId = "time_div",
            label = "Select Option",
            choices = list(
              "View by Time of Day (Morning, Afternoon, Evening, Late Night)" = "view_time",
              "View by Hour" = "view_hour"
            )
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
            value = 0,
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
        conditionalPanel(
          condition = "input.additionalplots == 'crime_proportion'",
          selectInput(
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
        conditionalPanel(
          condition = "input.additionalplots == 'crime_socioeco'",
          selectInput(
            inputId = "factor_choice",
            label = "Select Factor for Comparison",
            choices = list(
              "Median Income" = "median_household_income",
              "Unemployment Rate" = "unemployment",
              "Poverty Gap" = "gap"
            )
          )
        )
      ),
      mainPanel(
        tags$h3("Explore Temporal and Socioeconomic Crime Patterns"),
        tags$p("Dive deeper into how crime relates to time, demographic factors, and socio-economic conditions."),
        
        plotOutput("additionalPlot"),
        conditionalPanel(
          condition = "input.additionalplots == 'mosaic'",
          verbatimTextOutput("contingencyTableOutput"),
          verbatimTextOutput("chiSquareOutput")
        ),
        conditionalPanel(
          condition = "input.additionalplots == 'crime'",
          br(),
          uiOutput("dist_summary_table_L1_page2"),
          uiOutput("dist_summary_table_Dur_page2")
        ),
        # input$select_metric == 'ecdf'
        conditionalPanel(
          condition = "input.additionalplots == 'density' & input.select_metric == 'ecdf'",
          br(),
          tableOutput("summaryTable")
        ),
        plotOutput("additionalPlot2")
      )
    )
  ),
  tabPanel(
    "Predictive Modeling🔮",
    sidebarLayout(
      sidebarPanel(
        selectInput("model_age", "Select Victim Age Group:", choices = vict_age_choices),
        selectInput("model_sex", "Select Victim Sex:", choices = vict_sex_choices),
        selectInput("model_descent", "Select Victim Descent:", choices = DEC),
        selectInput("model_period", "Select Period:", choices = time_choices),
        actionButton("predict_btn", "Predict"),
        br(),
        conditionalPanel(
          condition = "output.predictionReady",
          radioButtons("select_model_vis", "Select Model for Visualization:", 
                       choices = c("Multinomial_Logit", "Random_Forest", "XGBoost"),
                       selected = "Multinomial_Logit")
        )
      ),
      mainPanel(
        tags$h3("Crime Prediction by Modeling"),
        tags$p("Apply advanced models to predict the likelihood of various crime types based on 
                victim and time characteristics. Adjust parameters and visualize outcomes below."),
        fluidRow(
          column(
            width = 6,
            tags$div(
              style = "width:100%; margin:auto;",
              plotlyOutput("map", height = "500px")
            )
          ),
          column(
            width = 6,
            plotOutput("predictionChart", height = "500px")
          )
        ),
        h4("Selected Area Info"),
        verbatimTextOutput("clickInfo"),
        br(),
        h4("Crime Prediction Table"),
        uiOutput("predictionTable1"),
        uiOutput("predictionTable2")
      )
    )
  )
)

server <- function(input, output, session) {
  ## PAGE 1
  
  capturedPlot <- reactiveVal(NULL)
  
  observe({
  req(input$use_end, input$end_dur, input$start_dur)
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
  print(filteredData %>% dim())
  
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
      
      if (input$refuse_selected == TRUE) {area_data$selected_percentage =0}
      
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
  
  observeEvent(input$dist_summary, {
    if (!input$dist_summary) return(NULL)
    
    Usedata <- filteredData() %>% select(., c(`Dur Rptd`, L2_dist, L1_dist))

    dist_col <- "L1_dist"

    bins <- list(
      "0~6 days" = Usedata %>% filter(`Dur Rptd` >= 0 & `Dur Rptd` <= 6),
      "7~49 days" = Usedata %>% filter(`Dur Rptd` >= 7 & `Dur Rptd` <= 49),
      "50~100 days" = Usedata %>% filter(`Dur Rptd` >= 50 & `Dur Rptd` <= 100),
      "101~365 days" = Usedata %>% filter(`Dur Rptd` >= 101 & `Dur Rptd` <= 365),
      "365+ days" = Usedata %>% filter(`Dur Rptd` > 365)
    )
    
    get_mode <- function(x) {
      tb <- table(x)
      mode_val <- names(tb)[which.max(tb)]
      return(as.numeric(mode_val))
    }

    summary_list <- lapply(names(bins), function(range_name) {
      data_subset <- bins[[range_name]][[dist_col]]
      if (length(data_subset) == 0) {
        c(Min = NA, `1st Qu.` = NA, Median = NA, Mean = NA, `3rd Qu.` = NA, Max = NA, Mode = NA)
      } else {
        binned_dist <- floor(data_subset / 0.01) * 0.01
        s <- summary(binned_dist)
        md <- get_mode(binned_dist)

        c(Min = s["Min."],
          `1st Qu.` = s["1st Qu."],
          Median = s["Median"],
          Mean = s["Mean"],
          `3rd Qu.` = s["3rd Qu."],
          Max = s["Max."],
          Mode = md)
      }
    })
    
    summary_df <- do.call(rbind, summary_list)
    rownames(summary_df) <- names(bins)
    colnames(summary_df) <- c("Min", "1st Quantile", "Median", "Mean", "3rd Quantile", "Max", "Mode")
    
    output$dist_summary_table <- renderTable({
      summary_df
    }, rownames = TRUE)
  })
  
  ## PAGE 2 ##
  
  filteredData.Page2 <- reactive({
    data <- Mosaic.CD %>% data.table
    if (length(input$selected_areas) > 0) {
      data <- data[`AREA NAME` %in% input$selected_areas]
    }
    return(data)
  })
  
  filteredData.Page2.1 <- reactive({
    data <- Geo.CD %>% select(., c(period, Crm.Cd.Group, `Dur Rptd`, L1_dist, L2_dist))
  })
  
  output$additionalPlot <- renderPlot({
    if (input$additionalplots == "mosaic") {
      data <- filteredData.Page2()
      plot <- NULL
      
      if (input$selected_plot == "plot1") {
        vict_sex_data <- data[!is.na(vict_sex)]
        vict_sex_data[, vict_sex := as.factor(vict_sex)]
        
        tbl <- table(vict_sex_data$Severity, vict_sex_data$vict_sex)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = vict_sex_data) +
          geom_mosaic(aes(x = product(Severity, vict_sex), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Sex", y = "Severity", title = "Victim Sex vs Severity") +
          theme_mosaic()
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
      } else if (input$selected_plot == "plot2") {
        weapon_usage_data <- data
        weapon_usage_data[, weapon_usage := as.factor(weapon_usage)]
        
        tbl <- table(weapon_usage_data$Severity, weapon_usage_data$weapon_usage)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = weapon_usage_data) +
          geom_mosaic(aes(x = product(Severity, weapon_usage), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Weapon Usage", y = "Severity", title = "Weapon Usage vs Severity") +
          theme_mosaic()
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
      } else if (input$selected_plot == "plot3") {
        crime_status_data <- data[!is.na(crime_status)]
        crime_status_data[, crime_status := as.factor(crime_status)]
        
        tbl <- table(crime_status_data$Severity, crime_status_data$crime_status)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = crime_status_data) +
          geom_mosaic(aes(x = product(Severity, crime_status), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Crime Status", y = "Severity", title = "Crime Status vs Severity") +
          theme_mosaic()
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
      } else if (input$selected_plot == "plot4") {
        vict_age_data <- data[!is.na(vict_age)]
        vict_age_data[, vict_age := factor(vict_age, levels = c("Youth (0~14)", "Adult (15~64)", "Elderly (65~)"))]
        
        tbl <- table(vict_age_data$Severity, vict_age_data$vict_age)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = vict_age_data) +
          geom_mosaic(aes(x = product(Severity, vict_age), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Age Group", y = "Severity", title = "Victim Age Group vs Crime Severity") +
          theme_mosaic()
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
      } else if (input$selected_plot == "plot5") {
        vict_descent_data <- data[!is.na(vict_descent) & vict_descent != "Descent is not Specified"]
        vict_descent_data[, vict_descent := as.factor(vict_descent)]
        
        tbl <- table(vict_descent_data$Severity, vict_descent_data$vict_descent)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = vict_descent_data) +
          geom_mosaic(aes(x = product(Severity, vict_descent), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Victim Descent", y = "Severity", title = "Victim Descent vs Crime Severity") +
          theme_mosaic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
        
      } else if (input$selected_plot == "plot6") {
        cat_dur_rptd_data <- data[!is.na(cat_dur_rptd)]
        cat_dur_rptd_data[, cat_dur_rptd := as.factor(cat_dur_rptd)]
        
        tbl <- table(cat_dur_rptd_data$Severity, cat_dur_rptd_data$cat_dur_rptd)
        tbl_with_margins <- addmargins(tbl, FUN = list(Total = sum))
        
        plot <- ggplot(data = cat_dur_rptd_data) +
          geom_mosaic(aes(x = product(Severity, cat_dur_rptd), fill = Severity)) +
          scale_fill_manual(values = c("turquoise3", "orange2")) +
          labs(x = "Duration of Reported", y = "Severity", title = "Duration of Reported vs Crime Severity") +
          theme_mosaic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
        output$contingencyTableOutput <- renderPrint({
          cat("Contingency Table:\n")
          print(tbl_with_margins)
        })
        
        output$chiSquareOutput <- renderPrint({
          cat("Chi-squared Test:\n")
          chisq_result <- chisq.test(tbl)
          print(chisq_result)
        })
        
      }
      
      if (!is.null(plot)) {
        print(plot)
      }
      
    } else if (input$additionalplots == "crime") {
      get_mode <- function(x) {
        tb <- table(x)
        mode_val <- names(tb)[which.max(tb)]
        return(as.numeric(mode_val))
      }
      
      dist_col <- input$dist_metric

      all_data <- filteredData.Page2.1() %>%
        select(Crm.Cd.Group, `Dur Rptd`, L1_dist, L2_dist)
      
      crimes <- unique(all_data$Crm.Cd.Group)
      
      # 범죄별 L1 Dist Summary
      L1_summary_list <- lapply(crimes, function(crime) {
        data_subset <- all_data %>% filter(Crm.Cd.Group == crime) %>% pull(L1_dist)
        if (length(data_subset) == 0) {
          c(Min = NA, `1st Quantile` = NA, Median = NA, Mean = NA, `3rd Quantile` = NA, Max = NA, Mode = NA)
        } else {
          binned_L1 <- floor(data_subset / 0.02) * 0.02
          s_L1 <- summary(binned_L1)
          md_L1 <- get_mode(binned_L1)
          c(Min = s_L1["Min."],
            `1st Quantile` = s_L1["1st Qu."],
            Median = s_L1["Median"],
            Mean = s_L1["Mean"],
            `3rd Quantile` = s_L1["3rd Qu."],
            Max = s_L1["Max."],
            Mode = md_L1)
        }
      })
      L1_summary_df <- do.call(rbind, L1_summary_list)
      rownames(L1_summary_df) <- crimes
      colnames(L1_summary_df) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Mode")

      Dur_summary_list <- lapply(crimes, function(crime) {
        data_subset <- all_data %>% filter(Crm.Cd.Group == crime) %>% pull(`Dur Rptd`)
        if (length(data_subset) == 0) {
          c(Min = NA, `1st Quantile` = NA, Median = NA, Mean = NA, `3rd Quantile` = NA, Max = NA, Mode = NA)
        } else {
          s_Dur <- summary(data_subset)
          md_Dur <- get_mode(data_subset)
          c(Min = s_Dur["Min."],
            `1st Quantile` = s_Dur["1st Qu."],
            Median = s_Dur["Median"],
            Mean = s_Dur["Mean"],
            `3rd Quantile` = s_Dur["3rd Qu."],
            Max = s_Dur["Max."],
            Mode = md_Dur)
        }
      })
      Dur_summary_df <- do.call(rbind, Dur_summary_list)
      rownames(Dur_summary_df) <- crimes
      colnames(Dur_summary_df) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Mode")

      selected_crime <- input$select_crime

      output$dist_summary_table_L1_page2 <- renderUI({
        L1_df <- as.data.frame(L1_summary_df) %>%
          tibble::rownames_to_column("Crime")

        L1_html <- L1_df %>%
          kbl(format = "html", caption = "L1 Distance Summary for All Crimes") %>%
          kable_styling(full_width = FALSE) %>%
          row_spec(which(L1_df$Crime == selected_crime), background = "yellow")

        tagList(
          HTML(L1_html), 
          HTML("<p>This table shows summary statistics (Min, Quantiles, Median, Mean, Max, Mode) for L1 distances. The selected crime is highlighted.</p>")
        )
      })

      output$dist_summary_table_Dur_page2 <- renderUI({
        Dur_df <- as.data.frame(Dur_summary_df) %>%
          tibble::rownames_to_column("Crime")
        
        Dur_html <- Dur_df %>%
          kbl(format = "html", caption = "Duration of Reported Summary for All Crimes") %>%
          kable_styling(full_width = FALSE) %>%
          row_spec(which(Dur_df$Crime == selected_crime), background = "yellow")
        
        tagList(
          HTML(Dur_html),
          HTML("<p>This table shows summary statistics for Duration of Reported. The selected crime is highlighted.</p>")
        )
      })

      data <- all_data %>%
        filter(Crm.Cd.Group == selected_crime)
      
      plot <- ggplot(data, aes(x = .data[[dist_col]], y = `Dur Rptd`)) +
        geom_point(alpha = 0.6, size = 0.7) +
        labs(
          title = paste("Duration Reported vs", dist_col, "for", selected_crime),
          x = dist_col,
          y = "Duration of Reported (days)"
        ) +
        theme_minimal()
      
      print(plot)
      
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
        ddd <- filteredData.Page2()

        if (input$selected_category == "sex") {
          ddd <- ddd[vict_sex %in% input$subset_sex]
          category_var <- "vict_sex"
        } else if (input$selected_category == "descent") {
          ddd <- ddd[vict_descent %in% input$subset_descent]
          category_var <- "vict_descent"
        } else if (input$selected_category == "age") {
          ddd <- ddd[vict_age %in% input$subset_age]
          category_var <- "vict_age"
        } else if (input$selected_category == "area") {
          category_var <- "AREA NAME"
        }

        ddd <- ddd %>%
          mutate(Dur_bins = case_when(
            `Dur Rptd` == 0 ~ "0 day",
            `Dur Rptd` == 1 ~ "1 day",
            `Dur Rptd` == 2 ~ "2 day",
            `Dur Rptd` == 3 ~ "3 day",
            `Dur Rptd` == 4 ~ "4 day",
            `Dur Rptd` == 5 ~ "5 day",
            `Dur Rptd` == 6 ~ "6 day",
            `Dur Rptd` == 7 ~ "7 day",
            `Dur Rptd` >= 8 & `Dur Rptd` <= 49 ~ "8~49 day",
            `Dur Rptd` >= 50 & `Dur Rptd` <= 100 ~ "50~100 day",
            `Dur Rptd` >= 101 & `Dur Rptd` <= 365 ~ "101~365 day",
            `Dur Rptd` > 365 ~ "365+ day"
          ))
        
        ddd$Dur_bins <- factor(ddd$Dur_bins,
                               levels = c("0 day","1 day","2 day","3 day","4 day","5 day","6 day","7 day",
                                          "8~49 day","50~100 day","101~365 day","365+ day"))
        
        ddd[, category := factor(get(category_var))]

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
        }
        
        data[, category := factor(get(category_var))]
        
        # ECDF plot
        plot <- ggplot(data, aes(x = `Dur Rptd`, color = category)) +
          stat_ecdf() +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'ECDF',
               color = input$selected_category,
               title = 'ECDF of Reporting Delay by Category') +
          theme_minimal()
        
        print(plot)
        
        freq_table <- ddd %>%
          group_by(category, Dur_bins) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(category) %>%
          arrange(Dur_bins, .by_group = TRUE) %>%
          mutate(cum_count = cumsum(count),
                 total_count = sum(count),
                 cum_ratio = cum_count / total_count) %>%
          ungroup() %>%
          select(category, Dur_bins, cum_ratio) %>%
          tidyr::pivot_wider(names_from = Dur_bins, values_from = cum_ratio, values_fill = 0)
        
        output$summaryTable <- renderTable({
          freq_table
        })
      }
      else if (input$select_metric == "hist_fill") {
        plot <- ggplot(data, aes(x = `Dur Rptd`, fill = category)) +
          geom_histogram(position = "fill") +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'Ratio',
               color = input$selected_category,
               title = 'Ratio of Reporting Delay by Category') +
          theme_minimal()
        print(plot)
      }
      else if (input$select_metric == "hist_fill") {
        plot <- ggplot(data, aes(x = `Dur Rptd`, fill = category)) +
          geom_histogram(position = "fill") +
          labs(x = 'Duration between Occurrence and Report Date',
               y = 'Ratio',
               color = input$selected_category,
               title = 'Ratio of Reporting Delay by Category') +
          theme_minimal()
      }
      print(plot)
    } else if (input$additionalplots == "crime_socioeco") {
      
      selected_label <- names(factor_choices)[factor_choices == input$factor_choice]
      area_crime <- CD %>%
        group_by(`AREA NAME`) %>%
        count(name = "n") %>%
        left_join(zipcodes_final, by = "AREA NAME") %>%
        mutate(CrimePerPopulation = n / Total_population,
               unemployment = Total_unemployed / (Total_employed + Total_unemployed),
               gap = (poverty_under_50 + poverty_50_to_99 + poverty_100_to_124) / poverty_200_and_over)
      
      plot <- ggplot(area_crime, aes_string(x = input$factor_choice, y = "CrimePerPopulation")) +
        geom_point() +
        geom_smooth(method = "loess", span = 1, se = FALSE) +
        labs(
          title = paste("Crime Per Population vs", selected_label),
          x = selected_label,
          y = "Crime Per Population"
        ) +
        theme_minimal()
      print(plot)
    } else if (input$additionalplots == "crime_proportion") {
      data <- filteredData.Page2.1() %>%
        select(., c(period, Crm.Cd.Group))
      
      total_count <- CD %>%
        group_by(Crm.Cd.Group) %>%
        count(name = "total_n")
      
      time_comp <- data %>%
        filter(period %in% input$selected_periods) %>%
        group_by(Crm.Cd.Group) %>%
        count(name = "n") %>%
        left_join(total_count, by = "Crm.Cd.Group") %>%
        mutate(prop = n / total_n)
      
      plot.1 <- ggplot(time_comp, aes(x = Crm.Cd.Group, y = prop, fill = Crm.Cd.Group)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set3") +
        labs(
          title = "Proportions by Selected Time Periods",
          x = "Crime Group",
          y = "Proportion"
        ) +
        theme_minimal()
      
      print(plot.1)
      
    } else if (input$additionalplots == "time_crime") {
      data <- filteredData.Page2() %>%
        select(., c(hour, period, `Dur Rptd`)) %>%
        mutate(hour = factor(hour, levels = 0:23),
               period = factor(period, levels = time_choices)) %>%
        filter(`Dur Rptd` > 1)
      
      if (input$time_div == "view_time") {
        plot.1 <- ggplot(data, aes(x = period, y = log(`Dur Rptd`))) +
          geom_violin(trim = FALSE, fill = "lightblue", draw_quantiles = c(0.25, 0.5, 0.75)) +
          xlab("Time of Day") +
          ylab("Log-transformated Duration of Report") +
          labs(
            title = "Crime Reporting Delays by Time of Day",
            subtitle = "Examining how long after the crime occurred it was reported\nacross distinct periods of the day",
            caption = "Data filtered for reports with more than 1 day delay"
          ) +
          theme_minimal()
        
      } else {
        plot.1 <- ggplot(data, aes(x = hour, y = log(`Dur Rptd`))) +
          geom_violin(trim = FALSE, fill = "lightblue", draw_quantiles = c(0.25, 0.5, 0.75)) +
          xlab("Hour") +
          ylab("Log-transformated Duration of Report") +
          labs(
            title = "Crime Reporting Delays by Hour of the Day",
            subtitle = "Visualizing reporting delays for crimes occurring at different hours\nof the day, focusing on reports taking more than 1 day",
            caption = "Data filtered for reports with more than 1 day delay"
          ) +
          theme_minimal()
      }
      
      print(plot.1)
    }
  })
    
    
    output$additionalPlot2 <- renderPlot({
      if (input$additionalplots == "crime_proportion") {
        crime_time_data <- CD %>%
          filter(Crm.Cd.Group == input$crime_for_time) %>%
          group_by(period) %>%
          count(name = "n") %>%
          ungroup() %>%
          mutate(prop = n / sum(n))
        
        plot.2 <- ggplot(crime_time_data, aes(x = "", y = prop, fill = period)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_brewer(palette = "Set3") +
          labs(
            title = paste("Crime Composition by Time: ", input$crime_for_time),
            fill = "Time Period"
          ) +
          theme_void()
        
        print(plot.2)
      } else if (input$additionalplots == "time_crime") {
        data <- filteredData.Page2() %>%
          select(., c(hour, period, `Dur Rptd`)) %>%
          mutate(hour = factor(hour, levels = 0:23),
                 period = factor(period, levels = time_choices))
        
        t <- data %>% group_by(period) %>% count(name = "sum")
        tt <- data %>% group_by(hour) %>% count(name = "sum")
        
        if (input$time_div == "view_time") {
          data.1 <- data %>% filter(., `Dur Rptd` <= 1) %>%
            group_by(period) %>% count(name = "n") %>%
            left_join(t, by = "period") %>%
            mutate(ratio = n / sum)
          
          
          plot.2 <- ggplot(data.1, aes(x = period, y = ratio)) +
            geom_bar(stat = "identity", fill = "orange") +
            xlab("Time of Day") +
            ylab("Ratio") +
            labs(title = "Ratio of Same-Day Reported Crimes by Time of Day")+
            theme_minimal()
          
        } else {
          data.1 <- data %>% filter(., `Dur Rptd` <= 1) %>%
            group_by(hour) %>% count(name = "n") %>%
            left_join(tt, by = "hour") %>%
            mutate(ratio = n / sum)
          
          plot.2 <- ggplot(data.1, aes(x = hour, y = ratio)) +
            geom_bar(stat = "identity", fill = "orange") +
            xlab("Hour") +
            ylab("Ratio") +
            labs(title = "Ratio of Same-Day Reported Crimes by Hour")+
            theme_minimal()
        }
        
        print(plot.2)
      }
    })
    
    ## PAGE 3 start
    selected_region <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", source = "myPlot"), {
      d <- event_data("plotly_click", source = "myPlot")
      if (!is.null(d)) {
        selected_region(d$key)
      }
    })
    
    output$map <- renderPlotly({
      sr <- selected_region()
      
      boundary_highlight <- boundary_df %>%
        mutate(fill_color = if (is.null(sr)) {
          "grey90"
        } else {
          ifelse(id == sr, "blue", "grey90")
        })
      
      p <- ggplot(boundary_highlight, aes(x = long, y = lat, group = group, fill = fill_color, key = id)) +
        geom_polygon(color = "white") +
        scale_fill_identity() +
        coord_equal() +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
      
      ggplotly(p, tooltip = "id", source = "myPlot") %>%
        layout(dragmode = "pan") %>%
        config(
          displayModeBar = FALSE,
          scrollZoom = FALSE
        ) %>%
        event_register("plotly_click")
    })
    
    output$clickInfo <- renderPrint({
      sr <- selected_region()
      
      if (is.null(sr)) {
        cat("Select Region.")
      } else {
        info <- daily_crime_by_area %>% filter(AREA_NAME == sr)
        
        if (nrow(info) == 0) {
          cat(paste0("Selected Region:\n", sr, "\nNo data available."))
        } else {
          percent_val <- round(info$dailycrimepercent[1], 4)
          
          output_text <- paste0(
            "Selected Region:\n", sr, "\n",
            "Daily Crime Probability per 100 people:\n", as.character(percent_val), "%",
            "\n(Derived as [(Total Crime / Population / 365) * 100])",
            "\nDon't worry! The actual crime likelihood is calculated by multiplying SELECTED AREA's crime probability."
          )
          cat(output_text)
        }
      }
    })
    
    prediction_ready <- reactiveVal(FALSE)
    observeEvent(input$predict_btn, {
      req(selected_region())
      
      
      new_data <- data.frame(
        AREA_NAME = factor(selected_region(), levels = levels(train_data$AREA_NAME)),
        vict_age = factor(input$model_age, levels = levels(train_data$vict_age)),
        vict_sex = factor(input$model_sex, levels = levels(train_data$vict_sex)),
        vict_descent = factor(input$model_descent, levels = levels(train_data$vict_descent)),
        period = factor(input$model_period, levels = levels(train_data$period))
      )

      prob_logit <- predict(multi_logit_model, newdata = new_data, type = "prob") %>% as.vector()
      names(prob_logit) <- levels(train_data$Crm.Cd.Group)
      print(prob_logit)
      prob_rf <- predict(rf_model, newdata = new_data, type = "prob") %>% as.vector()
      names(prob_rf) <- levels(train_data$Crm.Cd.Group)
      
      
      
      dmy <- dummyVars("~ AREA_NAME + vict_age + vict_sex + vict_descent + period", data = train_data)
      new_x <- data.frame(predict(dmy, newdata = new_data))
      dnew <- xgb.DMatrix(data = as.matrix(new_x))
      pred_xgb <- predict(xgb_model, dnew) %>% as.vector()
      names(pred_xgb) <- levels(train_data$Crm.Cd.Group)
      
      df_logit <- data.frame(
        Model = "Multinomial_Logit",
        Crime_Group = names(prob_logit),
        Probability = prob_logit
      )
      
      df_rf <- data.frame(
        Model = "Random_Forest",
        Crime_Group = names(prob_rf),
        Probability = prob_rf
      )
      
      df_xgb <- data.frame(
        Model = "XGBoost",
        Crime_Group = names(pred_xgb),
        Probability = pred_xgb
      )
      
      result_df <- rbind(df_logit, df_rf, df_xgb) %>%
        pivot_wider(
          names_from = Crime_Group,
          values_from = Probability
        )
      
      crime_names <- levels(train_data$Crm.Cd.Group)
      
      first_5 <- crime_names[1:5]
      second_5 <- crime_names[6:10]
      
      max_len <- max(nchar(crime_names))
      padded_names <- sapply(crime_names, function(x) sprintf("%-*s", max_len, x))
      
      table_df <- result_df
      
      table_df[, first_5] <- lapply(table_df[, first_5], function(x) sprintf("%.4f%%", x * 100))
      table_df[, second_5] <- lapply(table_df[, second_5], function(x) sprintf("%.4f%%", x * 100))

      colnames(result_df) <- c("Model", crime_names)
      
      first_5 <- crime_names[1:5]
      second_5 <- crime_names[6:10]
      
      output$predictionTable1 <- renderUI({
        tbl <- table_df[, c("Model", first_5)]
        HTML(
          kable(tbl, format = "html", align = c("l", rep("l", length(first_5)))) %>%
            kable_styling(full_width = FALSE) %>%
            add_header_above(c(" " = 1, "First 5 Crimes" = length(first_5)))
        )
      })
      
      output$predictionTable2 <- renderUI({
        tbl <- table_df[, c("Model", second_5)]
        HTML(
          kable(tbl, format = "html", align = c("l", rep("l", length(second_5)))) %>%
            kable_styling(full_width = FALSE) %>%
            add_header_above(c(" " = 1, "Next 5 Crimes" = length(second_5)))
        )
      })
      
      prediction_ready(TRUE)
      
      observeEvent(input$select_model_vis, {
        selected_model_data <- result_df %>%
          filter(Model == input$select_model_vis) %>%
          select(-Model) %>%
          pivot_longer(cols = everything(), names_to = "Crime_Type", values_to = "Probability")
        
        output$predictionChart <- renderPlot({
          ggplot(selected_model_data, aes(x = Crime_Type, y = Probability, fill = Crime_Type)) +
            geom_col(show.legend = FALSE) +
            scale_y_continuous(limits = c(0, NA), labels = function(x) sprintf("%.4f%%", x * 100)) +
            labs(
              title = paste("Predicted Probabilities -", input$select_model_vis),
              x = "Crime Type",
              y = "Probability"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
      }, ignoreInit = TRUE)
      
      selected_model_data <- result_df %>%
        filter(Model == "Multinomial_Logit") %>%
        select(-Model) %>%
        pivot_longer(cols = everything(), names_to = "Crime_Type", values_to = "Probability")
      
      output$predictionChart <- renderPlot({
        ggplot(selected_model_data, aes(x = Crime_Type, y = Probability, fill = Crime_Type)) +
          geom_col(show.legend = FALSE) +
          labs(
            title = "Predicted Probabilities - Multinomial_Logit",
            x = "Crime Type",
            y = "Probability"
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output$predictionReady <- reactive({
        prediction_ready()
      })
      outputOptions(output, "predictionReady", suspendWhenHidden = FALSE)
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)