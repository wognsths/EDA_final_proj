library(shiny);library(tidyverse);library(data.table);library(ggmosaic)

data <- fread("Crime_Data_2023.csv")
data[, `:=` (
  Severity = factor(`Part 1-2`, levels = c(1, 2), labels = c("Severe", "Less Severe")),
  weapon_usage = ifelse(is.na(`Weapon Used Cd`), "No", "Yes"),
  crime_status = ifelse(`Status Desc` %like% "Juv", "Juvenile", 
                        ifelse(`Status Desc` %like% "Adult", "Adult", NA)),
  vict_sex = ifelse(((`Vict Sex` == "-") | (`Vict Sex` == "X") | (`Vict Sex` == "")), NA, `Vict Sex`)
)]


ui <- fluidPage(
  titlePanel("Crime Data Mosaic Plot Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_areas", 
                         "Select Area(s):", 
                         choices = unique(data$`AREA NAME`), 
                         selected = unique(data$`AREA NAME`)),
      actionButton("select_all", "Select All Areas"),
      actionButton("clear_selection", "Clear Selection")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Victim Sex",
                 plotOutput("vict_sex_plot")),
        tabPanel("Weapon Usage",
                 plotOutput("weapon_usage_plot")),
        tabPanel("Crime Status",
                 plotOutput("crime_status_plot"))
      )
    )
  )
)


server <- function(input, output, session) {
  # "Select All Areas" 
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_areas", 
                             selected = unique(data$`AREA NAME`))
  })
  
  # "Clear Selection" -> initialize choices 
  observeEvent(input$clear_selection, {
    updateCheckboxGroupInput(session, "selected_areas", 
                             selected = character(0))
  })
  
  filtered_data <- reactive({
    if (length(input$selected_areas) == 0) {
      return(data)  
    } else {
      return(data[`AREA NAME` %in% input$selected_areas])
    }
  })
  
  # Victim Sex vs Severity Plot
  output$vict_sex_plot <- renderPlot({
    vict_sex_data <- filtered_data()[!is.na(vict_sex)] %>% .[, vict_sex := as.factor(vict_sex)]
    
    ggplot(data = vict_sex_data) +
      geom_mosaic(aes(x = product(Severity, vict_sex), 
                      fill = Severity)) +
      scale_fill_manual(values = c("turquoise3", "orange2")) +
      labs(x = "Victim Sex", 
           y = "Severity", 
           title = ifelse(
             length(input$selected_areas) == length(unique(data$`AREA NAME`)), 
             "Victim Sex vs Severity in All Areas", 
             paste("Victim Sex vs Severity in", paste(input$selected_areas, collapse = ", "))
           )) +
      theme_mosaic()
  })
  
  # Weapon Usage vs Severity Plot
  output$weapon_usage_plot <- renderPlot({
    weapon_usage_data <- filtered_data() %>% .[, weapon_usage := as.factor(weapon_usage)]
    
    ggplot(data = weapon_usage_data) +
      geom_mosaic(aes(x = product(Severity, weapon_usage), 
                      fill = Severity)) +
      scale_fill_manual(values = c("turquoise3", "orange2")) +
      labs(x = "Weapon Usage", 
           y = "Severity", 
           title = ifelse(
             length(input$selected_areas) == length(unique(data$`AREA NAME`)), 
             "Weapon Usage vs Severity in All Areas", 
             paste("Weapon Usage vs Severity in", paste(input$selected_areas, collapse = ", "))
           )) +
      theme_mosaic()
  })
  
  # Crime Status vs Severity Plot
  output$crime_status_plot <- renderPlot({
    crime_status_data <- filtered_data()[!is.na(crime_status)] %>% .[, crime_status := as.factor(crime_status)]
    
    ggplot(data = crime_status_data) +
      geom_mosaic(aes(x = product(Severity, crime_status), 
                      fill = Severity)) +
      scale_fill_manual(values = c("turquoise3", "orange2")) +
      labs(x = "Crime Status", 
           y = "Severity", 
           title = ifelse(
             length(input$selected_areas) == length(unique(data$`AREA NAME`)), 
             "Crime Status vs Severity in All Areas", 
             paste("Crime Status vs Severity in", paste(input$selected_areas, collapse = ", "))
           )) +
      theme_mosaic()
  })
}

shinyApp(ui, server)