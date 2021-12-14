library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(data.table)
library(tidyverse)


data_raw <- read.csv('shiny_app_table.csv')
source("functions.R")

# Define UI for data download app ----

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "MS Wearable Sensors - A Review",
                  titleWidth = 320),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(width = 320,
                   # Input: Choose dataset ----
                   #selectInput("dataset", "Choose a dataset:",
                   #           choices = c("rock", "pressure", "cars")),
                   
                   # # Button
                   # downloadButton("downloadData", "Download table", style="margin: 15px;"),
                   
                   # Input: Choose type of wearable ----
                   checkboxGroupInput("wearable_type", label = "Choose a type of sensor:",
                                      choices = list("accelerometer", 
                                                     "gyroscope", 
                                                     "magnetometer",
                                                     "smartphone/touchscreen",
                                                     'others',
                                                     'not reported'),
                                      selected = c("accelerometer", 
                                                      "gyroscope", 
                                                      "magnetometer",
                                                      "smartphone/touchscreen",
                                                      'others', 'not reported')),
                   
                   div(style="text-align:center",em("Others: electrocardiogram amplifier, motion tracker", "global positioning system (GPS), surface electromyography (sEMG)")),
                   #helpText('Others: electrocardiogram amplifier, motion tracker, global positioning system (GPS), surface electromyography (sEMG)'),
                   
                   # Input: Choose position of wearable ----
                   checkboxGroupInput("wearable_position", label = "Choose a position of interest for the sensor:",
                                      choices = list("sternum", "upper back", "lower back", "waist", 
                                                     "upper arm", "lower arm", "wrist", "hand",
                                                     "upper leg", "lower leg", "ankle", "foot", 'others', "not reported"),
                                      selected = c("sternum", "upper back", "lower back", "waist", 
                                                   "upper arm", "lower arm", "wrist", "hand",
                                                   "upper leg", "lower leg", "ankle", "foot", 'others', "not reported")),
                   
                   div(style="text-align:center",em('Others: head, normal phone position, pocket or bag, and tip of crutches.')),
                   
                   # Input: Choose context of wearable ----
                   checkboxGroupInput("wearable_context", label = "Choose a context for usage of sensor:",
                                      choices = list("real-life", "lab", "mixed" = 'both'),
                                      selected = c("real-life", "lab", "both"))
                   
                   
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0253b5;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #0253b5;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #0253b5;
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #6baeff;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ff0000;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ff9800;
                                }
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff9800;
                                }
                                
                                /* sidebar text color */  
                                .skin-blue .sidebar a{
                                color: #0f0f0f;}
                                
                                .btn.btn-success {
                     color: #fff;
                    background-color: #ff9800;
                    border-color: #ff9800;
                    }
                    .btn.btn-success.focus,
                    .btn.btn-success:focus {
                    color: #fff;
                    background-color: #ff9800;
                    border-color: #ff9800;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success:hover {
                    color: #fff;
                    background-color: #ff9800;
                    border-color: #ff9800;
                    outline: none;
                    box-shadow: none;
                    }
                    .btn.btn-success.active,
                    .btn.btn-success:active {
                    color: #fff;
                    background-color: #f7dfbc;
                    border-color: #f7dfbc;
                    outline: none;
                    }
                    .btn.btn-success.active.focus,
                    .btn.btn-success.active:focus,
                    .btn.btn-success.active:hover,
                    .btn.btn-success:active.focus,
                    .btn.btn-success:active:focus,
                    .btn.btn-success:active:hover {
                    color: #fff;
                    background-color: #f7dfbc ;
                    border-color: #f7dfbc ;
                    outline: none;
                    box-shadow: none;
                    }
                                
                                '))),
    
    tabBox(width=12,id="tabBox_next_previous",
           tabPanel("Summary",
                    fluidRow(
                      column(12,
                             fluidRow(
                               column(3, 
                                      valueBoxOutput("nb_papers", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_reallife", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_lab", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_mixed", width = 12))
                               
                             )),
                      #valueBoxOutput("nb_papers", width = 6),
                      box(plotlyOutput("hist_years", height = "60vh"), width = 12, height = "60vh"),
                      
                      conditionalPanel(condition = "input.wearable_type.indexOf('accelerometer') > -1",
                        box(plotlyOutput("hist_axes", height = "60vh"), width = 12, height = "60vh"))
                    ) # end fluidRow
                    
                    #valueBox("5000+", "Patients", icon = icon("hospital-user"), width = 12, color = "yellow"),
                    #valueBox("15", "Countries", icon = icon("globe-europe"), width = 3, color = "blue"),
                    #valueBox("20", "Years", icon = icon("calendar-alt"), width = 3, color = "blue"),
                    #valueBox("50+", "Researchers", icon = icon("user-cog"), width = 3, color = "blue")#,
                    
           ), #end tabPanel 1
           
           tabPanel("Papers",
                    downloadButton("downloadData", "Download table", style="margin: 15px;"),
                    dataTableOutput("table"),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
                    
           ) #end tabPanel 2
    ) #end tabBox
  ) # end dashboardBody
) # end dashboardPage


# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # # Reactive value for selected dataset ----
  # datasetInput <- reactive({
  #   types <- input$wearable_type
  #   position <- input$wearable_position
  #   context <- input$wearable_context
  #   data <- data_raw[data_raw$Wearable %like% types, ]
  #   data <- data[data$Wearable %like% position, ]
  #   data <- data[data$Context.of.primary.wearable %like% context, ]
  # })
  
  # Table of selected dataset ----
  output$table <- renderDataTable(escape=F, {
    data <- data_raw
    data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
    
    data$DOI <- paste0("https://doi.org/", data$DOI, sep='')
    data$First.author.and.year <- paste0("<a href='", data$DOI,"' target='_blank'>", data$First.author.and.year,"</a>")
    subset(data, select=-c(Authors, Year, DOI, sensors_type_plot, axes))
  })
  
  #Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste(input$wearable_type, "wearables_", 
            input$wearable_position, "positions_", 
            input$wearable_context, "context", ".csv", 
            sep = "")
      
    },
    
    content = function(file) {
      
      data <- data_raw
      data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
      data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
      data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
      
      sub_data <- subset(data, select=-c(sensors_type_plot, axes))
      write.csv(sub_data, file, row.names = FALSE)
      
    }
  )
  
  output$nb_papers <- renderValueBox({
    
    data <- data_raw
    data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
    
    #data <- data[data$sensors_type_plot %like% input$wearable_type, ]
    #data <- data[data$Wearable %like% input$wearable_position, ]
    #data <- data[data$Context %like% input$wearable_context, ]
    
    valueBox(paste0(dim(data)[1], " papers"), 
             "meeting your input criteria", 
             icon = icon("scroll"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_reallife <- renderValueBox({
    
    #print(input$wearable_type[1])
    data <- data_raw
    data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
    
    nb_realsetting <- sum(data$Context == 'real-life setting')
    
    valueBox(paste0(nb_realsetting, " papers"),
             "conducted in a real-life setting",
             icon = icon("house-user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_lab <- renderValueBox({
    
    data <- data_raw
    data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
    
    nb_lab <- sum(data$Context == 'lab setting (controlled)')
    
    valueBox(paste0(nb_lab, " papers"),
             "conducted in a lab setting",
             icon = icon("search"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_mixed <- renderValueBox({
    
    data <- data_raw
    data <- data[grepl(paste(input$wearable_type, collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(input$wearable_position, collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(input$wearable_context, collapse="|"), data$Context), ]
    
    nb_mixed <- sum(data$Context == 'both')
    
    valueBox(paste0(nb_mixed, " papers"),
             "conducted in mixed setting",
             icon = icon("user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$hist_years <- renderPlotly({
    
    data_nb_wearable <- prepare_data_nb_wearable(data = data_raw,
                                                 wearable = input$wearable_type,
                                                 position = input$wearable_position,
                                                 context = input$wearable_context)
    
    plot <- plot_ly(
      data = data_nb_wearable,
      x = ~year,
      y = data_nb_wearable[[2]],
      type = 'bar',
      name = names(data_nb_wearable)[2]) %>%
      layout(yaxis = list(title = 'Number of wearables'), 
             xaxis = list(title = 'Year of publication'),
             barmode = 'stack',
             orientation='v')
    
    count = 3
    while (!(is.na(names(data_nb_wearable)[count]))) {
      plot <- plot %>% add_trace(y=data_nb_wearable[[count]], name = names(data_nb_wearable)[count])
      count = count + 1
    }
      #add_trace(y=~accelerometer, name = 'accelerometer') %>%
      #add_trace(y=~gyroscope, name = 'gyroscope') %>%
      #add_trace(y=~magnetometer, name = 'magnetometer') %>%
      #add_trace(y=~`smartphone/touchscreen`, name = 'smartphone/touchscreen') %>%
      #add_trace(y=~others, name = 'others') %>%
     plot

  })
  
  output$hist_axes <- renderPlotly({
    
    data <- data_raw
    data$Year[data$Year == 2020] <- "2020-2021*"
    data$Year[data$Year == 2021] <- "2020-2021*"
    
    accelerometer_data_axis <- data %>%
      group_by(Year) %>% 
      filter(grepl("accelerometer", sensors_type_plot)) %>% 
      count(Year, sensors_type_plot, axes) %>% 
      spread(sensors_type_plot, n, fill = 0) 
    accelerometer_data2_axis <- accelerometer_data_axis %>% 
      group_by(Year, axes) %>%
      mutate(total = sum(c_across(starts_with("accelerometer"))))
    
    # Subset the accelerometer_data2_axis table to only the total number per year
    data_all_axis <- accelerometer_data2_axis[c('Year', 'axes', 'total')]
    # Harmonise column names
    colnames(data_all_axis) <- c("year","axis", "number")
    # Add a year with no wearable to create a space between 1997 and 2006
    data_all_axis <- rbind(data.frame(data_all_axis), c(1998, NA, NA)) 
    
    test <- reshape(data_all_axis, idvar = "year", timevar = "axis", direction = "wide")
    
    plot_ly(
      data = test,
      x = ~year,
      y = ~`number.1`,
      type = 'bar',
      name = 'uniaxial') %>%
      add_trace(y=~`number.2`, name = 'biaxial') %>%
      add_trace(y=~`number.3`, name = 'triaxial') %>%
      layout(yaxis = list(title = 'Number of accelerometers'), 
             xaxis = list(title = 'Year of publication'),
             barmode = 'stack')
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)