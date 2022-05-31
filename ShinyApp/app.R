library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(data.table)
library(tidyverse)
library(readr)
library(ggpubr)

data <- read.csv('extracted_data.csv', check.names=F)
data_raw <- data[c('Authors','Year','First author and year','Title','DOI','Number (% female)','Age','Type of MS','Severity','Duration of disease in years','Treatments','Comorbidities','Comparator population, number (number of females)','Wearable','Context','Duration wearable was worn','Does the study have a clearly defined research objective (including an outcome)?','Does the study adequately describe the inclusion/exclusion criteria?','Does the study report on the population parameters/demographics (at least age, sex)?','Does the study report details on assessment of MS (severity [EDSS or PDSS], type)?','Does the study provide sufficient details on the wearables used (type, positioning of wearable, context, recording frequency)?','Does the study apply proper statistical analysis? Correction for multiple comparisons?','Does the study adequately report the strength of the results (e.g., ways of calculating effect sizes, reporting confidence intervals/standard deviation?','Does the study make the data and/or code publicly available?','Do the authors report on the limitations of their study?','sensors_type_plot','axes')]

sensor_types = c("accelerometer", "gyroscope", "magnetometer", "touchscreen", "mechanical pedometer", "others")
wearable_positions = c("sternum", "upper back", "lower back", "waist",
                       "upper arm", "lower arm", "wrist", "hand",
                       "upper leg", "lower leg", "ankle", "foot", "others", "not reported")
wearable_contexts = c("real-world", "laboratory", "mixed")
wearable_domains = c("Physical activity", 'Gait', 'Balance', 'Dexterity/Tremor')
result_columns <- c(
  "Association with MS severity",
  "Association with other measure",
  "Test-retest reliability",
  "Group differences MS vs HC",
  "Group differences MS vs MS",
  "Group differences MS vs other diseases",
  "Responsiveness to change",
  "Responsiveness to intervention",
  "Subjective patient acceptability"
)

data_metaplot <- data[c('DOI','Domain1','Domain2','Domain3','Domain4',result_columns,paste0(result_columns, " - Effect"))]

# Define filter functions here ...
filter_by_sensor_type = function(data, sensor_types) data[grepl(paste(sensor_types, collapse="|"), data$sensors_type_plot),]
filter_by_wearable_position = function(data, wearable_position) data[grepl(paste(wearable_position, collapse="|"), data$Wearable),]
filter_by_wearable_context = function(data, wearable_context) data[grepl(paste(wearable_context, collapse="|"), data$Context),]
filter_by_wearable_domain = function(data, wearable_domain) {
  indices <- grep(paste(wearable_domain, collapse="|"), data$Domain1)
  indices <- append(indices, grep(paste(wearable_domain, collapse="|"), data$Domain2))
  indices <- append(indices, grep(paste(wearable_domain, collapse="|"), data$Domain3))
  indices <- append(indices, grep(paste(wearable_domain, collapse="|"), data$Domain4))
  indices <- sort(unique(indices))
  
  data[indices,]
}
filter_by_reported_results = function(data, reported_results) {
  # CAREFUL: 5 is hardcoded, check again when changing data base
  if ("None of the above (5)" %in% reported_results) {
    selected = data %>% filter_at(vars(result_columns), all_vars(. %in% c('no')))
  } else {
    selected = data.frame()
  }
  results_selected_wo_none <- reported_results[!reported_results %in% c("None of the above (5)")]
  if (length(results_selected_wo_none)) selected = rbind(selected, data %>% filter_at(vars(results_selected_wo_none), any_vars(. %in% c('yes'))))
  selected
}

# ...and add total number of publications behind each filter option in sidebar
for (i in 1:length(sensor_types)) {
  names(sensor_types)[i] = paste0(sensor_types[i], " (", nrow(filter_by_sensor_type(data_raw, sensor_types[i])), ")")
}
for (i in 1:length(wearable_positions)) {
  names(wearable_positions)[i] = paste0(wearable_positions[i], " (", nrow(filter_by_wearable_position(data_raw, wearable_positions[i])), ")")
}
for (i in 1:length(wearable_contexts)) {
  names(wearable_contexts)[i] = paste0(wearable_contexts[i], " (", nrow(filter_by_wearable_context(data_raw, wearable_contexts[i])), ")")
}
for (i in 1:length(wearable_domains)) {
  names(wearable_domains)[i] = paste0(wearable_domains[i], " (", nrow(filter_by_wearable_domain(data_metaplot, wearable_domains[i])), ")")
}
for (i in 1:length(result_columns)) {
  names(result_columns)[i] = paste0(result_columns[i], " (", nrow(filter_by_reported_results(data_metaplot, result_columns[i])), ")")
}

facet_strip_bigger <- function(gp){
  
  # n_facets should be the number of facets x2
  n_facets <- c(1:length(gp[["x"]][["layout"]][["shapes"]]))
  
  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      gp[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- + 50 # increase as needed
      gp[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }
  
  return(gp)
}

# Define UI for data download app ----

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "MS Wearable Sensors - A Review",
                  titleWidth = 320),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(width = 320,
                   
                   div(style="text-align:center",em('For each category, papers with at least one item corresponding to selection will be presented.')),
                   
                   # Input: Choose type of wearable ----
                   checkboxGroupInput("sensor_type", label = "Choose a type of sensor:",
                                      choices = sensor_types,
                                      selected = sensor_types),
                   
                   div(style="text-align:center",em("Others: electrocardiogram (ECG), global ", "positioning system (GPS), surface electromyography (sEMG),", "portable metabolic system (VO2)")),

                   # Input: Choose position of wearable ----
                   checkboxGroupInput("wearable_position", label = "Choose a position of interest for the sensor:",
                                      choices = wearable_positions,
                                      selected = wearable_positions),
                   
                   div(style="text-align:center",em('Others: head, pocket or bag, and tip of crutches.')),
                   
                   # Input: Choose context of wearable ----
                   checkboxGroupInput("wearable_context", label = "Choose a context for usage of sensor:",
                                      choices = wearable_contexts,
                                      selected = wearable_contexts),
                   
                   # Input: Choose context of wearable ----
                   checkboxGroupInput("wearable_domain", label = "Choose a domain for usage of sensor:",
                                      choices = wearable_domains,
                                      selected = wearable_domains),
                   
                   checkboxGroupInput("reported_results", label = "Choose the type of results reported:",
                                      choices = c(result_columns, "None of the above (5)"),
                                      
                                      selected = c(result_columns, "None of the above (5)"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(' /* fix background-color of wrapper (is black otherwise for some reason) */
                                .skin-blue .wrapper {
                                background-color: #ecf0f5;
                                }
                                p {
                                margin: 10px
                                }
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
                                      valueBoxOutput("nb_realworld", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_lab", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_mixed", width = 12))
                               
                             )),
                      #valueBoxOutput("nb_papers", width = 6),
                      
                      box(plotlyOutput("meta_results", height = "800px"), width = 12, height = "800px"),
                      
                      box(plotlyOutput("hist_years", height = "400px"), width = 12, height = "400px"),
                      
                      conditionalPanel(condition = "input.sensor_type.indexOf('accelerometer') > -1",
                        box(plotlyOutput("hist_axes", height = "400px"), width = 12, height = "400px")),
                      
                      p("* 2020-2021 contains papers published from January 2020 to March 2021, when the literature search was performed"),
                      p("** Publications using multiple types of sensors count multiple times")
                      
                    ) # end fluidRow

           ), #end tabPanel 1
           
           tabPanel("Papers",
                    dataTableOutput("table"),style = "overflow-x: scroll;"
                    
           ) #end tabPanel 2
    ) #end tabBox
  ) # end dashboardBody
) # end dashboardPage


# Debug
input = list(
  sensor_type=sensor_types,
  wearable_position=wearable_positions,
  wearable_context=wearable_contexts,
  wearable_domain=wearable_domains,
  reported_results=c(result_columns, "None of the above (5)")
)
sensor_type_d = function() input$sensor_type
wearable_position_d = function() input$wearable_position
wearable_context_d = function() input$wearable_context
wearable_domain_d = function() input$wearable_domain
reported_results_d = function() input$reported_results

filtered_data = function() inner_join(data_raw, data_metaplot, by=c("DOI"))
filtered_data_combined_2020_2021 = function() {
  data = filtered_data()
  data$Year[data$Year %in% c(2020, 2021)] <- "2020-2021*"
  return(data)
}

# Define server logic to display and download selected file ----
server <- function(input, output) {
  # Debounce checkboxGroupInput filter inputs so that multiple changes in selections are evaluated together
  sensor_type <- reactive(input$sensor_type)
  sensor_type_d <- sensor_type %>% debounce(1000)
  
  wearable_position <- reactive(input$wearable_position)
  wearable_position_d <- wearable_position %>% debounce(1000)
  
  wearable_context <- reactive(input$wearable_context)
  wearable_context_d <- wearable_context %>% debounce(1000)
  
  wearable_domain <- reactive(input$wearable_domain)
  wearable_domain_d <- wearable_domain %>% debounce(1000)
  
  reported_results <- reactive(input$reported_results)
  reported_results_d <- reported_results %>% debounce(1000)
  
  filtered_data <- reactive({
    data <- data_raw
    data <- filter_by_sensor_type(data, sensor_type_d())
    data <- filter_by_wearable_position(data, wearable_position_d())
    data <- filter_by_wearable_context(data, wearable_context_d())
    
    merged.df <- inner_join(data, data_metaplot, by=c("DOI"))
    
    merged.df <- filter_by_wearable_domain(merged.df, wearable_domain_d())
    merged.df <- filter_by_reported_results(merged.df, reported_results_d())
    
    return(merged.df)
  })
  
  filtered_data_combined_2020_2021 <- reactive({
    data <- filtered_data()
    data$Year[data$Year %in% c(2020, 2021)] <- "2020-2021*"
    return(data)
  })
  
  # Table of selected dataset ----
  output$table <- renderDataTable({
    merged.df <- filtered_data()
    
    merged.df$DOI <- paste0("https://doi.org/", merged.df$DOI, sep='')
    merged.df$`First author and year` <- paste0("<a href='", merged.df$DOI,"' target='_blank'>", merged.df$`First author and year`, "</a>")
    
    # Remove unnecessary columns
    x <- names(merged.df)
    col_to_select <- x[! x %in% c('Authors', 'DOI', 'sensors_type_plot', 'axes', 'X', colnames(data_metaplot))]
    datatable(subset(merged.df, select=col_to_select), escape=F, filter="top", extensions = 'Buttons', options = list(
      dom = 'Bfrti',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'colvis'),
      pageLength=1000)
    )
  })
  
  output$nb_papers <- renderValueBox({
    merged.df = filtered_data()
    
    valueBox(paste0(dim(merged.df)[1], " papers"), 
             "meeting your input criteria", 
             icon = icon("scroll"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_realworld <- renderValueBox({
    merged.df = filtered_data()
    
    nb_realsetting <- sum(merged.df$Context == 'real-world')
    
    valueBox(paste0(nb_realsetting, " papers"),
             "conducted in a real-world setting",
             icon = icon("house-user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_lab <- renderValueBox({
    merged.df = filtered_data()
    
    nb_lab <- sum(merged.df$Context == 'laboratory')
    
    valueBox(paste0(nb_lab, " papers"),
             "conducted in a laboratory setting",
             icon = icon("search"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_mixed <- renderValueBox({
    merged.df = filtered_data()
    
    nb_mixed <- sum(merged.df$Context == 'mixed')
    
    valueBox(paste0(nb_mixed, " papers"),
             "conducted in mixed setting",
             icon = icon("user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$hist_years <- renderPlotly({
    
    prepare_data_nb_wearable <- function(data, data_metaplot, wearable, position, context, domain, results){
      merged.df = filtered_data_combined_2020_2021()
      
      counts_combined_fig2=data.frame(Year=unique(merged.df$Year))
      
      if ('mechanical pedometer' %in% wearable){
        mechanical_pedometer_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("mechanical pedometer", sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        mechanical_pedometer_data2 <- mechanical_pedometer_data %>% mutate(total_mechanical_pedometer = sum(c_across(contains("mechanical pedometer"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      mechanical_pedometer_data2[c('Year', 'total_mechanical_pedometer')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "mechanical_pedometer" = 'total_mechanical_pedometer')
      }
      
      if ('accelerometer' %in% wearable){
        accelerometer_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("accelerometer", sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        accelerometer_data2 <- accelerometer_data %>% mutate(total_accelerometer = sum(c_across(contains("accelerometer"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      accelerometer_data2[c('Year', 'total_accelerometer')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "accelerometer" = 'total_accelerometer')
      }
      
      # Count all appearances of gyroscopes per year
      if ('gyroscope' %in% wearable){
        gyroscope_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("gyroscope",sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        gyroscope_data2 <- gyroscope_data %>% mutate(total_gyroscope = sum(c_across(contains("gyroscope"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      gyroscope_data2[c('Year', 'total_gyroscope')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "gyroscope" = 'total_gyroscope')
      }
      
      # Count all appearances of magnetometers per year
      if ('magnetometer' %in% wearable){
        magnetometer_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("magnetometer",sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        magnetometer_data2 <- magnetometer_data %>% mutate(total_magnetometer = sum(c_across(contains("magnetometer"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      magnetometer_data2[c('Year', 'total_magnetometer')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "magnetometer" = 'total_magnetometer')
      }
      
      # Count all appearances of touchscreen per year
      if ('touchscreen' %in% wearable){
        touchscreen_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("touchscreen",sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        touchscreen_data2 <- touchscreen_data %>% mutate(total_touchscreen = sum(c_across(contains("touchscreen"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      touchscreen_data2[c('Year', 'total_touchscreen')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "touchscreen" = 'total_touchscreen')
      }
      
      # Count all appearances of other wearables used per year
      if ("others" %in% wearable){
        others_data <- merged.df %>%
          group_by(Year) %>% 
          filter(grepl("others",sensors_type_plot)) %>% 
          count(Year, sensors_type_plot) %>% 
          spread(sensors_type_plot, n, fill = 0) 
        others_data2 <- others_data %>% mutate(total_others = sum(c_across(contains("others"))))
        
        counts_combined_fig2 <- merge(counts_combined_fig2, 
                                      others_data2[c('Year', 'total_others')], 
                                      by="Year", all = T)
        counts_combined_fig2 = rename(counts_combined_fig2, 
                                      "others" = 'total_others')
      }
      
      counts_combined_fig2[is.na(counts_combined_fig2)] <- 0
      
      # Harmonise column names
      #colnames(counts_combined_fig2) <- c("year", "accelerometer", "gyroscope", 
      #                                    'magnetometer', 'touchscreen', 
      #                                    "others")
      
      counts_combined_fig2 = rename(counts_combined_fig2, 'year' = "Year")
      
      return (counts_combined_fig2)
    }
    
    data_nb_wearable <- prepare_data_nb_wearable(data = data_raw,
                                                 data_metaplot = data_metaplot,
                                                 wearable = sensor_type_d(),
                                                 position = wearable_position_d(),
                                                 context = wearable_context_d(),
                                                 domain = wearable_domain_d(),
                                                 results = reported_results_d())
    
    colorsv = c("#1F78B4", "#A6CEE3", "#1F78B4", "#FDBF6F", "#FF7F00", "#FFFF99")
    plot <- plot_ly(
      data = data_nb_wearable,
      x = ~year,
      y = data_nb_wearable[[2]],
      type = 'bar',
      name = names(data_nb_wearable)[2],
      color = colorsv[2],
      colors = colorRamp(colorsv)) %>%
      layout(title = list(text = '<b>Number of publications published per year, per type of sensor used<b>', font = list(size = 14)),
             yaxis = list(title = 'Number of publications per sensor type**'), 
             xaxis = list(title = 'Year of publication'),
             barmode = 'stack')
    
    count = 3
    while (!(is.na(names(data_nb_wearable)[count]))) {
      plot <- plot %>% add_trace(y=data_nb_wearable[[count]], 
                                 name = names(data_nb_wearable)[count],
                                 color = colorsv[count])
      count = count + 1
    }
    plot

  })
  
  output$hist_axes <- renderPlotly({
    data = filtered_data_combined_2020_2021()
    
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
    colnames(data_all_axis) <- c("year", "axis", "number")
    # Add a year with no wearable to create a space between 1997 and 2006
    data_all_axis <- rbind(data.frame(data_all_axis), c(1998, NA, NA))
    
    test <- reshape(data_all_axis, idvar = "year", timevar = "axis", direction = "wide")
    
    if (!('number.1' %in% names(test))){
      test['number.1'] <- NA
    }
    if (!('number.2' %in% names(test))){
      test['number.2'] <- NA
    }
    if (!('number.3' %in% names(test))){
      test['number.3'] <- NA
    }
          
    plot_ly(
      data = test,
      x = ~year,
      y = ~`number.3`,
      type = 'bar',
      name = 'triaxial',
      colors = colorRamp(c("#A6CEE3", "#FFFF99", "#1F78B4")),
      color=c('FFFF99')) %>%
      add_trace(y=~`number.2`, name = 'biaxial', color=c('#A6CEE3')) %>%
      add_trace(y=~`number.1`, name = 'uniaxial', color=c('#1F78B4')) %>%
      layout(title = list(text = '<b>Number of publications published per year, per accelerometer type (number of axes)<b>', font = list(size = 14)),
        yaxis = list(title = 'Number of publications per accelerometer type'), 
        xaxis = list(title = 'Year of publication'),
        barmode = 'stack')

    
  })
  
  output$meta_results <- renderPlotly({
    merged.df = filtered_data()
    
    domains = c("RW: Physical activity", "RW: Gait", "RW: Balance", "RW: Dexterity/Tremor", "Lab: Physical activity", "Lab: Gait", "Lab: Balance", "Lab: Dexterity/Tremor")
    effects = c("not-tested", "non-significant", "mixed", "significant")
    
    plot_data = data.frame()
    
    for (column in result_columns) {
      total = sum(merged.df[column] == "yes")
      for (domain in domains) {
        for (effect in effects) {
          count = sum(substr(merged.df[apply(merged.df, 1, function(row) domain %in% row[paste0("Domain", 1:6)]), paste0(column, " - Effect")], 0, nchar(effect)) == effect)
          plot_data = rbind(plot_data, list(
            column = column,
            domain = domain,
            effect = effect,
            count = count,
            proportion = count/total
          ))
        }
      }
    }
    
    plot_data$domain = factor(plot_data$domain, levels=rev(domains))
    plot_data$effect = factor(plot_data$effect, levels=effects, labels=c("significance not tested", "non-significant", "some significant", "significant"))
    
    plot_data_sub = plot_data#[plot_data$column %in% c(columns[1:4], columns[6:8]),]
    plot_data_sub$column = factor(plot_data_sub$column, levels=result_columns, labels=result_columns)
    
    plot <- ggplot(plot_data_sub, aes(domain, count, fill=effect, label=count)) + 
      geom_bar(stat="identity", position=position_stack(reverse=T)) + 
      geom_text(data=plot_data_sub[plot_data_sub$count>0,], size = 3, position=position_stack(vjust=0.5, reverse=T)) + #, aes(color = effect), show.legend = FALSE) +
      #scale_color_manual(values = c("black", "black", "black", "white")) + 
      facet_wrap(facets=vars(column), scales="free_x", nrow=3, ncol=3, drop=FALSE) +
      theme_pubclean() + xlab(NULL) + ylab(NULL) + coord_flip() + scale_fill_manual(values=c("#d9d9d9", "#fdbf6f", "#96c3dc", "#1b63a5")) +
      #https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      theme(panel.grid.major.x=element_line(size=.1, linetype=2, color="black"), 
            panel.grid.major.y=element_blank(),
            legend.title = element_blank())
    
    #pdf("../Figure 3.pdf", width=8, height=8)
    #plot
    #dev.off()
    
    ggplotly(plot) %>%
      #facet_strip_bigger() %>%
      layout(title = list(text = '<b>Results reported by domain and context studied<b>', font = list(size = 14), y=.95),
             margin = list(l = 75, t = 100))
      
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)