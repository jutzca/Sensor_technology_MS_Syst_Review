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

data_raw <- read.csv('shiny_app_table.csv', check.names=F)
data_metaplot <- read.csv('data_meta_figure.csv')

result_columns <- c(
  "Correlation...association.with.clinical.MS.severity.scores..cross.sectional...e.g..EDSS..PDDS",
  "Correlation...association.with.other.measure..cross.sectional.",
  "Test.retest.reliability",
  "Group.differences..MS.vs.HC.",
  "Group.differences..MS.vs.MS.",
  "Group.differences..MS.vs.other.diseases.",
  "Responsiveness.to.change",
  "Responsiveness.to.intervention..controlled.study.",
  "Content.validity..meaningfulness.to.patients."
)

source("functions.R")

# Define UI for data download app ----

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "MS Wearable Sensors - A Review",
                  titleWidth = 320),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(width = 320,
                   
                   div(style="text-align:center",em('For each category, papers with at least one item corresponding to selection will be presented.')),
                   
                   # Input: Choose type of wearable ----
                   checkboxGroupInput("wearable_type", label = "Choose a type of sensor:",
                                      choices = list("accelerometer", 
                                                     "gyroscope", 
                                                     "magnetometer",
                                                     "smartphone touchscreen",
                                                     'others',
                                                     'not reported'),
                                      selected = c("accelerometer", 
                                                      "gyroscope", 
                                                      "magnetometer",
                                                      "smartphone touchscreen",
                                                      'others', 'not reported')),
                   
                   div(style="text-align:center",em("Others: electrocardiogram (ECG), global ", "positioning system (GPS), surface electromyography (sEMG)")),

                   # Input: Choose position of wearable ----
                   checkboxGroupInput("wearable_position", label = "Choose a position of interest for the sensor:",
                                      choices = list("sternum", "upper back", "lower back", "waist", 
                                                     "upper arm", "lower arm", "wrist", "hand",
                                                     "upper leg", "lower leg", "ankle", "foot", 'others', "not reported"),
                                      selected = c("sternum", "upper back", "lower back", "waist", 
                                                   "upper arm", "lower arm", "wrist", "hand",
                                                   "upper leg", "lower leg", "ankle", "foot", 'others', "not reported")),
                   
                   div(style="text-align:center",em('Others: head, pocket or bag, and tip of crutches.')),
                   
                   # Input: Choose context of wearable ----
                   checkboxGroupInput("wearable_context", label = "Choose a context for usage of sensor:",
                                      choices = list("real world" = "real-life", "lab", "mixed" = 'both'),
                                      selected = c("real-life", "lab", "both")),
                   
                   # Input: Choose context of wearable ----
                   checkboxGroupInput("wearable_domain", label = "Choose a domain for usage of sensor:",
                                      choices = list("Actigraphy", 'Qualitative gait', 'Balance', 'Dexterity/Tremor', 'Others'),
                                      selected = c("Actigraphy", 'Qualitative gait', 'Balance', 'Dexterity/Tremor', 'Others')),
                   
                   checkboxGroupInput("reported_results", label = "Choose the type of results reported:",
                                      choices = list("Association with MS severity" = "Correlation...association.with.clinical.MS.severity.scores..cross.sectional...e.g..EDSS..PDDS", 
                                                     "Association with other measures" = "Correlation...association.with.other.measure..cross.sectional.", 
                                                     "Test-retest reliability" = "Test.retest.reliability",
                                                     "Group difference MS vs HC" = "Group.differences..MS.vs.HC.",
                                                     "Group difference MS vs MS" = "Group.differences..MS.vs.MS.", 
                                                     "Group difference MS vs other diseases" = "Group.differences..MS.vs.other.diseases.",
                                                     "Responsiveness to change" = "Responsiveness.to.change",
                                                     "Responsiveness to intervention" = "Responsiveness.to.intervention..controlled.study.",
                                                     "Content validity" = "Content.validity..meaningfulness.to.patients.",
                                                     "None"),
                                      
                                      selected = c(
                                        "Correlation...association.with.clinical.MS.severity.scores..cross.sectional...e.g..EDSS..PDDS",
                                        "Correlation...association.with.other.measure..cross.sectional.",
                                        "Test.retest.reliability",
                                        "Group.differences..MS.vs.HC.",
                                        "Group.differences..MS.vs.MS.",
                                        "Group.differences..MS.vs.other.diseases.",
                                        "Responsiveness.to.change",
                                        "Responsiveness.to.intervention..controlled.study.",
                                        "Content.validity..meaningfulness.to.patients.",
                                        "None"
                                      ))
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
                                      valueBoxOutput("nb_reallife", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_lab", width = 12)),
                               column(3,
                                      valueBoxOutput("nb_mixed", width = 12))
                               
                             )),
                      #valueBoxOutput("nb_papers", width = 6),
                      
                      box(plotlyOutput("meta_results", height = "800px"), width = 12, height = "800px"),
                      
                      box(plotlyOutput("hist_years", height = "400px"), width = 12, height = "400px"),
                      
                      conditionalPanel(condition = "input.wearable_type.indexOf('accelerometer') > -1",
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


# Define server logic to display and download selected file ----
server <- function(input, output) {
  # Debounce checkboxGroupInput filter inputs so that multiple changes in selections are evaluated together
  wearable_type <- reactive(input$wearable_type)
  wearable_type_d <- wearable_type %>% debounce(1000)
  
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
    data <- data[grepl(paste(wearable_type_d(), collapse="|"), data$sensors_type_plot), ]
    data <- data[grepl(paste(wearable_position_d(), collapse="|"), data$Wearable), ]
    data <- data[grepl(paste(wearable_context_d(), collapse="|"), data$Context), ]
    
    merged.df <- inner_join(data, data_metaplot, by=c("DOI"))
    
    indices <- grep(paste(wearable_domain_d(), collapse="|"), merged.df$Domain1)
    indices <- append(indices, grep(paste(wearable_domain_d(), collapse="|"), merged.df$Domain2))
    indices <- append(indices, grep(paste(wearable_domain_d(), collapse="|"), merged.df$Domain3))
    indices <- append(indices, grep(paste(wearable_domain_d(), collapse="|"), merged.df$Domain4))
    indices <- sort(unique(indices))
    
    merged.df <- merged.df[indices, ]
    
    if ("None" %in% reported_results_d()){
      results_selected <- reported_results_d()[!reported_results_d() %in% c('None')]
      merged.df <- rbind(merged.df %>% filter_at(vars(results_selected), any_vars(. %in% c('yes'))),
                         merged.df %>% filter_at(vars(result_columns), all_vars(. %in% c('no'))))
    } else {
      merged.df <- merged.df %>% filter_at(vars(reported_results_d()), any_vars(. %in% c('yes')))
    }
    return(merged.df)
  })
  
  filtered_data_combined_2020_2021 <- reactive({
    data <- filtered_data()
    data$Year[data$Year == 2020] <- "2020-2021*"
    data$Year[data$Year == 2021] <- "2020-2021*"
    return(data)
  })
  
  # Table of selected dataset ----
  output$table <- renderDataTable({
    merged.df <- filtered_data()
    
    merged.df$DOI <- paste0("https://doi.org/", merged.df$DOI, sep='')
    merged.df$`First author and year` <- paste0("<a href='", merged.df$DOI,"' target='_blank'>", merged.df$`First author and year`, "</a>")
    
    # Remove unnecessary columns
    x <- names(merged.df)
    col_to_select <- x[! x %in% c('Authors', 'Year', 'DOI', 'sensors_type_plot', 'axes', 'X', colnames(data_metaplot))]
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
  
  output$nb_reallife <- renderValueBox({
    merged.df = filtered_data()
    
    nb_realsetting <- sum(merged.df$Context == 'real-life setting')
    
    valueBox(paste0(nb_realsetting, " papers"),
             "conducted in a real world setting",
             icon = icon("house-user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_lab <- renderValueBox({
    merged.df = filtered_data()
    
    nb_lab <- sum(merged.df$Context == 'lab setting (controlled)')
    
    valueBox(paste0(nb_lab, " papers"),
             "conducted in a lab setting",
             icon = icon("search"), 
             color = "yellow", 
             width = 12)
  })
  
  output$nb_mixed <- renderValueBox({
    merged.df = filtered_data()
    
    nb_mixed <- sum(merged.df$Context == 'both')
    
    valueBox(paste0(nb_mixed, " papers"),
             "conducted in mixed setting",
             icon = icon("user"), 
             color = "yellow", 
             width = 12)
  })
  
  output$hist_years <- renderPlotly({
    
    data_nb_wearable <- prepare_data_nb_wearable(data = data_raw,
                                                 data_metaplot = data_metaplot,
                                                 wearable = wearable_type_d(),
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
      layout(title = list(text = '<b>Number of accelerometers classified by the number of axes, per year of publication of the study<b>', font = list(size = 14)),
        yaxis = list(title = 'Number of publications per accelerometer type**'), 
        xaxis = list(title = 'Year of publication'),
        barmode = 'stack')

    
  })
  
  output$meta_results <- renderPlotly({
    merged.df = filtered_data()
    
    columns = c(
      "Correlation...association.with.clinical.MS.severity.scores..cross.sectional...e.g..EDSS..PDDS",
      "Correlation...association.with.other.measure..cross.sectional.",
      "Test.retest.reliability",
      "Group.differences..MS.vs.HC.",
      "Group.differences..MS.vs.MS.",
      "Group.differences..MS.vs.other.diseases.",
      "Responsiveness.to.change",
      "Responsiveness.to.intervention..controlled.study.",
      "Content.validity..meaningfulness.to.patients."
    )
    
    columns_names = c(
      "Association with \n clinical severity score",
      "Association with \n other measure",
      "Test-retest \n reliability",
      "Group differences \n (MS vs HC)",
      "Group differences \n (MS vs MS)",
      "Group differences \n (MS vs other diseases)",
      "Responsiveness to \n change",
      "Responsiveness to \n intervention (controlled study)",
      "Content validity \n (meaningfulness to patients)"
    )
    
    domains = c("RW: Actigraphy", "RW: Qualitative gait", "RW: Dexterity/Tremor", "RW: Other", "Lab: Actigraphy", "Lab: Qualitative gait", "Lab: Balance", "Lab: Dexterity/Tremor")
    effects = c("not-tested", "non-significant", "mixed", "significant")
    
    plot_data = data.frame()
    
    for (column in columns) {
      total = sum(merged.df[column] == "yes")
      for (domain in domains) {
        for (effect in effects) {
          count = sum(substr(merged.df[apply(merged.df, 1, function(row) domain %in% row[paste0("Domain", 1:6)]), paste0(column, "...Effect")], 0, nchar(effect)) == effect)
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
    plot_data_sub$column = factor(plot_data_sub$column, levels=columns, labels=columns_names)
    
    plot <- ggplot(plot_data_sub, aes(domain, count, fill=effect, label=count)) + 
      geom_bar(stat="identity", position=position_stack(reverse=T)) + 
      geom_text(data=plot_data_sub[plot_data_sub$count>0,], size = 3, position=position_stack(vjust=0.5, reverse=T)) + #, aes(color = effect), show.legend = FALSE) +
      #scale_color_manual(values = c("black", "black", "black", "white")) + 
      facet_wrap(facets=vars(column), scales="free_x", nrow=3, ncol=3, drop=FALSE) +
      theme_pubclean() + xlab(NULL) + ylab(NULL) + coord_flip() + scale_fill_manual(values=c("#d9d9d9", "#fdbf6f", "#96c3dc", "#1b63a5")) +
      theme(panel.grid.major.x=element_line(size=.1, linetype=2, color="black"), 
            panel.grid.major.y=element_blank(),
            legend.title = element_blank())
    
    ggplotly(plot) %>%
      facet_strip_bigger() %>%
      layout(title = list(text = '<b>Results reported by domain and context studied<b>', font = list(size = 14), y=.95),
             margin = list(l = 75, t = 100))
      

    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)