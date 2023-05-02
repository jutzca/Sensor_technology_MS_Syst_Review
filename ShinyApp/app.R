suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr)) # pivot_longer (alternative to melt)
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggpubr)) # theme_pubclean
suppressPackageStartupMessages(library(shinyWidgets)) # sliderTextInput, pickerInput

# Only set to TRUE when data source changes
SAVE_FIGURES <- FALSE
SANITY_CHECK <- FALSE

data <- read.csv("extracted_data_new.csv", check.names = F, na.strings = c("NA", "not reported"))

if (SANITY_CHECK) stopifnot(duplicated(tolower(data$DOI)) == F)
# data <- read.csv("23-02-23 extracted_data_new.csv", check.names = F, na.strings = c("NA", "not reported"))

# Extract number of wearables from 'Wearable' summary column by adding the 'Number of wearables: ' fields
wearables <- strsplit(data$Wearable, "\n\n")
data$number_wearables <- sapply(lapply(wearables, function(x) as.integer(substr(gsub("\n.*", "", gsub(".*Number of wearables: ", "", x)), 0, 1))), sum, na.rm = T)


sensor_types <- c("accelerometer", "gyroscope", "magnetometer", "touchscreen", "mechanical pedometer", "others")
wearable_positions <- c("sternum", "upper back", "lower back", "waist", "upper arm", "lower arm", "wrist", "hand", "upper leg", "lower leg", "ankle", "foot", "others", "not reported")
wearable_contexts <- c("real-world", "laboratory", "mixed")
if (SANITY_CHECK) stopifnot(names(table(data$Context)) %in% wearable_contexts)
wearable_domains <- c("Physical activity", "Gait", "Balance", "Dexterity/Tremor")
if (SANITY_CHECK) stopifnot(names(table(unlist(data[c("Domain1", "Domain2", "Domain3", "Domain4")]))) %in% c("", paste0(c("Lab: ", "RW: "), rep(wearable_domains, each = 2))))
# Explore brands
# sort(table(unlist(sapply(strsplit(data$Wearable, "\n\n"), function(x) gsub(" .*", "", gsub("\n.*", "", x))))))
wearable_brands <- c("ActiGraph", "APDM", "Axivity", "Bring-your-own-smartphone", "Fitbit", "G-Sensor", "Samsung", "Shimmer", "StepWatch", "Xsens", "Yamax", "Others")
result_columns <- c("Association with MS severity", "Association with other measure", "Test-retest reliability", "Group differences MS vs HC", "Group differences MS vs MS", "Group differences MS vs other diseases", "Responsiveness to change", "Responsiveness to intervention", "Subjective participant acceptability")
if (SANITY_CHECK) stopifnot(names(table(unlist(data[, result_columns]))) %in% c("no", "yes"))
if (SANITY_CHECK) stopifnot(names(table(unlist(data[, paste0(result_columns, " - Effect")]))) %in% c("", "significance not tested", "non-significant", "some significant", "significant"))

result_types <- c(result_columns, "Others")

data <- data[c(
  "Authors", "Year", "First author and year", "Title", "DOI", "Number of MS patients", "Thereof female, n", "Age", "Type of MS", "Severity", "Duration of disease in years", "Treatments", "Comorbidities", "Comparator population, number (number of females)", "Wearable", "Context", "Duration wearable was worn",
  "Domain1", "Domain2", "Domain3", "Domain4", result_columns, paste0(result_columns, " - Effect"), "number_wearables"
)]

# Define filter functions here ...
filter_by_year <- function(data, years) data[between(data$Year, years[1], years[2]), ]
filter_by_sensor_type <- function(data, sensor_types) data[grepl(paste0("Type of sensor: [^\n]*", paste0(sensor_types, collapse = "|")), data$Wearable), ]
filter_by_wearable_position <- function(data, wearable_position) data[grepl(paste0("Position: [^\n]*", paste0(wearable_position, collapse = "|")), data$Wearable), ]
filter_by_wearable_context <- function(data, wearable_context) data[grepl(paste0(wearable_context, collapse = "|"), data$Context), ]

filter_by_wearable_domain <- function(data, wearable_domain) {
  indices <- grep(paste0(wearable_domain, collapse = "|"), data$Domain1)
  indices <- append(indices, grep(paste0(wearable_domain, collapse = "|"), data$Domain2))
  indices <- append(indices, grep(paste0(wearable_domain, collapse = "|"), data$Domain3))
  indices <- append(indices, grep(paste0(wearable_domain, collapse = "|"), data$Domain4))
  indices <- sort(unique(indices))

  data[indices, ]
}

filter_by_wearable_brand <- function(data, wearable_brand) {
  # lapply(wearables, function (x) grepl(paste0(gsub("(", "\\(", gsub(")", "\\)", wearable_brand, fixed=T), fixed=T), collapse = "|"), x))
  if ("Others" %in% wearable_brand) {
    selected <- data[!grepl(paste0(gsub("(", "\\(", gsub(")", "\\)", wearable_brands[!wearable_brands %in% c("Others")], fixed = T), fixed = T), collapse = "|"), data$Wearable), ]
  } else {
    selected <- data.frame()
  }
  wearable_brand_wo_others <- wearable_brand[!wearable_brand %in% c("Others")]
  if (length(wearable_brand_wo_others)) {
    selected <- rbind(selected, data[grepl(paste0(gsub("(", "\\(", gsub(")", "\\)", wearable_brand_wo_others, fixed = T), fixed = T), collapse = "|"), data$Wearable), ])
  }
  selected
}

filter_by_result_types <- function(data, result_type) {
  if ("Others" %in% result_type) {
    selected <- data[rowMeans(data[result_columns] == "no") == 1, ] # data %>% filter_at(vars(result_columns), all_vars(. %in% c("no")))
  } else {
    selected <- data.frame()
  }
  result_type_wo_others <- result_type[!result_type %in% c("Others")]
  if (length(result_type_wo_others)) {
    # selected <- rbind(selected, data %>% filter_at(vars(result_type_wo_others), any_vars(. %in% c("yes"))))
    selected <- rbind(selected, data[rowMeans(data[result_type_wo_others] == "yes") > 0, ])
  }
  selected
}

# ...and add total number of publications behind each filter option in sidebar
for (i in 1:length(sensor_types)) {
  names(sensor_types)[i] <- paste0(sensor_types[i], " (", nrow(filter_by_sensor_type(data, sensor_types[i])), ")")
}

for (i in 1:length(wearable_positions)) {
  names(wearable_positions)[i] <- paste0(wearable_positions[i], " (", nrow(filter_by_wearable_position(data, wearable_positions[i])), ")")
}

for (i in 1:length(wearable_contexts)) {
  names(wearable_contexts)[i] <- paste0(wearable_contexts[i], " (", nrow(filter_by_wearable_context(data, wearable_contexts[i])), ")")
}

for (i in 1:length(wearable_domains)) {
  names(wearable_domains)[i] <- paste0(wearable_domains[i], " (", nrow(filter_by_wearable_domain(data, wearable_domains[i])), ")")
}

for (i in 1:length(wearable_brands)) {
  names(wearable_brands)[i] <- paste0(wearable_brands[i], " (", nrow(filter_by_wearable_brand(data, wearable_brands[i])), ")")
}

for (i in 1:length(result_types)) {
  names(result_types)[i] <- paste0(result_types[i], " (", nrow(filter_by_result_types(data, result_types[i])), ")")
}

# Debug
input <- list(year_publication = c(1997, 2022), sensor_type = sensor_types, wearable_position = wearable_positions, wearable_context = wearable_contexts, wearable_domain = wearable_domains, result_type = result_types, wearable_brand = wearable_brands, barplot_domain_proportion = F, barplot_sensor_type_proportion = F, barplot_accelerometer_axes_proportion = F, barplot_publications_number_patients_proportion = F, barplot_publications_context_proportion = F, barplot_publications_positions_proportion = F, barplot_publications_number_wearables_proportion = F)
year_publication_d <- function() input$year_publication
sensor_type_d <- function() input$sensor_type
wearable_position_d <- function() input$wearable_position
wearable_context_d <- function() input$wearable_context
wearable_domain_d <- function() input$wearable_domain
wearable_brand_d <- function() input$wearable_brand
result_type_d <- function() input$result_type

filtered_data <- function() {
  data <- filter_by_sensor_type(data, sensor_type_d())
  data <- filter_by_wearable_position(data, wearable_position_d())
  data <- filter_by_wearable_context(data, wearable_context_d())
  data <- filter_by_year(data, year_publication_d())
  data <- filter_by_wearable_domain(data, wearable_domain_d())
  data <- filter_by_wearable_brand(data, wearable_brand_d())
  data <- filter_by_result_types(data, result_type_d())
  data
}

# Make sure that in default filter context all publications are actually selected
if (SANITY_CHECK) stopifnot(nrow(data) == nrow(filtered_data()))

create_barplot <- function(data, fill, fill_var, proportion = F) {
  # Expects data to have 3 columns: 'year', 'count', fill

  colors <- c("#D9D9D9", "#FFFF99", "#FF7F00", "#FDBF6F", "#A6CEE3", "#1F78B4")[(7 - length(unique(data[, fill]))):6]

  # Add a year to create a space between 1997 and 2006
  data <- rbind(data, c(1998, NA, NA))

  data$year <- as.factor(data$year)

  ggplot(data, aes_string(x = "year", y = "count", fill = fill)) +
    geom_bar(stat = "identity", colour = "black", position = ifelse(proportion, "fill", "stack")) +
    scale_fill_manual(values = colors) +
    theme_classic() +
    xlab("Year of publication") +
    ylab(paste0(ifelse(proportion, "Proportion", "Number"), " of publications")) +
    labs(fill = fill_var) +
    geom_vline(xintercept = 2, linetype = 2) +
    scale_x_discrete(breaks = data$year[data$year != 1998]) +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.1, color = "black"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      legend.position = c(0.25, 0.8)
    )
}

# Define UI

css <- "
/* fix background-color of wrapper (is black otherwise for some reason) */
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
}"

ui <- dashboardPage(
  dashboardHeader(title = "MS Wearable Sensors - A Review", titleWidth = 340),

  # Sidebar layout with input and output definitions
  dashboardSidebar(
    width = 320,
    div(style = "text-align:center", em("Papers satisfying at least one selected criterion will be presented (inclusive).")),

    # Input: Choose year(s) of publication
    sliderTextInput(
      inputId = "year_publication", # create new slider text
      label = "Years of publication:", # label of the box
      choices = list(
        "1997" = 1997, "2006" = 2006, "2007" = 2007, "2008" = 2008, "2009" = 2009,
        "2010" = 2010, "2011" = 2011, "2012" = 2012, "2013" = 2013, "2014" = 2014,
        "2015" = 2015, "2016" = 2016, "2017" = 2017, "2018" = 2018, "2019" = 2019,
        "2020" = 2020, "2021" = 2021, "2022" = 2022
      ),
      selected = c(1997, 2022),
      animate = F, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
      to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
      to_max = NULL, force_edges = T, width = NULL, pre = NULL,
      post = NULL, dragRange = TRUE
    ),

    # Input: Choose context of wearable
    pickerInput(
      "wearable_context",
      label = "Choose the contexts:",
      choices = wearable_contexts,
      selected = wearable_contexts,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),

    # Input: Choose domain
    pickerInput(
      "wearable_domain",
      label = "Choose the domains studied:",
      choices = wearable_domains,
      selected = wearable_domains,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),

    # Input: Choose type of results
    pickerInput(
      "result_type",
      label = "Choose the types of results reported:",
      choices = result_types,
      selected = result_types,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),

    # Input: Choose type of sensor
    pickerInput(
      "sensor_type",
      label = "Choose the types of sensors: *",
      choices = sensor_types,
      selected = sensor_types,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),

    # Input: Choose position of wearable
    pickerInput(
      "wearable_position",
      label = "Choose the positions for the wearables: **",
      choices = wearable_positions,
      selected = wearable_positions,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),

    # Input: Choose brand
    pickerInput(
      "wearable_brand",
      label = "Wearable brands used most often (≥5):",
      choices = wearable_brands,
      selected = wearable_brands,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),
    hr(),
    div(
      style = "text-align:center",
      em("* Other sensor types: electrocardiogram (ECG), global positioning system (GPS), surface electromyography (sEMG), portable metabolic system (VO2), skin impedance, force sensor, barometer, thermometer")
    ),
    div(
      style = "text-align:center",
      em("** Other positions: head, pocket, bag, chest, crutches")
    )
  ),
  dashboardBody(
    tags$head(tags$style(css)),
    tabBox(
      width = 12,
      tabPanel(
        "Contexts and domains",
        fluidRow(
          column(
            12,
            column(3, valueBoxOutput("nb_papers", width = 12)),
            column(3, valueBoxOutput("nb_realworld", width = 12)),
            column(3, valueBoxOutput("nb_lab", width = 12)),
            column(3, valueBoxOutput("nb_mixed", width = 12))
          ),
          box(
            h3("Publications per year by context"),
            radioButtons("barplot_publications_context_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_publications_context", height = "350px"),
            width = 12
          ),
          box(
            h3("Publications per year by domain *"),
            radioButtons("barplot_domain_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_domain", height = "350px"),
            width = 12
          ),
          p("* Publications satisfying multiple criteria count multiple times")
        )
      ),
      tabPanel(
        "Types of results",
        fluidRow(
          box(
            h3("Number of publications by types of results per context and domain *"),
            plotlyOutput("meta_results", height = "800px"),
            width = 12
          ),
          p("* Publications satisfying multiple criteria count multiple times")
        )
      ),
      tabPanel(
        "Type of sensor and position",
        fluidRow(
          box(
            h3("Publications per year by type of sensor *"),
            radioButtons("barplot_sensor_type_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_sensor_type", height = "350px"),
            width = 12
          ),
          box(
            h3("Publications per year by type of accelerometer (number of axes) *"),
            radioButtons("barplot_accelerometer_axes_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_accelerometer_axes", height = "350px"),
            width = 12
          ),
          box(
            h3("Publications per year by position *"),
            radioButtons("barplot_publications_positions_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_publications_positions", height = "350px"),
            width = 12
          ),
          box(
            h3("Publications per wearable position per context *"),
            plotOutput("donut_publications_context_position"),
            width = 12
          ),
          p("* Publications satisfying multiple criteria count multiple times"),
        )
      ),
      tabPanel(
        "Supplementary",
        fluidRow(
          box(
            h3("Publications per year by number of MS patients"),
            radioButtons("barplot_publications_number_patients_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_publications_number_patients"),
            width = 12
          ),
          box(
            h3("Publications per year by number of wearables"),
            radioButtons("barplot_publications_number_wearables_proportion", "", c("Show numbers" = F, "Show proportions" = T), selected = F, inline = T),
            plotlyOutput("barplot_publications_number_wearables"),
            width = 12
          ),
          box(
            h3("Publications per number of wearables per context"),
            plotOutput("barplot_number_wearables"),
            width = 12
          )
        )
      ),
      tabPanel("Papers Table",
        dataTableOutput("table"),
        style = "overflow-x: scroll"
      )
    )
  )
)

# Define server logic to display and download selected file
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

  wearable_brand <- reactive(input$wearable_brand)
  wearable_brand_d <- wearable_brand %>% debounce(1000)

  result_type <- reactive(input$result_type)
  result_type_d <- result_type %>% debounce(1000)

  year_publication <- reactive(input$year_publication)
  year_publication_d <- year_publication %>% debounce(1000)

  filtered_data <- reactive({
    data <- filter_by_sensor_type(data, sensor_type_d())
    data <- filter_by_wearable_position(data, wearable_position_d())
    data <- filter_by_wearable_context(data, wearable_context_d())
    data <- filter_by_year(data, year_publication_d())
    data <- filter_by_wearable_domain(data, wearable_domain_d())
    data <- filter_by_wearable_brand(data, wearable_brand_d())
    data <- filter_by_result_types(data, result_type_d())

    return(data)
  })

  # Table of selected dataset
  output$table <- renderDataTable({
    data <- filtered_data()



    highlight_first_row <- function(x) {
      sapply(sapply(strsplit(x, "\n\n"), function(x) sub("([^\n]*)\n", "<b>\\1</b>\n", x)), paste, collapse = "\n\n")
    }

    abbr <- c("significance not tested" = "ns", "non-significant" = "ns", "some significant" = "ss", "significant" = "s")

    data$`First author and year` <- paste0(data$`First author and year`, "<br><sup><a href='https://doi.org/", data$DOI, "' target='_blank'>", data$DOI, "</a></sup>")
    # data$ = gsub("\n", "<br><sup>", highlight_first_row(paste0(data$`First author and year`, "\n[", data$DOI, "](https://doi.org/", data$DOI, ")</sup>"))),
    data$`MS Population` <- paste0("n=", data$`Number of MS patients`, ifelse(!is.na(data$`Thereof female, n`), paste0(" (", round(data$`Thereof female, n` / data$`Number of MS patients` * 100), "% female)"), ""), "<br>age: ", data$Age, "<br><br>Type: ", data$`Type of MS`)
    data$`Severity & Disease Duration` <- paste0(ifelse(!is.na(data$Severity), paste0("<b>Severity:</b> ", data$Severity), ""), ifelse(!is.na(data$`Duration of disease in years`), paste0("<br><b>Disease duration:</b> ", data$`Duration of disease in years`), ""))
    data$`Treatments & Comorbidities` <- paste0(ifelse(!is.na(data$Treatments), paste0("<b>Treatments:</b> ", data$Treatments), ""), ifelse(!is.na(data$Comorbidities), paste0("<br><b>Comorbidities:</b> ", data$Comorbidities), ""))
    data$`Comparator Population` <- gsub("\n", "<br>", highlight_first_row(data$`Comparator population, number (number of females)`))
    data$Wearable <- gsub("\n", "<br>", highlight_first_row(gsub("Type of sensor: ", "", gsub("Number of axes: 1", "1 axis", gsub("Number of axes: (2|3)", "\\1 axes", gsub("Number of wearables: ([0-9]+)", "\\1 wearable(s)", data$Wearable))))))
    data$Results <- gsub("\n", "<br>", gsub("()", "(other type of result)", paste0(
      "<b>", gsub(",$", "", trimws(gsub(" ,", "", gsub(", ,", "", apply(data, 1, function(x) paste(x["Domain1"], x["Domain2"], x["Domain3"], x["Domain4"], sep = ", ")))))), "</b>\n",
      sapply(apply(data, 1, function(x) {
        return(paste0(result_columns[x[result_columns] == "yes"], " (", abbr[x[paste0(result_columns, " - Effect")][x[result_columns] == "yes"]], ")"))
      }), paste, collapse = "\n")
    ), fixed = T))

    cols_to_select <- c("Year", "First author and year", "Title", "MS Population", "Severity & Disease Duration", "Treatments & Comorbidities", "Comparator Population", "Wearable", "Results")

    datatable(data[order(data$Year, data$Authors, data$Title, data$DOI), cols_to_select], rownames = FALSE, escape = F, filter = "top", extensions = "Buttons", selection = "none", options = list(
      dom = "Bfrti",
      buttons = c("copy", "csv", "excel", "colvis"),
      pageLength = 1000
    ))
  })

  output$nb_papers <- renderValueBox({
    valueBox(
      paste0(nrow(filtered_data()), " papers"),
      "meeting your input criteria",
      icon = icon("scroll"),
      color = "yellow",
      width = 12
    )
  })

  output$nb_realworld <- renderValueBox({
    valueBox(
      paste0(sum(filtered_data()$Context == "real-world"), " papers"),
      "conducted in a real-world context (RW)",
      icon = icon("house-user"),
      color = "yellow",
      width = 12
    )
  })

  output$nb_lab <- renderValueBox({
    valueBox(
      paste0(sum(filtered_data()$Context == "laboratory"), " papers"),
      "conducted in a laboratory context",
      icon = icon("search"),
      color = "yellow",
      width = 12
    )
  })

  output$nb_mixed <- renderValueBox({
    valueBox(
      paste0(sum(filtered_data()$Context == "mixed"), " papers"),
      "conducted in a mixed context",
      icon = icon("user"),
      color = "yellow",
      width = 12
    )
  })

  output$meta_results <- renderPlotly({
    domains <- c(paste0("RW: ", wearable_domains), paste0("Lab: ", wearable_domains))
    effects <- c("significance not tested", "non-significant", "some significant", "significant")

    plot_data <- data.frame()

    for (column in result_columns) {
      total <- sum(filtered_data()[column] == "yes")
      for (domain in domains) {
        for (effect in effects) {
          count <- sum(substr(filtered_data()[apply(filtered_data(), 1, function(row) domain %in% row[paste0("Domain", 1:6)]), paste0(column, " - Effect")], 0, nchar(effect)) == effect)
          plot_data <- rbind(plot_data, list(
            column = column,
            domain = domain,
            effect = effect,
            count = count,
            proportion = count / total
          ))
        }
      }
    }

    plot_data$domain <- factor(plot_data$domain, levels = rev(domains))
    plot_data$effect <- factor(plot_data$effect, levels = effects, labels = c("significance not tested", "non-significant", "some significant", "significant"))

    plot_data$column <- factor(plot_data$column, levels = result_columns, labels = result_columns)

    p <- ggplot(plot_data, aes(domain, count, fill = effect, label = count)) +
      geom_bar(stat = "identity", position = position_stack(reverse = T)) +
      geom_text(data = plot_data[plot_data$count > 0, ], size = 3, position = position_stack(vjust = 0.5, reverse = T), color = rep(c("black", "black", "black", "white"), times = nrow(plot_data) / 4)[plot_data$count > 0]) +
      facet_wrap(facets = vars(column), scales = "free_x", nrow = 3, ncol = 3, drop = FALSE) +
      theme_pubclean() +
      xlab(NULL) +
      ylab(NULL) +
      coord_flip() +
      scale_fill_manual(values = c("#d9d9d9", "#fdbf6f", "#96c3dc", "#1b63a5")) +
      # https://stackoverflow.com/questions/15622001/how-to-display-only-integer-values-on-an-axis-using-ggplot2
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      theme(
        panel.grid.major.x = element_line(linewidth = .1, linetype = 2, color = "black"),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank()
      )

    if (SAVE_FIGURES) ggsave("../Figures/Figure 3 Results reported by domain and context studied.png", p, width = 10, height = 10)
    ggplotly(p)
  })

  output$barplot_domain <- renderPlotly({
    yearly_publication_counts_wearable_domain <- data.frame()
    for (year in unique(data$Year)) {
      for (domain in wearable_domains) {
        yearly_publication_counts_wearable_domain <- rbind(yearly_publication_counts_wearable_domain, list(
          year = year,
          domain = domain,
          count = nrow(filter_by_wearable_domain(filter_by_year(filtered_data(), c(year, year)), domain))
        ))
      }
    }

    p <- create_barplot(yearly_publication_counts_wearable_domain, "domain", "domain", input$barplot_domain_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Wearable domains over time.png", p, width = 8, height = 6)
    ggplotly(p)
  })

  output$barplot_sensor_type <- renderPlotly({
    # Count number of sensor types in "Wearable" summary column
    yearly_publication_counts_sensor_type <- data.frame()
    for (year in unique(data$Year)) {
      for (sensor in sensor_types) {
        yearly_publication_counts_sensor_type <- rbind(yearly_publication_counts_sensor_type, list(
          year = year,
          sensors = sensor,
          count = nrow(filter_by_sensor_type(filter_by_year(filtered_data(), c(year, year)), sensor))
        ))
      }
    }

    # Change level order in the sensor column
    # such that the majority group (accelerometers) is plotted at the bottom
    yearly_publication_counts_sensor_type$sensors <- factor(yearly_publication_counts_sensor_type$sensors, levels = rev(sensor_types))

    p <- create_barplot(yearly_publication_counts_sensor_type, "sensors", "type of sensor", input$barplot_sensor_type_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Figure 2A Type of sensor over time.png", p, width = 8, height = 6)
    ggplotly(p)
  })

  output$barplot_accelerometer_axes <- renderPlotly({
    # Count number of publications with 1/2/3 axis-accelerometers from "Wearable" summary column
    yearly_publication_counts_acc_axes <- data.frame()
    for (year in unique(data$Year)) {
      for (axes in 1:3) {
        yearly_publication_counts_acc_axes <- rbind(yearly_publication_counts_acc_axes, list(
          year = year,
          axes = axes,
          count = sum(grepl(paste0("accelerometer[^\n]*\nNumber of axes: ", axes), filter_by_year(filtered_data(), c(year, year))$Wearable))
        ))
      }
    }

    yearly_publication_counts_acc_axes$axes <- as.factor(yearly_publication_counts_acc_axes$axes)

    p <- create_barplot(yearly_publication_counts_acc_axes, "axes", "number of axes", input$barplot_accelerometer_axes_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Figure 2B Type of accelerometer over time.png", p, width = 8, height = 6)
    ggplotly(p)
  })

  output$donut_publications_context_position <- renderPlot({
    plots <- list()
    for (pos in wearable_positions) {
      data_temp <- data.frame(
        context = factor(wearable_contexts, levels = wearable_contexts),
        count = c(
          nrow(filter_by_wearable_position(filter_by_wearable_context(filtered_data(), wearable_contexts[1]), pos)),
          nrow(filter_by_wearable_position(filter_by_wearable_context(filtered_data(), wearable_contexts[2]), pos)),
          nrow(filter_by_wearable_position(filter_by_wearable_context(filtered_data(), wearable_contexts[3]), pos))
        )
      )

      # Compute percentages
      data_temp$fraction <- data_temp$count / sum(data_temp$count)
      data_temp$percent <- round((data_temp$fraction) * 100, 1)

      # Compute the cumulative percentages (top of each rectangle)
      data_temp$ymax <- cumsum(data_temp$fraction)

      # Compute the bottom of each rectangle
      data_temp$ymin <- c(0, head(data_temp$ymax, n = -1))

      # Make the plot without percentages of each context
      plots[[pos]] <- ggplot(data_temp, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = context)) +
        geom_rect() +
        geom_text(x = -1, y = 0.25, label = paste0(pos, "\n n=", sum(data_temp$count)), size = 4) +
        scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#FDBF6F")) +
        # scale_color_brewer(palette=1) +
        coord_polar(theta = "y") +
        xlim(c(-1, 4)) +
        theme_void()
    }
    p <- ggarrange(plotlist = plots, ncol = 7, nrow = 2, common.legend = T) + bgcolor("white")
    if (SAVE_FIGURES) ggsave("../Figures/Figure 2C Wearable position context.png", p, width = 10, height = 4)
    p
  })

  output$barplot_publications_context <- renderPlotly({
    yearly_publication_counts_context <- data.frame(filtered_data() %>%
      group_by(Year, Context) %>%
      count())

    colnames(yearly_publication_counts_context) <- c("year", "context", "count")

    yearly_publication_counts_context$context <- factor(yearly_publication_counts_context$context, levels = rev(wearable_contexts))

    p <- create_barplot(yearly_publication_counts_context, "context", "context", input$barplot_publications_context_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Context over time.png", p, width = 8, height = 6)
    p
  })

  output$barplot_publications_positions <- renderPlotly({
    yearly_publication_counts_positions <- data.frame()
    for (year in unique(data$Year)) {
      for (position in c("waist", "lower extremities", "trunk", "upper extremities", "others", "not reported")) {
        positions <- position
        if (position == "trunk") positions <- c("lower back", "upper back", "sternum")
        if (position == "lower extremities") positions <- c("foot", "ankle", "lower leg", "upper leg")
        if (position == "upper extremities") positions <- c("hand", "wrist", "lower arm", "upper arm")
        yearly_publication_counts_positions <- rbind(yearly_publication_counts_positions, list(
          year = year,
          position = position,
          count = nrow(filter_by_wearable_position(filter_by_year(filtered_data(), c(year, year)), positions))
        ))
      }
    }

    yearly_publication_counts_positions$position <- factor(yearly_publication_counts_positions$position, rev(c("waist", "lower extremities", "trunk", "upper extremities", "others", "not reported")))

    p <- create_barplot(yearly_publication_counts_positions, "position", "positions", input$barplot_publications_positions_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Positions over time.png", p, width = 8, height = 6)
    p
  })

  output$hist_number_patients <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = get("Number of MS patients"))) +
      geom_histogram(color = "black", fill = "white", binwidth = 100) +
      # facet_grid(rows = vars(Year)) +
      facet_wrap(Year ~ ., ncol = 6) +
      theme_classic() +
      xlab("Number of PwMS included") +
      ylab("Number of publications") +
      theme(
        panel.grid.major.y = element_line(linewidth = .1, color = "black"),
        panel.grid.major.x = element_line(linewidth = .1, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
      )
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Number of MS patients over time.png", p, width = 8, height = 6)
    p
  })

  output$barplot_publications_number_patients <- renderPlotly({
    yearly_publication_counts_number_patients <- data.frame(filtered_data() %>% group_by(Year) %>%
      summarise(
        "<50 patients" = sum(`Number of MS patients` < 50),
        "<100 patients" = sum(between(`Number of MS patients`, 50, 99)),
        "<200 patients" = sum(between(`Number of MS patients`, 100, 199)),
        "<500 patients" = sum(between(`Number of MS patients`, 200, 499)),
        "≥500 patients" = sum(`Number of MS patients` >= 500)
      ))
    colnames(yearly_publication_counts_number_patients) <- c("year", "<50 patients", "<100 patients", "<200 patients", "<500 patients", "≥500 patients")
    yearly_publication_counts_number_patients <- as.data.frame(pivot_longer(yearly_publication_counts_number_patients, cols = !year, names_to = "number_bins", values_to = "count"))

    yearly_publication_counts_number_patients$number_bins <- factor(yearly_publication_counts_number_patients$number_bins, levels = rev(unique(yearly_publication_counts_number_patients$number_bins)))

    p <- create_barplot(yearly_publication_counts_number_patients, "number_bins", "number of MS patients", input$barplot_publications_number_patients_proportion)
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Number of MS patients over time.png", p, width = 8, height = 6)
    ggplotly(p)
  })

  output$barplot_publications_number_wearables <- renderPlotly({
    yearly_publication_counts_number_wearables <- data.frame(filtered_data() %>% group_by(Year) %>%
      summarise(
        "1 wearable" = sum(number_wearables == 1, na.rm = T),
        "2 wearables" = sum(number_wearables == 2, na.rm = T),
        "3 wearables" = sum(number_wearables == 3, na.rm = T),
        ">3 wearables" = sum(number_wearables > 3, na.rm = T)
      ))
    colnames(yearly_publication_counts_number_wearables) <- c("year", "1 wearable", "2 wearables", "3 wearables", ">3 wearables")
    yearly_publication_counts_number_wearables <- as.data.frame(pivot_longer(yearly_publication_counts_number_wearables, cols = !year, names_to = "number_bins", values_to = "count"))

    yearly_publication_counts_number_wearables$number_bins <- factor(yearly_publication_counts_number_wearables$number_bins, levels = rev(unique(yearly_publication_counts_number_wearables$number_bins)))

    p <- create_barplot(yearly_publication_counts_number_wearables, "number_bins", "number of wearables", input$barplot_publications_number_wearables_proportion)
    # if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Publications number of MS wearables over time.png", p, width=8, height=6)
    ggplotly(p)
  })

  output$barplot_number_wearables <- renderPlot({
    plot_data <- filtered_data()
    plot_data$Context <- factor(plot_data$Context, levels = wearable_contexts)

    p <- ggplot(plot_data, aes(x = number_wearables)) +
      geom_histogram(color = "black", fill = "white", binwidth = 1) +
      facet_grid(Context ~ .) +
      theme_classic() +
      xlab("Number of wearables") +
      ylab("Number of publications") +
      theme(
        panel.grid.major.y = element_line(linewidth = .1, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
      ) +
      stat_bin(binwidth = 1, geom = "text", aes(label = after_stat(count)), vjust = -1) +
      scale_x_continuous(breaks = c(1:max(plot_data$number_wearables))) +
      ylim(0, 165)
    if (SAVE_FIGURES) ggsave("../Figures/Multimedia Appendix Number wearables per context.png", p, width = 6, height = 6)
    p
  })
}

# Create Shiny app
shinyApp(ui, server)
