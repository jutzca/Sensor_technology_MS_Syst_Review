
prepare_data_nb_wearable <- function(data, data_metaplot, wearable, position, context, domain, results){
  
  result_columns <- c(
    "Correlation...association.with.clinical.MS.severity.scores..cross.sectional...e.g..EDSS..PDDS",
    "Correlation...association.with.other.measure..cross.sectional.",
    "Group.differences..MS.vs.HC.",
    "Group.differences..MS.vs.MS.",
    "Group.differences..MS.vs.other.diseases.",
    "Test.retest.reliability",
    "Responsiveness.to.change",
    "Responsiveness.to.intervention..controlled.study.",
    "Content.validity..meaningfulness.to.patients."
  )
  
  data$Year[data$Year == 2020] <- "2020-2021*"
  data$Year[data$Year == 2021] <- "2020-2021*"
  data <- data[grepl(paste(wearable, collapse="|"), data$sensors_type_plot), ]
  data <- data[grepl(paste(position, collapse="|"), data$Wearable), ]
  data <- data[grepl(paste(context, collapse="|"), data$Context), ]
  
  merged.df <- merge(data, data_metaplot, by=c("DOI"))
  
  indices <- grep(paste(domain, collapse="|"), merged.df$Domain1)
  indices <- append(indices, grep(paste(domain, collapse="|"), merged.df$Domain2))
  indices <- append(indices, grep(paste(domain, collapse="|"), merged.df$Domain3))
  indices <- append(indices, grep(paste(domain, collapse="|"), merged.df$Domain4))
  indices <- sort(unique(indices))
  
  merged.df <- merged.df[indices, ]
  
  if ("None" %in% results){
    results_selected <- results[!results %in% c('None')]
    merged.df <- rbind(merged.df %>% filter_at(vars(results_selected), any_vars(. %in% c('yes'))),
                       merged.df %>% filter_at(vars(result_columns), all_vars(. %in% c('no'))))
  } else {
    merged.df <- merged.df %>% filter_at(vars(results), any_vars(. %in% c('yes')))
  }
  
  counts_combined_fig2=data.frame(Year=unique(merged.df$Year))
  
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
  
  # Count all appearances of smartphone/touchscreen per year
  if ('smartphone/touchscreen' %in% wearable){
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
                                  "smartphone/touchscreen" = 'total_touchscreen')
  }
  
  # Count all appearances of other wearables used per year
  if ('others' %in% wearable){
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
  #                                    'magnetometer', 'smartphone/touchscreen', 
  #                                    'others')
  
  counts_combined_fig2 = rename(counts_combined_fig2, 'year' = "Year")
  
  return (counts_combined_fig2)
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