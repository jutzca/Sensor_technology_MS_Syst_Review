################################################################################
# Wearable sensor technology for patients with multiple sclerosis
# Does current knowledge drive future development
# A systematic review
# ------------------------------------------------------------------------------
# Code by L. Bourguignon
# Last update : 12.12.2022
# ------------------------------------------------------------------------------
# Figures
################################################################################

# Libraries --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)
library(ggsci)
library("viridis")

# Data loading -----------------------------------------------------------------
setwd('/Volumes/blucie/PhD/10_MS/')
data_figure2 <- read.csv('./Figures&tables_afterrevision/data_figure2_MS.csv')
data_meta <- read.csv('./ShinyApp_old/data_meta_figure.csv')

# Figure 2 --------------------------------------------------------------------
# Bar plot
# Results aggregates by domain of sensor, by type of result, by setting

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
  "Correlation / association with clinical MS severity scores (cross-sectional), e.g. EDSS, PDDS",
  "Correlation / association with other measure (cross-sectional)",
  "Test-retest reliability",
  "Group differences (MS vs HC)",
  "Group differences (MS vs MS)",
  "Group differences (MS vs other diseases)",
  "Responsiveness to change",
  "Responsiveness to intervention (controlled study)",
  "Content validity (meaningfulness to patients)"
)

domains = c("RW: Actigraphy", "RW: Qualitative gait", "RW: Dexterity/Tremor", "RW: Other", "Lab: Actigraphy", "Lab: Qualitative gait", "Lab: Balance", "Lab: Dexterity/Tremor")
effects = c("not-tested", "non-significant", "mixed", "significant")

plot_data = data.frame()

for (column in columns) {
  total = sum(data_meta[column] == "yes")
  for (domain in domains) {
    for (effect in effects) {
      count = sum(substr(data_meta[apply(data_meta, 1, function(row) domain %in% row[paste0("Domain", 1:6)]), paste0(column, "...Effect")], 0, nchar(effect)) == effect)
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

plot_data_sub = plot_data[plot_data$column %in% c(columns[1:4], columns[6:8]),]

plot_data$column = factor(plot_data$column, levels=columns, labels=c("Association with clinical severity score", 
                                                                             "Association with other measure", 
                                                                             columns_names[3:9]))

plot <- ggplot(plot_data, aes(domain, count, fill=effect, label=count)) + 
  geom_bar(stat="identity", position=position_stack(reverse=T)) + 
  geom_text(data=plot_data[plot_data$count>0,], size = 5, position=position_stack(vjust=0.5, reverse=T), aes(color = effect), show.legend = FALSE) +
  scale_color_manual(values = c("black", "black", "black", "white")) + 
  facet_wrap(facets=vars(column), scales="free_x", nrow=3, drop=T) +
  theme_pubclean() + xlab(NULL) + ylab(NULL) + coord_flip() + scale_fill_manual(values=c("#d9d9d9", "#fdbf6f", "#96c3dc", "#1b63a5")) +
  theme(panel.grid.major.x=element_line(size=.1, linetype=2, color="black"), 
        panel.grid.major.y=element_blank(),
        panel.spacing.y = unit(2, "lines"),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        strip.text.x = element_text(face = "bold", size=12),
        axis.text.y = element_text(size=12)
        )
plot

library(gtable)
g <- ggplotGrob(plot)
strips <- g$layout[grep("strip_t", g$layout$name), ]
titles <- lapply(paste0("(", letters[seq_len(nrow(strips))], ")"), 
                 textGrob, x = 0, hjust = 0, vjust = 1)
g <- gtable_add_grob(g, grobs = titles, 
                     t = strips$t, b = strips$b - 2, 
                     l = strips$l, r = strips$r)
grid.newpage()
grid.draw(g)



# Figure 2A --------------------------------------------------------------------
# Bar plot
# Number of sensors per year of publication per type
# Types include accelerometer, gyroscope, magnetometer, smartphone and others
# Others include [....]

data_figure2$year[data_figure2$year == 2020] <- "2020-2021*"
data_figure2$year[data_figure2$year == 2021] <- "2020-2021*"

# Count all appearances of accelerometers per year
accelerometer_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("accelerometer", sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
accelerometer_data2 <- accelerometer_data %>% mutate(total_accelerometer= sum(c_across(contains('accelerometer'))))

# Count all appearances of gyroscopes per year
gyroscope_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("gyroscope",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0)
gyroscope_data2 <- gyroscope_data %>% mutate(total_gyroscope= sum(c_across(contains('gyroscope'))))

# Count all appearances of magnetometers per year
magnetometer_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("magnetometer",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0)
magnetometer_data2 <- magnetometer_data %>% mutate(total_magnetometer= sum(c_across(contains('magnetometer'))))

magnetometer_data2 <- magnetometer_data %>% mutate(total_magnetometer = sum(c(`accelerometer, gyroscope, magnetometer`)))

# Count all appearances of smartphone/touchscreen per year
touchscreen_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("touchscreen",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
touchscreen_data2 <- touchscreen_data %>% mutate(total_touchscreen= sum(c_across(contains('touchscreen'))))

mechanical_pedometer_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("mechanical pedometer", sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
mechanical_pedometer_data2 <- mechanical_pedometer_data %>% mutate(total_mechanical_pedometer = sum(c_across(contains("mechanical pedometer"))))

# Count all appearances of other wearables used per year
others_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("others",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
others_data2 <- others_data %>% mutate(total_others= sum(c_across(contains('others'))))

# Combine counts of all types of wearables
counts_combined_fig2 <- accelerometer_data2[c('year', 'total_accelerometer')]
counts_combined_fig2 <- merge(counts_combined_fig2, 
                              gyroscope_data2[c('year', 'total_gyroscope')], 
                              by="year", all = T)
counts_combined_fig2 <- merge(counts_combined_fig2, 
                              magnetometer_data2[c('year', 'total_magnetometer')], 
                              by="year", all = T)
counts_combined_fig2 <- merge(counts_combined_fig2, 
                              touchscreen_data2[c('year', 'total_touchscreen')], 
                              by="year", all = T)
counts_combined_fig2 <- merge(counts_combined_fig2, 
                              mechanical_pedometer_data2[c('year', 'total_mechanical_pedometer')], 
                              by="year", all = T)
counts_combined_fig2 = rename(counts_combined_fig2, 
                              "mechanical pedometer" = 'total_mechanical_pedometer')
counts_combined_fig2 <- merge(counts_combined_fig2, 
                              others_data2[c('year', 'total_others')], 
                              by="year", all = T)
counts_combined_fig2[is.na(counts_combined_fig2)] <- 0
# Harmonise column names
colnames(counts_combined_fig2) <- c("year", "accelerometer", "gyroscope", 
                                    'magnetometer', 'touchscreen', 
                                    'mechanical pedometer', 'others')
# Reshape to long format
counts_combined_fig2_long <- melt(setDT(counts_combined_fig2), 
                                  id.vars = c("year"), 
                                  variable.name = "sensors")
# Add a year with no wearable to create a space between 1997 and 2006
matrix_temp <- as.data.frame(t(matrix(c(1998, NA, NA))))
names(matrix_temp) <- names(counts_combined_fig2_long)
counts_combined_fig2_long <- rbind(counts_combined_fig2_long, 
                                   matrix_temp)
# Change level order in the sensor column
# such that the majority group (accelerometers) is plotted at the bottom
counts_combined_fig2_long$sensors <- factor(counts_combined_fig2_long$sensors, 
                                            levels = c('others', 
                                                       'mechanical pedometer',
                                                       "touchscreen", 
                                                       'magnetometer', 
                                                       'gyroscope', 
                                                       'accelerometer'))


barplot_sensor <- ggplot(data=counts_combined_fig2_long, aes(x=factor(year), y=value, fill=sensors)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#FFFF99", '#d9d9d9', "#FF7F00", "#FDBF6F", "#1F78B4", "#A6CEE3")) + 
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications per sensor type') +
  labs(fill = "Type of sensor(s)") +
  geom_vline(xintercept = 2, linetype=2) + # add vertical line to separate 1997 and 2006
  scale_x_discrete(breaks=counts_combined_fig2_long$Year[as.character(counts_combined_fig2_long$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.25, 0.76)
  )

barplot_sensor

# Figure 2B --------------------------------------------------------------------
# Bar plot
# Number of accelerometer per year of publication per number of axis
# Numbers of axis ranged from 1 to 3, included

accelerometer_data_axis <- data %>%
  group_by(Year) %>% 
  filter(grepl("accelerometer", sensors_type_plot)) %>% 
  count(Year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 

accelerometer_data2_axis <- accelerometer_data_axis %>% 
  group_by(Year) %>%
  mutate(total = sum(c_across(contains("accelerometer"))))

accelerometer_data_axis_test <- data %>%
  group_by(Year) %>%
  summarise(`uniaxial` = str_count(Wearable, "Type of sensor: .*accelerometer.*\nNumber of axes: 1"),
            `biaxial` = str_count(Wearable, "Type of sensor: .*accelerometer.*\nNumber of axes: 2"),
            `triaxial` = str_count(Wearable, "Type of sensor: .*accelerometer.*\nNumber of axes: 3"))

accelerometer_data_axis_test <- aggregate(. ~ Year, data=accelerometer_data_axis_test, FUN=sum)


# Subset the accelerometer_data2_axis table to only the total number per year
data_all_axis <- accelerometer_data2_axis[c('year', 'axes', 'total')]
# Harmonise column names
colnames(data_all_axis) <- c("year","axis", "number")
# Add a year with no wearable to create a space between 1997 and 2006
data_all_axis <- rbind(data.frame(data_all_axis), c(1998, NA, NA))
#test <- reshape(data_all_axis, idvar = "year", timevar = "axis", direction = "wide")

data_long1 <- melt(accelerometer_data_axis_test,
                   id.vars = c("Year"))

data_all_axis <- accelerometer_data2_axis[c('year', 'axes', 'total')]
colnames(data_long1) <- c("year","axis", "number")
data_long1 <- rbind(data.frame(data_long1), c(1998, NA, NA))

barplot_axis_accelerometer <- ggplot(data=data_long1, aes(x=factor(year), y=number, fill=factor(axis))) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#A6CEE3", "#FFFF99", "#1F78B4")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications per accelerometer type') +
  labs(fill = "Number of axes") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=data_long1$year[as.character(data_long1$year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.25, 0.86)
  )# +
  #scale_fill_discrete(name = "Number of axes", labels = c("uni-axial", "bi-axial", "tri-axial"))

barplot_axis_accelerometer

# Figure 2C --------------------------------------------------------------------
# Donut plots
# Number of sensors per position per context
# Contexts included real-life setting, lab (controlled) setting and mixed settings
# Other positions included bag or pocket (n=5, real-life), tip of the cane or crutches (n=1, lab), head (n=1, lab)

data_donut <- matrix(c(c(2,12,2),
                     c(2,3,0),
                     c(0,4,0),
                     c(16, 13, 4),
                     c(69, 13, 10),
                     c(3, 12, 1),
                     c(1, 2, 0),
                     c(4, 34, 5),
                     c(8, 8, 2),
                     c(8, 9, 1),
                     c(10, 14, 3),
                     c(1, 15, 0),
                     c(25, 0, 2),
                     c(2, 1, 0)), ncol=3, byrow = T)

labels_donut <- c('Sternum\nn = 16',
                  'Upper\narm\nn = 5',
                  'Lower\narm\nn = 4',
                  'Wrist\nn = 33',
                  'Waist\nn = 92',
                  'Foot\nn = 16',
                  'Upper\nback\nn = 3',
                  'Lower\nback\nn = 43',
                  'Hand\nn = 18',
                  'Upper\nleg\nn = 18',
                  'Ankle\nn = 27',
                  'Lower\nleg\nn = 16',
                  'Not\nreported\nn = 27',
                  'Others\nn = 3')

labels_names_donut <- c('Sternum',
                        'UpperA',
                        'LowerA',
                        'Wrist',
                        'Waist',
                        'Foot',
                        'PostTrunk',
                        'L.backSacrum',
                        'Hand',
                        'UpperL',
                        'Ankle',
                        'LowerL',
                        'Not reported',
                        'Others')

for (i in c(1:dim(data_donut)[1])){
  data_temp <- data.frame(
    category=c("real-life setting", "lab setting", "both"),
    count=data_donut[i,]
  )
  
  # Compute percentages
  data_temp$fraction <- data_temp$count / sum(data_temp$count)
  data_temp$percent <- round((data_temp$fraction)*100, 1)
  
  # Compute the cumulative percentages (top of each rectangle)
  data_temp$ymax <- cumsum(data_temp$fraction)
  
  # Compute the bottom of each rectangle
  data_temp$ymin <- c(0, head(data_temp$ymax, n=-1))
  
  # Make the plot without percentages of each context
  ggplot(data_temp, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=category)) +
    geom_rect() +
    geom_text( x=-1, aes(y=0.25, label=labels_donut[i]), size=26) + 
    scale_fill_manual(values = c("#FDBF6F", "#1F78B4", "#A6CEE3")) +
    scale_color_brewer(palette=1) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")
  ggsave(paste0("/Volumes/blucie/PhD/10_MS/Figures&tables_afterrevision/Final/donuts/", labels_names_donut[i], '.png'), dpi = "retina")
  
  ## Make the plot with percentages of each context
  # ggplot(data_temp, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=category)) +
  #   geom_rect() +
  #   geom_text( x=-1, aes(y=0.25, label=labels_donut[i]), size=18) + # x here controls label position (inner / outer)
  #   scale_fill_manual(values = c("#FDBF6F", "#1F78B4", "#A6CEE3"))+
  #   scale_color_brewer(palette=1) +
  #   coord_polar(theta="y") +
  #   xlim(c(-1, 4)) +
  #   theme_void() +
  #   theme(legend.position = "none") +
  #   geom_label_repel(data = data,
  #                    aes(x = 3.5, y = (ymin+ymax)/2, label = paste0(percent, '%')),
  #                    size = 22, nudge_x = 4, show.legend = FALSE, label.size = NA,
  #                    fill = alpha(0.5))
  # ggsave(paste0("./donuts/", labels_names_donut[i], '_withpercentage.png'))
}


context_data <- data %>%
  group_by(Year, Context) %>% 
  count(Year, Context)

context_data$Context <- factor(context_data$Context, 
                               levels = c("mixed", "laboratory", "real-world"))

ggplot(data=context_data, aes(x=factor(Year), y=n, fill=factor(Context))) +
  geom_bar(position = "fill", stat='identity', colour="black") +
  scale_fill_manual(values = c("#FFFF99", "#A6CEE3", "#1F78B4")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications per context') +
  labs(fill = "Context") +
  scale_x_discrete(breaks=data_long1$year[as.character(data_long1$year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    #legend.position = c(0.25, 0.86)
    legend.position = c(0.1, 0.56)
  )

# Figure Supp --------------------------------------------------------------------
# RW/lab/mixed settings proportions over time

count_setting_time <- data_shiny %>%
  group_by(Year, Context) %>%
  count()

matrix_temp <- as.data.frame(t(matrix(c(1998, NA, NA))))
names(matrix_temp) <- names(count_setting_time)
count_setting_time <- rbind(as.data.frame(count_setting_time), matrix_temp)

barplot_settings <- ggplot(data=count_setting_time, aes(x=factor(Year), y=n, fill=factor(Context))) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#FFFF99", "#A6CEE3", "#1F78B4")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications') +
  labs(fill = "Context studied") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=count_setting_time$Year[as.character(count_setting_time$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.25, 0.86)
  )
barplot_settings

barplot_settings_prop <- ggplot(data=count_setting_time, aes(x=factor(Year), y=n, fill=factor(Context))) +
  geom_bar(stat="identity", colour="black", position = "fill") +
  scale_fill_manual(values = c("#FFFF99", "#A6CEE3", "#1F78B4")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Proportion of publications per context') +
  labs(fill = "Context studied") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=count_setting_time$Year[as.character(count_setting_time$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.83, 0.15)
  )
barplot_settings_prop


# Figure Supp --------------------------------------------------------------------
# Sensor locations over time

data_shiny <- read.csv('/Volumes/blucie/PhD/10_MS/ShinyApp_new/extracted_data.csv')
names(data_shiny)

data_sub_position <- data_shiny[c('Position.of.primary.wearable..drop.down.', 'Year')]
data_sub_position$Year[data_sub_position$Year == 2020] <- "2020-2021*"
data_sub_position$Year[data_sub_position$Year == 2021] <- "2020-2021*"

levels(factor(data_shiny$Position.of.primary.wearable..drop.down.))

data_sub_position <- data_sub_position %>%
  mutate(position_group = case_when(
    Position.of.primary.wearable..drop.down. %in% 
      c("ankle", "feet", "lower leg", "upper leg") ~ "lower.extremities",
    Position.of.primary.wearable..drop.down. %in% 
      c("hand", 'index finger (right)', "lower arm", "upper arm", "wrist") ~ "upper.extremities",
    Position.of.primary.wearable..drop.down. %in% 
      c("hip", "pelvis", "sacrum", "waist", "waist (non-dominant hip)") ~ "waist",
    Position.of.primary.wearable..drop.down. %in% 
      c("lower back", "lower-lumbar level", "posterior trunk", "sternum") ~ "trunk",
    Position.of.primary.wearable..drop.down. %in% 
      c("habitual phone position", "tip of cane / crutch") ~ "others",
    Position.of.primary.wearable..drop.down. %in% 
      c("", "not reported") ~ "not reported"#,
    #Position.of.primary.wearable..drop.down. %in% 
    #  c("multiple positions") ~ "multiple positions",
  ))

count_position_time <- data_sub_position %>%
  group_by(Year, position_group) %>%
  count()

multiple_sensors <- data.frame(Year = c("2007", "2007", "2007",
                                        "2010", "2010","2010",
                                        "2011", "2011",
                                        "2012", "2012", "2012", "2012",
                                        "2013", "2013", "2013",
                                        "2014", "2014", "2014", "2014",
                                        '2015', '2015','2015','2015','2015',
                                        '2016', '2016','2016','2016',
                                        '2017','2017','2017',
                                        '2018', '2018','2018','2018',
                                        '2019', '2019', '2019', '2019', '2019',
                                        "2020-2021*", "2020-2021*", "2020-2021*"),
                               position_group = c("upper.extremities", "waist", "lower.extremities",
                                                  "trunk", "waist", "lower.extremities",
                                                  "waist", "lower.extremities",
                                                  "upper.extremities", "trunk", "waist", "lower.extremities",
                                                  "upper.extremities", "trunk", "lower.extremities",
                                                  "upper.extremities", "trunk", "waist", "lower.extremities",
                                                  "upper.extremities", "trunk", "waist", "lower.extremities", "not reported",
                                                  "upper.extremities", "trunk", "waist", "lower.extremities",
                                                  "upper.extremities", "trunk", "lower.extremities",
                                                  "upper.extremities", "trunk", "lower.extremities", "others",
                                                  "upper.extremities", "trunk", "waist", "lower.extremities", "others",
                                                  "upper.extremities", "trunk", "lower.extremities"),
                               n = c(2, 1, 1,
                                     2, 2, 3,
                                     2, 6,
                                     2, 6, 1, 11,
                                     2, 2, 2,
                                     2, 2, 1, 4, 
                                     10, 5, 1, 8, 1,
                                     4, 1, 3, 2, 
                                     2, 5, 10,
                                     2, 5, 6, 2, 
                                     4, 5, 1, 11, 1,
                                     10, 8, 28)
)

count_position_time <- as.data.frame(count_position_time %>% drop_na())

merged.df <- merge(count_position_time, multiple_sensors ,by=c("Year", "position_group"), all.x =T)
merged.df[is.na(merged.df)] <- 0
merged.df$total <- merged.df$n.x + merged.df$n.y

matrix_temp <- as.data.frame(t(matrix(c(1998, NA, NA, NA, NA))))
names(matrix_temp) <- names(merged.df)
merged.df <- rbind(as.data.frame(merged.df), matrix_temp)

merged.df$position_group <- factor(merged.df$position_group, 
                                             levels = 
                                               c("not reported", "others", 
                                                 "multiple positions", "upper.extremities", 
                                                 "trunk", "waist", "lower.extremities"))

barplot_positions <- ggplot(data=merged.df, aes(x=factor(Year), y=total, fill=factor(position_group))) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#FDD0A2", "#FD8D3C", "#EFF3FF",  "#9ECAE1",  "#4292C6", "#084594")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of sensors') +
  labs(fill = "Wearable positions") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=merged.df$Year[as.character(merged.df$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.26, 0.80)
  )
barplot_positions

barplot_positions_prop <- ggplot(data=merged.df, aes(x=factor(Year), y=total, fill=factor(position_group))) +
  geom_bar(stat="identity", colour="black", position = "fill") +
  scale_fill_manual(values = c("#FDD0A2", "#FD8D3C", "#EFF3FF",  "#9ECAE1",  "#4292C6", "#084594")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of sensors') +
  labs(fill = "Sensor positions") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=merged.df$Year[as.character(merged.df$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    #legend.position = c(0.26, 0.80)
  )
barplot_positions_prop

#-------
# With multiple sensors as one category 

data_sub_position <- data_sub_position %>%
  mutate(position_group = case_when(
    Position.of.primary.wearable..drop.down. %in% 
      c("ankle", "feet", "lower leg", "upper leg") ~ "lower.extremities",
    Position.of.primary.wearable..drop.down. %in% 
      c("hand", 'index finger (right)', "lower arm", "upper arm", "wrist") ~ "upper.extremities",
    Position.of.primary.wearable..drop.down. %in% 
      c("hip", "pelvis", "sacrum", "waist", "waist (non-dominant hip)") ~ "waist",
    Position.of.primary.wearable..drop.down. %in% 
      c("lower back", "lower-lumbar level", "posterior trunk", "sternum") ~ "trunk",
    Position.of.primary.wearable..drop.down. %in% 
      c("habitual phone position", "tip of cane / crutch") ~ "others",
    Position.of.primary.wearable..drop.down. %in% 
      c("", "not reported") ~ "not reported",
    Position.of.primary.wearable..drop.down. %in% 
      c("multiple positions") ~ "multiple positions",
  ))

count_position_time <- data_sub_position %>%
  group_by(Year, position_group) %>%
  count()

matrix_temp <- as.data.frame(t(matrix(c(1998, NA, NA))))
names(matrix_temp) <- names(count_position_time)
count_position_time <- rbind(as.data.frame(count_position_time), matrix_temp)

count_position_time$position_group <- factor(count_position_time$position_group, 
                                   levels = 
                                     c("not reported", "others", 
                                       "multiple positions", "upper.extremities", 
                                       "trunk", "waist", "lower.extremities"))

barplot_positions <- ggplot(data=count_position_time, aes(x=factor(Year), y=n, fill=factor(position_group))) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#FDD0A2", "#FD8D3C", "#ec3700", "#EFF3FF",  "#9ECAE1",  "#4292C6", "#084594")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications per single/multiple wearable positions') +
  labs(fill = "Wearable positions") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=count_position_time$Year[as.character(count_position_time$Year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.26, 0.80)
  )
barplot_positions


# Figure Supp --------------------------------------------------------------------
# Number of PwMS included over time
library(ggridges)

# ggplot(data_shiny, aes(x = Number.of.MS.patients.included.in.analysis, y = factor(Year))) +
#   geom_density_ridges2(scale = 1.5, size = 0.3, rel_min_height = 0.01) +
#   theme_classic() +
#   xlab('Number of PwMS included') +
#   ylab('Year') +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
#   theme(
#     panel.grid.major.y = element_line(size=.1, color="black"),
#     panel.grid.major.x = element_line(size=.1, color="black"),
#     axis.title.x = element_text(size=14, face="bold"),
#     axis.title.y = element_text(size=14, face="bold"),
#     legend.title = element_text(size=14, face="bold"),
#     axis.text.x = element_text(angle = 45, vjust = 0.5),
#     #legend.position = c(0.26, 0.80)
#   )

data_shiny$Year[data_shiny$Year == 2020] <- "2020-2021*"
data_shiny$Year[data_shiny$Year == 2021] <- "2020-2021*"

ggplot(data_shiny, aes(x = Number.of.MS.patients.included.in.analysis)) + 
  geom_histogram(color="black", fill="white", binwidth = 20) +
  #facet_grid(rows = vars(Year)) +
  facet_wrap(Year ~ ., ncol = 4) +
  theme_classic() +
  xlab('Number of PwMS included')  +
  ylab('Number of publications')  +
    theme(
      panel.grid.major.y = element_line(size=.1, color="black"),
      panel.grid.major.x = element_line(size=.1, color="black"),
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"),
      legend.title = element_text(size=14, face="bold"),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
    )


# Figure Supp --------------------------------------------------------------------
# Number of wearables per setting
library(ggridges)



ggplot(data_shiny, aes(x = as.numeric(Number.of.primary.wearable..s.), y = factor(Context))) +
  geom_density_ridges_gradient(scale = 1.5, size = 0.3, rel_min_height = 0.01) +
  theme_classic() +
  xlab('Number of wearables used') +
  ylab('Context in which the wearables were studied') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    panel.grid.major.y = element_line(size=.1, color="black"),
    panel.grid.major.x = element_line(size=.1, color="black"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    #legend.position = c(0.26, 0.80)
  )

ggplot(data_shiny, aes(x = as.numeric(Number.of.primary.wearable..s.), fill = factor(Context))) +
  geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("#FFFF99", "#A6CEE3", "#1F78B4"))+
  theme_classic() +
  xlab('Number of wearables used') +
  ylab('Density') +
  labs(fill = "Context") +
  xlim(-0.5, 8)+
  theme(
    panel.grid.major.y = element_line(size=.1, color="black"),
    panel.grid.major.x = element_line(size=.1, color="black"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.8, 0.80)
  )

data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == '3/9'] <- 9
data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == 'non-wheelchair users: 1,\nwheelchair users: 2'] <- 2
data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == 
                                            'not sure \"This study included 6 ActiGraph model 7164 accelerom-eters\" but sounds like it is one per participant'] <- 1
data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == '1, 2 sensors'] <- 1
data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == '1 Smartphone'] <- 1
data_shiny$Number.of.primary.wearable..s.[data_shiny$Number.of.primary.wearable..s. == '2?'] <- 2

ggplot(data_shiny, aes(x = as.numeric(Number.of.primary.wearable..s.))) + 
  geom_histogram(color="black", fill="white", binwidth = 1) +
  facet_grid(Context~.) +
  theme_classic() +
  xlab('Number of wearables')  +
  ylab('Number of publications')  +
  theme(
    panel.grid.major.y = element_line(size=.1, color="black"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
  ) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1) +
  scale_x_continuous(breaks = c(1:9)) +
  ylim(0, 127)
  #scale_y_continuous(expand = c(0.15, 0.15))
