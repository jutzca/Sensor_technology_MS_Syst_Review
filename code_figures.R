################################################################################
# Wearable sensor technology for patients with multiple sclerosis
# Does current knowledge drive future development
# A systematic review
# ------------------------------------------------------------------------------
# Code by L. Bourguignon
# Last update : 18.11.2021
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
#setwd('/Users/blucie/PhD/10_MS/Final_figures&tables')
data_figure2 <- read.csv('/Users/blucie/PhD/10_MS/Final_figures&tables/data_figure2_MS.csv')

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
accelerometer_data2 <- accelerometer_data %>% mutate(total_accelerometer = sum(c(accelerometer, 
                                                                                 `accelerometer, gyroscope`,
                                                                                 `accelerometer, gyroscope, magnetometer`,
                                                                                 `accelerometer, gyroscope, others`,
                                                                                 `accelerometer, gyroscope, touchscreen`,
                                                                                 `accelerometer, others`)))
# Count all appearances of gyroscopes per year
gyroscope_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("gyroscope",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
gyroscope_data2 <- gyroscope_data %>% mutate(total_gyroscope = sum(c(`accelerometer, gyroscope`,
                                                                     `accelerometer, gyroscope, magnetometer`,
                                                                     `accelerometer, gyroscope, others`,
                                                                     `accelerometer, gyroscope, touchscreen`)))
# Count all appearances of magnetometers per year
magnetometer_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("magnetometer",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
magnetometer_data2 <- magnetometer_data %>% mutate(total_magnetometer = sum(c(`accelerometer, gyroscope, magnetometer`)))

# Count all appearances of smartphone/touchscreen per year
touchscreen_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("touchscreen",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
touchscreen_data2 <- touchscreen_data %>% mutate(total_touchscreen = sum(c(`accelerometer, gyroscope, touchscreen`,
                                                                           `touchscreen`,
                                                                           `accelerometer, touchscreen`)))
# Count all appearances of other wearables used per year
others_data <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("others",sensors_type_plot)) %>% 
  count(year, sensors_type_plot) %>% 
  spread(sensors_type_plot, n, fill = 0) 
others_data2 <- others_data %>% mutate(total_others = sum(c(`accelerometer, gyroscope, others`,
                                                            `accelerometer, others`,
                                                            `others`)))

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
                              others_data2[c('year', 'total_others')], 
                              by="year", all = T)
counts_combined_fig2[is.na(counts_combined_fig2)] <- 0
# Harmonise column names
colnames(counts_combined_fig2) <- c("year", "accelerometer", "gyroscope", 
                                    'magnetometer', 'smartphone/touchscreen', 
                                    'others')
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
                                                       "smartphone/touchscreen", 
                                                       'magnetometer', 
                                                       'gyroscope', 
                                                       'accelerometer'))


barplot_sensor <- ggplot(data=counts_combined_fig2_long, aes(x=factor(year), y=value, fill=sensors)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#FFFF99", "#FF7F00", "#FDBF6F", "#1F78B4", "#A6CEE3")) + 
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of publications') +
  labs(fill = "Type of sensor(s)") +
  geom_vline(xintercept = 2, linetype=2) + # add vertical line to separate 1997 and 2006
  scale_x_discrete(breaks=counts_combined_fig2_long$year[as.character(counts_combined_fig2_long$year)!='1998']) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black" ),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.position = c(0.25, 0.86)
  )

barplot_sensor

# Figure 2B --------------------------------------------------------------------
# Bar plot
# Number of accelerometer per year of publication per number of axis
# Numbers of axis ranged from 1 to 3, included

accelerometer_data_axis <- data_figure2 %>%
  group_by(year) %>% 
  filter(grepl("accelerometer", sensors_type_plot)) %>% 
  count(year, sensors_type_plot, axes) %>% 
  spread(sensors_type_plot, n, fill = 0) 
accelerometer_data2_axis <- accelerometer_data_axis %>% 
  group_by(year, axes) %>%
  mutate(total = sum(c(accelerometer,
                       `accelerometer, gyroscope`,
                       `accelerometer, gyroscope, magnetometer`,
                       `accelerometer, gyroscope, others`,
                       `accelerometer, gyroscope, touchscreen`,
                       `accelerometer, others`,
                       `accelerometer, touchscreen`)))

# Subset the accelerometer_data2_axis table to only the total number per year
data_all_axis <- accelerometer_data2_axis[c('year', 'axes', 'total')]
# Harmonise column names
colnames(data_all_axis) <- c("year","axis", "number")
# Add a year with no wearable to create a space between 1997 and 2006
data_all_axis <- rbind(data.frame(data_all_axis), c(1998, NA, NA))  

barplot_axis_accelerometer <- ggplot(data=data_all_axis, aes(x=factor(year), y=number, fill=factor(axis))) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#A6CEE3", "#FFFF99", "#1F78B4"), labels = c("uni-axial", "bi-axial", "tri-axial", "not applicable")) +
  theme_classic() +
  xlab('Year of publication') +
  ylab('Number of accelerometers') +
  labs(fill = "Number of axes") +
  geom_vline(xintercept = 2, linetype=2) +
  scale_x_discrete(breaks=data_all_axis$year[as.character(data_all_axis$year)!='1998']) +
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

data_donut <- matrix(c(c(3,12,1),
                     c(3,4,0),
                     c(0,5,0),
                     c(13, 22, 5),
                     c(75, 16, 5),
                     c(6, 19, 3),
                     c(1, 4, 0),
                     c(3, 31, 5),
                     c(7, 10, 3),
                     c(9, 11, 2),
                     c(14, 23, 3),
                     c(1, 24, 0),
                     c(20, 0, 1),
                     c(5, 2, 0)), ncol=3, byrow = T)

labels_donut <- c('Sternum\nn = 16',
                  'Upper\narm\nn = 7',
                  'Lower\narm\nn = 5',
                  'Wrist\nn = 40',
                  'Waist\nn = 96',
                  'Foot\nn = 28',
                  'Upper\nback\nn = 5',
                  'Lower\nback\nn = 39',
                  'Hand\nn = 20',
                  'Upper\nleg\nn = 22',
                  'Ankle\nn = 40',
                  'Lower\nleg\nn = 25',
                  'Not\nreported\nn = 21',
                  'Others\nn = 7')

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
    geom_text( x=-1, aes(y=0.25, label=labels_donut[i]), size=18) + 
    scale_fill_manual(values = c("#FDBF6F", "#1F78B4", "#A6CEE3")) +
    scale_color_brewer(palette=1) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")
  ggsave(paste0("/Users/blucie/PhD/10_MS/InitialPlots/Figure4_donutplots/corrected/", labels_names_donut[i], '.png'))
  
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
