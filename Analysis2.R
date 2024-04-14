responses <- read.csv('responses.csv')
items <- read.csv('items.csv')
media <- read.csv('media_views.csv')
pageView <- read.csv('page_views.csv')
checkpoint <- read.csv('checkpoints_pulse.csv')

data2 <- pageView[pageView$book == 'College / Advanced Statistics and Data Science (ABCD)',]
data3 <- data2[data2$release=='v5.0',]

data4 <- pageView

library(dplyr)


distracted_students <- data4 %>%
  group_by(chapter) %>%
  summarise(
    total_sections = n_distinct(section_number),
    distracted_count = sum(off_page_brief > 1200 | off_page_long > 1200, na.rm = TRUE)
  )

# Printing result
print(distracted_students)

lm1 <- lm(total_sections ~ distracted_count, data = distracted_students )
summary(lm1) ##Gives us summary of our LM model above

library(ggplot2)

#Making a scatter plot
ggplot(distracted_students, aes(x = total_sections, y = distracted_count)) +
  geom_point(color = 'red', aes(shape = 'Data Points')) +  # Add red color points
  geom_smooth(method = 'lm', se = FALSE, color = 'blue', aes(linetype = 'Line of Best Fit')) +  # Add line of best fit
  labs(x = 'Total Sections', y = 'Distracted Count', title = 'Scatter Plot of Total Sections vs. Distracted Count') +  # Add axis labels and title
  theme_minimal() +  # Set a minimal theme
  scale_shape_manual(values = c(16)) +  # Set shape for data points
  scale_linetype_manual(values = 'solid') +  # Set line type for the line of best fit
  guides(shape = guide_legend(title = 'Legend', override.aes = list(color = c('red'))),  # Add legend for shape
         linetype = guide_legend(override.aes = list(color = c('blue')))) +  # Add legend for line type
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),  # Center title
    panel.grid.major = element_line(color = "black"),  # Set major grid lines to black
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA),  # Set border color to black
    axis.line = element_line(color = "black")  # Set axis lines to black
  )


## Analysis 2
data2 <- read.csv("checkpoints_eoc.csv")

# Group by chapter number and calculate the median EOC
median_eoc <- data2 %>%
  group_by(chapter_number) %>%
  summarise(median_eoc = median(EOC))

lm2 <- lm(median_eoc ~ chapter_number, data = median_eoc)

summary(lm2)

ggplot(median_eoc, aes(x = chapter_number, y = median_eoc)) +
  geom_point(color = 'blue', aes(shape = 'Median EOC')) +  # Adding points with a legend
  geom_smooth(method = 'lm', se = FALSE, color = 'red', aes(linetype = 'Trend')) +  # Adding regression line with the legend label
  labs(x = 'Chapter Number', y = 'Median EOC', title = 'Relationship between Chapter Number and Median EOC') +  # Adding axis labels and title
  theme_minimal() +  # Set a minimal theme
  theme(plot.title = element_text(hjust = 0.5),  
        legend.title = element_text(color = 'black', size = 12), 
        legend.position = 'bottom',  # Set legend position
        panel.grid.major = element_line(color = 'black'))

##Analysis 3
engaged_data <- merged_data %>%
  group_by(lrn_type) %>%
  summarise(
    median_eng = median(engaged, na.rm = TRUE) 
  ) %>%
  na.omit()

# Order engaged_data by median_eng in ascending order
engaged_data <- engaged_data[order(engaged_data$median_eng),]

# Plotting the bar graph
ggplot(engaged_data, aes(x = reorder(lrn_type, median_eng), y = median_eng)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(x = "Learnosity type", y = "Median Engagement", title = "Median Engagement by lrn_type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5))

##Analysis 4
merged_data <- merge(checkpoint, pageView, by = c('book', 'release', 'chapter_number'), all = TRUE)
head(merged_data)

summary_data <- merged_data %>%
  group_by(construct) %>%
  summarise(median_sum = sum(off_page_brief, na.rm = TRUE))

# Plotting the bar graph
ggplot(summary_data, aes(x = construct, y = median_sum)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Construct", y = "Median Sum of Off Page (Brief + Long)", title = "Median Sum of Off Page by Construct") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis 
