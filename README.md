## This project helped me win ASA DataFest Hackathon WIU 2024 award. Leading a team of 5 and presenting findings was very stressful, but it was worth the effort.

Here are the slides for reference: https://www.linkedin.com/in/anirudhmadarapu/overlay/1713134889717/single-media-viewer/?profileId=ACoAADDv7RwBLN9FKCn2tCWWb8J-RIJp6wCRr0Y

## 1st Graphic
The first graphic in the code tells the relationship between the Median EOC(End of chapter question score) and the chapter number. As the chapter number increases, there is a greater chance that the EOC will decrease.
```
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
```
![image](https://github.com/Anirudh-Madarapu/DataFest-Hackathon-2024/assets/123264579/a9cae805-4bdb-45f6-a18c-969c1215459b)

## 2nd Graphic
The code associated with 2nd Graphic
```distracted_students <- data4 %>%
  group_by(chapter) %>%
  summarise(
    total_sections = n_distinct(section_number),
    distracted_count = sum(off_page_brief > 1200 | off_page_long > 1200, na.rm = TRUE)
  )

# Printing result
print(distracted_students)

lm1 <- lm( distracted_count ~ total_sections, data = distracted_students )
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
```
![image](https://github.com/Anirudh-Madarapu/DataFest-Hackathon-2024/assets/123264579/7ffef58a-2d5e-4f6e-a76b-aa25150ecfe2)

## 3rd Graphic
The 3rd graphic was created by Justin Douty, another teammate of mine. This graphic gives the relationship between the revision of each book and the proportion of student sessions that were classified as distracted

### 4th Graphic

This shows the amount of engagement for pages with different types of questions. Pages with formulaV2 get low engagement and close association one gets high engagement
```merged_data <- merge(pageView, items, by = c("chapter", "page"), all = TRUE)

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
```
![image](https://github.com/Anirudh-Madarapu/DataFest-Hackathon-2024/assets/123264579/45b4b386-9598-4a04-8bd0-29927590e527)



### 5th Graphic
The number of questions that were marked as review
```##Review against type of question
review_count_data <- items %>%
  group_by(lrn_type) %>%
  summarise(
    review_count = sum(review_flag== TRUE, na.rm = TRUE) # Count occurrences of TRUE
  )

# Order review_count_data by review_count in ascending order
review_count_data <- review_count_data[order(review_count_data$review_count),]

review_count_data_nonzero <- review_count_data %>%
  filter(!is.na(lrn_type), review_count > 0)
review_count_data_zero <- review_count_data %>%
  filter(!is.na(lrn_type), review_count == 0)

# Plotting the bar plot for non-zero counts
p <- ggplot(review_count_data_nonzero, aes(x = reorder(lrn_type, review_count), y = review_count)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(x = "Learnosity type", y = "Review Flag Count", title = "Review Flag Count by Learnosity Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5))

# Adding points for zero counts
p + geom_point(data = review_count_data_zero, aes(x = reorder(lrn_type, review_count), y = review_count), size = 3, color = "red")
```
![image](https://github.com/Anirudh-Madarapu/DataFest-Hackathon-2024/assets/123264579/9f6219a9-e2cd-440b-85ba-ea4c2bc60fa3)
