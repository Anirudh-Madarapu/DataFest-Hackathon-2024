## This project helped me win ASA DataFest Hackathon WIU 2024 award. Leading a team of 5 and presenting findings was very stressful, but it was worth the effort.

Here are the slides for reference: https://www.linkedin.com/in/anirudhmadarapu/overlay/1713134889717/single-media-viewer/?profileId=ACoAADDv7RwBLN9FKCn2tCWWb8J-RIJp6wCRr0Y

## 1st Graphic
The first graphic in the code tells the relationship between the Median EOC(End of chapter question score) and the chapter number. As the chapter number increases, there is a more chance that the EOC will decrease.

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
![image](https://github.com/Anirudh-Madarapu/DataFest-Hackathon-2024/assets/123264579/398e3099-3541-4f4a-b330-22efa8dd05cd)


