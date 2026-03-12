#BSIT 2-1 | GROUP 2 | ASSIGNMENT #2

#----Load necessary libraries-----
library(readxl) # to read the excel file
library(dplyr) # for data manipulation
library(ggplot2) # to visualize the data

#-----DATA-----
German_Credit_Risk <- read_excel("C:/Users/Jenny/Downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
DATA <- German_Credit_Risk
attach(DATA)

#-----Task 1 – Custom Function-----
# Create a custom R function (e.g., a function that calculates credit risk metrics or summarizes loan info) and demonstrate its use with examples.



#-----Task 2 – Data Structures-----
# Create and assign all required data structures: a vector, an ordered factor, an unordered factor, a table, and a data frame (using the German Credit Dataset where applicable).



#-----Task 3 – Data Manipulation with dplyr----



#-----Task 4 – Base R Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using Base R graphics only



#-----Task 5 – ggplot2 Visualizations-----
# Create meaningful visualizations of the German Credit Dataset using ggplot2 only
ggplot(DATA, aes(x = Age, y = Credit_amount)) + 
  
  # Points
  geom_point(aes(color = Risk), alpha = 0.6) +

  # Color of points
  scale_color_manual(values = c("bad" = "plum2", "good" = "mediumorchid")) +
  
  # Smooth regression line
  geom_smooth(method = "lm", se = FALSE, color = "darkorchid4") +

  # Facet by purpose
  facet_wrap(~Purpose) +
    
  # Customize theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom") +
  
  # Titles and labels
  labs(title = "Age vs. Credit Amount",
       subtitle = "Colored by Risk and Faceted by Purpose",
       x = "Applicant Age",
       y = "Credit Amount") 