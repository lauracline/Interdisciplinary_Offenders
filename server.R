# Load libraries, data ------------------------------------------------
library(ggplot2)
library(tidyverse)

female_inmates <- read.csv("csc_female_data.csv") 

female_inmates <-
  female_inmates %>%
  filter(motivation != "" & reintegration_potential != "" & offender_security_level != "" & dynamic_need != "" & static_risk != "")

female_inmates$motivation <- factor(female_inmates$motivation, levels = c("HIGH", "MEDIUM", "LOW"))
female_inmates$reintegration_potential<- factor(female_inmates$reintegration_potential, levels = c("LOW", "MEDIUM", "HIGH"))
female_inmates$static_risk <- factor(female_inmates$static_risk, levels = c("LOW", "MEDIUM", "HIGH"))
female_inmates$dynamic_need <- factor(female_inmates$dynamic_need, levels = c("LOW", "MEDIUM", "HIGH"))
female_inmates$offender_security_level <- factor(female_inmates$offender_security_level, levels = c("MINIMUM", "MEDIUM", "MAXIMUM"))

female_inmates$race_categories <- sort(female_inmates$race_categories)
# Create server -------------------------------------------------------
server <- function(input, output) {
  output$plot <- renderPlot({
    
    ggplot(data = female_inmates) +
      geom_bar(mapping = aes_string(x = 'race_categories', fill = input$y_var), position = "fill") +
      theme_minimal() +
      labs(
        title = "Female Inmate's in Canadian Federal Prisons by Race (2012-2018)",
        x = "Ethnic Group",
        y = "Proportion of Inmates") +
      coord_flip() +
      scale_fill_brewer(palette = "Blues", name = "Legend") 
  })
}