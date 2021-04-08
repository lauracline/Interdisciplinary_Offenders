# title: "Overclassified and Overrepresented: Breaking the Cycle of Trauma and Crime for Indigenous Women in Canadian Federal Prisons (2012-2018)"
# author: "Laura Cline"
# date: "19/04/2021"

knitr::opts_chunk$set(echo = TRUE)

## Load and install packages ##

library(tidyverse)
library(janitor)
library(forcats)
library(kableExtra)
library(stargazer)

## Load data ##

csc_data <- read.csv("The_Globe_and_Mail_CSC_OMS.csv")

## Create dataset just for female inmates ##

female_ds <- csc_data %>%
  filter(GENDER == "FEMALE")

## Clean data ##

# Put column names to lowercase
female_ds <- female_ds %>%
  clean_names()

# Check for missing values 
sum(is.na(female_ds))

# Check for missing values in the race column
inmates_per_race <- female_ds %>%
  count(race)

inmates_per_race <- female_ds %>%
  count(race_grouping)

# Check number per offender security level 
inmates_per_race <- female_ds %>%
  count(offender_security_level)

# Check number per dynamic/need
inmates_per_race <- female_ds %>%
  count(dynamic_need)

# Check number per dynamic/need
inmates_per_race <- female_ds %>%
  count(static_risk)

# Check number per dynamic/need
inmates_per_race <- female_ds %>%
  count(reintegration_potential)

# Check nmber per dynamic/need
inmates_per_race <- female_ds %>%
  count(motivation)

# We see that we can rename some of these values 
unique(female_ds$race)

# We can see that the North America, Metis, and Inuit Races are all part of the Indigenous racial grouping 
female_ds_test <-
  female_ds %>%
  filter(race_grouping == "Indigenous") %>%
  select(race, race_grouping)

female_ds_test
unique(female_ds_test$race)


# Create a new column called race_categories 
female_ds$race_categories <- female_ds$race

# Group Ethnicities into Racial Categories  based on Statistics Canada 

female_ds$race_categories[female_ds$race_categories == ""] <- "Unable to Specify"
female_ds$race_categories[female_ds$race_categories == "North American"] <- "Indigenous"
female_ds$race_categories[female_ds$race_categories == "Metis"] <- "Indigenous"
female_ds$race_categories[female_ds$race_categories == "East Indian"] <- "South Asian"
female_ds$race_categories[female_ds$race_categories == "Unknown"] <- "Unable to Specify"
female_ds$race_categories[female_ds$race_categories == "Korean"] <- "East Asian"
female_ds$race_categories[female_ds$race_categories == "Euro.-Southern"] <- "White"
female_ds$race_categories[female_ds$race_categories == "Asian-South"] <- "South Asian"
female_ds$race_categories[female_ds$race_categories == "Euro.-Western"] <- "White"
female_ds$race_categories[female_ds$race_categories == "Asi-E/Southeast"] <- "East Asian"
female_ds$race_categories[female_ds$race_categories == "Asian-West"] <- "West Asian"
female_ds$race_categories[female_ds$race_categories == "Arab/West Asian"] <- "West Asian"
female_ds$race_categories[female_ds$race_categories == "Arab"] <- "West Asian"
female_ds$race_categories[female_ds$race_categories == "Euro.-Eastern"] <- "White"
female_ds$race_categories[female_ds$race_categories == "Sub-Sahara Afri"] <- "Black"
female_ds$race_categories[female_ds$race_categories == "Unable Specify"] <- "Unable to Specify"
female_ds$race_categories[female_ds$race_categories == "Filipino"] <- "East Asian"
female_ds$race_categories[female_ds$race_categories == "Caribbean"] <- "Black"
female_ds$race_categories[female_ds$race_categories == "Multirac/Ethnic"] <- "Multiracial"
female_ds$race_categories[female_ds$race_categories == "Chinese"] <- "East Asian"
female_ds$race_categories[female_ds$race_categories == "Inuit"] <- "Indigenous"
female_ds$race_categories[female_ds$race_categories == "Asiatic"] <- "East Asian"
female_ds$race_categories[female_ds$race_categories == "S. E. Asian"] <- "East Asian"

# Create racial_analysis column 
female_ds$race_analysis <- female_ds$race_categories

# Sort races into three categories: Indigenous, White and Other 
female_ds$race_analysis[female_ds$race_analysis == "Latin American"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "Multiracial"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "Black"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "Unable to Specify"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "East Asian"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "West Asian"] <- "Other"
female_ds$race_analysis[female_ds$race_analysis == "South Asian"] <- "Other"

# Write the female inmates dataframe to a csv file - this is for the shiny app 
write_csv(female_ds, "csc_female_data.csv")

# Create new dataframe just for female indigenous inmates 
indigenous_df <- female_ds %>%
  filter(race_grouping == "Indigenous")

## Data ##

# Table for Total Female Inmates
number_of_inmates <- female_ds %>% # Use the female_ds dataframe 
  summarise(n = n(), # summarise by count 
            average_age = round(mean(age), digits=2), # Round average age to two decimal places 
            average_aggregate_sentence_length = round(mean(aggregate_sentence_length) / 365, digits=2)) %>% #Divide average aggregate sentence length by 365 to convert to years and limit to two decimal places
  rename("Number of Inmates" = n, # Rename columns 
         "Average Age" = "average_age",
         "Average Sentence Length (Years)" = "average_aggregate_sentence_length")

inmates_table <- number_of_inmates %>%
  knitr::kable(caption = "Number of Female Inmates in Federal Prisons (2011-2018)") %>% # Give table a caption
  kableExtra::kable_styling() %>% # Clean table 
save_kable("Tables/total_female_inmates.pdf") # Save table as a pdf 

# Create a Race Categrories Table 
inmate_chars_race <- female_ds %>% # Use the female_ds dataframe 
  group_by(race_categories) %>% # group by race categories 
  summarise(n = n(), # summarise by count
            average_age = round(mean(age), digits=2), # Round the average age to two decimal places
            average_aggregate_sentence_length = round(mean(aggregate_sentence_length) / 365, digits=2)) %>% # Divide average aggregate sentence length by 365 to get years and limit to two decimal places
  mutate(freq = (n / sum(n))*100) %>% # Get the proportion of inmates per race 
  arrange(desc(n)) %>% # arrange in descending order 
  rename("Number of Inmates" = n, # Rename columns 
         "Ethnic Group" = "race_categories",
         "Average Age" = "average_age",
         "Average Sentence Length (Years)" = "average_aggregate_sentence_length",
         "Percent of Female Inmates" = "freq")

race_table <- inmate_chars_race %>%
  knitr::kable(caption = "Number of Female Inmates in Federal Prisons (2011-2018)") %>% # Caption 
  kableExtra::kable_styling(latex_options = "scale_down") #%>% # Cleaner table and make sure it fits on output 
#save_kable("Tables/total_female_inmates_by_race.pdf")

# Create bar plot for female inmates per race 
race_plot <- female_ds %>% # Use female_ds dataset 
  group_by(race_categories) %>% # Group by race categories 
  summarise(count = n()) %>% # summarise using count 
  ggplot(mapping = aes(x = reorder(race_categories, (count)), y = count)) + # Reorder so race categories are in descending order by count 
  geom_bar(fill = "orangered2", stat = 'identity') + # Create bar plot
  coord_flip() + # Create horizontal bar chart 
  theme_minimal() + # Minimal Theme 
  labs( # Add labels 
    title = "Indigenous Women Disproportionately Represented in \nFederal Prisons (2012-2018)",
    subtitle = "Although Indigenous women make up less than 4% of the Canadian female \npopulation, they represent over 34% of female inmates",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Racial Group",
    y = "Number of Inmates"
  )

pp1 <- cowplot::plot_grid(race_plot, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot 

cowplot::save_plot("Figures/total_inmates_by_race.pdf", plot=pp1, base_width=7, base_height=12)

# Races for Indigenous Women Table 
indigenous_distribution <- indigenous_df %>% # Use indigenous_df dataframe 
  group_by(race) %>% # Group by race 
  summarise(n = n(), # summarise using count 
            average_age = round(mean(age), digits=2), # Round average age to two decimal places
            average_aggregate_sentence_length = round(mean(aggregate_sentence_length) / 365, digits=2)) %>% # Divide average aggregate sentence length by 365 to get years and round to two decimal places
  mutate(freq = (n / sum(n))*100) %>% # Get the proportion per race 
  arrange(desc(n)) %>% # Arrange in descending order 
  rename("Indigenous Ethnic Group" = "race", # Rename columns 
         "Number of Inmates" = n,
         "Average Age" = "average_age",
         "Average Sentence Length (Years)" = "average_aggregate_sentence_length",
         "Percent of Indigenous Female Inmates" = "freq")

indigenous_distribution %>%
  knitr::kable(caption = "Number of Female Indigenous Inmates by Ethnicity (2012-2018)") %>% # Add caption 
  kableExtra::kable_styling(latex_options = "scale_down") #%>% # Add style and fit to output 
#save_kable("Tables/total_female_indigenous_inmates_by_ethnic.pdf")


# Create a graph to show proportion of indigenous women 
i_plot <- indigenous_df %>% # Use indigenous_df 
  group_by(race) %>% # group by race 
  summarise(count = n()) %>% # summarise by count 
  ggplot(mapping = aes(x = reorder(race, (count)), y = count)) + # order in descending order by count 
  geom_bar(fill = "orangered2", stat = 'identity') + # Create bar plot 
  coord_flip() + # Create horizontal bar plot 
  theme_minimal() + # Use minimal theme 
  labs( # Add labels 
    title = "North American (First Nations) Women Are the Largest \nIndigenous Group in Federal Prisons (2012-2018)",
    subtitle = "The graph presents First Nations women as one group, but Indigenous women are not a \nsingle population with one single voice. Instead, they are members of distinct nations \nwith different histories, cultures, knowledge, and social experiences.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Indigenous Ethnic Group",
    y = "Number of Inmates"
  )

pp2 <- cowplot::plot_grid(i_plot, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot 

cowplot::save_plot("Figures/total_inmates_by_indigenous_ethnic.pdf", plot=pp2, base_width=7, base_height=12)

# Offender Security Level Proportion Chart 
security_level_sorted <- 
  female_ds %>% # Use female_df
  filter(offender_security_level != "") # Remove empty rows 

security_level_sorted$offender_security_level <- factor(security_level_sorted$offender_security_level, levels = c("MINIMUM", "MEDIUM", "MAXIMUM")) # Factor security levels by the order you want them to appear on the chart

security <- security_level_sorted %>% # Use security_level_sorted dataframe 
  ggplot() + 
  geom_bar(mapping = aes(x = race_analysis, fill = offender_security_level), position = "fill") + # Create a bar plot that is filled by proportion 
  theme_minimal() + # Use minimal theme 
  scale_fill_brewer(palette = "Reds", name = "Offender Security Level") + # Choose colour palette 
  labs( # Add labels 
    title = "Indigenous Women Overrepresented in Maximum Security (2012-2018)",
    subtitle = "Indigenous women are more likely to have a maximum or medium security level \ncompared to White women.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Ethnic Group",
    y = "Proportion of Inmates"
  )

pp3 <- cowplot::plot_grid(security, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot

cowplot::save_plot("Figures/inmate_security_level_score.pdf", plot=pp3, base_width=7, base_height=12)

# Dynamic/Need Proportion Chart 

dynamic_sorted <-
  female_ds %>% 
  filter(dynamic_need != "") # Remove rows with missing values 

dynamic_sorted$dynamic_need <- factor(dynamic_sorted$dynamic_need, levels = c("LOW", "MEDIUM", "HIGH")) # Factor dynamic need score with the order you want it to appear on the graph

dynamic <- dynamic_sorted %>%
  ggplot() +
  geom_bar(mapping = aes(x = race_analysis, fill = dynamic_need), position = "fill") + # Create a proportional bar plot filled by dynamic_need
  theme_minimal() + # Minimal theme 
  scale_fill_brewer(palette = "Reds", name = "Dynamic/Need Score") + # Choose colour palette 
  labs( # Add labels 
    title = "Indigenous Women Overclassified as High Dynamic Risk (2012-2018)",
    subtitle = "Indigenous women are more likely to be classified as having a high dynamic risk \ncompared to White women.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Ethnic Group",
    y = "Proportion of Inmates"
  )

pp4 <- cowplot::plot_grid(dynamic, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot 

cowplot::save_plot("Figures/dynamic_need_score.pdf", plot=pp4, base_width=7, base_height=12)

# Static/Risk Proportion Chart 

static_sorted <-
  female_ds %>%
  filter(static_risk != "") # Remove rows with missing values 

static_sorted$static_risk <- factor(static_sorted$static_risk, levels = c("LOW", "MEDIUM", "HIGH")) # Factor to order the labels the way you want them to appear in the chart 

static <- static_sorted %>%
  ggplot() +
  geom_bar(mapping = aes(x = race_analysis, fill = static_risk), position = "fill") + # Create proportional bar chart filled by static_risk
  theme_minimal() + # Minimal theme 
  scale_fill_brewer(palette = "Reds", name = "Static/Risk Score") + # Choose colour palette 
  labs( # Add labels 
    title = "Indigenous Women Overclassified as High Static Risk (2012-2018)",
    subtitle = "Indigenous women are more likely to be classified as having a high static risk compared \nto White women.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Ethnic Group",
    y = "Proportion of Inmates"
  )

pp5 <- cowplot::plot_grid(static, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot

cowplot::save_plot("Figures/static_risk_score.pdf", plot=pp5, base_width=7, base_height=12)

# Motivation Proportional Chart

motivation_sorted <-
  female_ds %>%
  filter(motivation != "") # Remove rows with missing values 

motivation_sorted$motivation <- factor(motivation_sorted$motivation, levels = c("HIGH", "MEDIUM", "LOW")) # Factor by the order you want them to appear in the chart

motivation <- motivation_sorted %>%
  ggplot() +
  geom_bar(mapping = aes(x = race_analysis, fill = motivation), position = "fill") + # Create proportional bar chart filled by static_risk
  theme_minimal() + # Minimal theme 
  scale_fill_brewer(palette = "Reds", name = "Motivation Score") + # Add colour palette 
  labs( # Add labels 
    title = "Indigenous Women Classified as Having a Low Criminal Motivation (2012-2018)",
    subtitle = "On top of their high security level, dynamic risk, and static risk, Indigenous women are \nclassified as having low motivation to change and reform compared to White women.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Ethnic Group",
    y = "Proportion of Inmates"
  )

pp6 <- cowplot::plot_grid(motivation, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot

cowplot::save_plot("Figures/motivation_score.pdf", plot=pp6, base_width=7, base_height=12)

# Reintegration Potential Proportion Chart

reintegration_sorted <-
  female_ds %>%
  filter(reintegration_potential != "") # Remove rows with missing values 

reintegration_sorted$reintegration_potential<- factor(reintegration_sorted$reintegration_potential, levels = c("LOW", "MEDIUM", "HIGH")) # Factor by order you want them to appear in the chart

reintegration <- reintegration_sorted %>%
  ggplot() +
  geom_bar(mapping = aes(x = race_analysis, fill = reintegration_potential), position = "fill") + # Create proportional bar chart filled by reintegration_potential
  theme_minimal() + # Minimal theme 
  scale_fill_brewer(palette = "Greens", name = "Reintegration Potential") + # Choose colour palette 
  labs( # Add labels 
    title = "Indigenous Women Given the Most Low Reintegration Scores (2012-2018)",
    subtitle = "White women and non-Indigenous minority groups are more likely to be given high \nreintegration scores compared to Indigenous women.",
    caption = "(Data from Correctional Services Canada (2020))",
    x = "Ethnic Group",
    y = "Proportion of Inmates"
  ) 

pp7 <- cowplot::plot_grid(reintegration, nrow=2, rel_heights = c(1,1.4), align="v") # Save plot 

cowplot::save_plot("Figures/reintegration_potential_score.pdf", plot=pp7, base_width=7, base_height=12)


## Model ##

# Logistic Regression Model for Race and Inmate's Security Level 

female_ds$offender_security_level %>% table() # Convert offender security level to a table 

inmate_experiment_one <-
  female_ds %>%
  filter(offender_security_level != "") # Remove rows with missing values 

inmate_experiment_one <-
  inmate_experiment_one %>%
  mutate(is_high_offender_security_level = if_else(offender_security_level == "MAXIMUM", 1, 0)) # Factor so Maximum is 1 and everything else is 0

offender_security_level_model <-
  glm(is_high_offender_security_level ~ race_analysis + age, # Logistic regression model where DV is maximum offender security level and IV is race. CV is age
      data = inmate_experiment_one, 
      family = 'binomial')

stargazer::stargazer(offender_security_level_model, header=TRUE, type = "latex", # Create nicer table for output
                     title = "Logistic Regression Model Predicting a Maximum Security Level for Female Inmates",
                     out="tables/offender_security_model.pdf")


# Dynamic/Need Score Logistic Regression Model

female_ds$dynamic_need%>% table() # Convert dynamic need  to a table 

inmate_experiment_three <-
  female_ds %>%
  filter(dynamic_need != "") # Remove rows with missing values 

inmate_experiment_three <-
  inmate_experiment_three %>%
  mutate(is_high_dynamic = if_else(dynamic_need == "HIGH", 1, 0)) # Factor so High is 1 and everything else is 0

offender_dynamic_model <-
  glm(is_high_dynamic ~ race_analysis + age,
      data = inmate_experiment_three,
      family = 'binomial') # Logistic regression model where DV is high dynamic need and IV is race. CV is age

stargazer::stargazer(offender_dynamic_model, header=TRUE, type = "latex", # Cleaner model 
                     title = "Logistic Regression Model Predicting a High Dynamic Score for Female Inmates",
                     out="tables/offender_dynamic_model.pdf")

# Static Risk Logisic Regression Model 

female_ds$static_risk%>% table() # Convert static risk to a table 

inmate_experiment_four <-
  female_ds %>%
  filter(static_risk != "") # Remove rows with missing values 

inmate_experiment_four <-
  inmate_experiment_four %>%
  mutate(is_high_static = if_else(static_risk == "HIGH", 1, 0)) # Factor so Low is 1 and everything else is 0

offender_static_model <-
  glm(is_high_static ~ race_analysis + age,
      data = inmate_experiment_four,
      family = 'binomial') # Logistic regression model where DV is high static risk and IV is race. CV is age

stargazer::stargazer(offender_static_model, header=TRUE, type = "latex", # Cleaner model output 
                     title = "Logistic Regression Model Predicting a High Static Score for Female Inmates",
                     out="tables/offender_static_model.pdf")


# Motivation Logistic Regression Model 

female_ds$motivation%>% table() # Convert motivation to a table 

inmate_experiment_five <-
  female_ds %>%
  filter(motivation != "") # Remove rows with missing values 

inmate_experiment_five <-
  inmate_experiment_five %>%
  mutate(is_low_motivation = if_else(motivation == "LOW", 1, 0)) # Factor so Low is 1 and everything else is 0

offender_motivation_model <-
  glm(is_low_motivation ~ race_analysis + age,
      data = inmate_experiment_five,
      family = 'binomial') # Logistic regression model where DV is low motivation and IV is race. CV is age

stargazer::stargazer(offender_motivation_model, header=TRUE, type = "latex", # Cleaner model output 
                     title = "Logistic Regression Model Predicting a High Motivation Score for Female Inmates",
                     out="tables/offender_motivation_model.pdf")


# Race and Inmate's Reintegration Potential Score Logistic Regression Model 

female_ds$reintegration_potential %>% table() # Convert offender reintegration potential to a table 

inmate_experiment_two <-
  female_ds %>%
  filter(reintegration_potential != "") # Remove rows with missing values 

inmate_experiment_two <-
  inmate_experiment_two %>%
  mutate(is_high_reintegration_potential = if_else(reintegration_potential == "HIGH", 1, 0)) # Factor so High is 1 and everything else is 0

reintegration_potential_model <-
  glm(is_high_reintegration_potential ~ race_analysis + age + aggregate_sentence_length,
      data = inmate_experiment_two,
      family = 'binomial') # Logistic regression model where DV is high reintragration potential  and IV is race. CV is age and aggregate sentence length 

stargazer::stargazer(reintegration_potential_model, header=TRUE, type = "latex", # Cleaner model output 
                     title = "Logistic Regression Model Predicting a High Reintegration Potential Score for Female Inmates",
                     out="tables/reintegration_potential_model.pdf")


