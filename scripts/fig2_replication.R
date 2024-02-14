#### Preamble ####
# Purpose: Figure 2 replication
# Author: Qisheng Liang
# Date: 10 February 2024
# Contact: frankie.liang@mail.utoronto.ca

## Data Sources:

# Kutscher, L., & Feldman, G. (2019). The impact of past behaviour normality on regret: 
# replication and extension of three experiments of the exceptionality effect. Cognition and Emotion, 33(5), 901–914.  
#https://doi.org/10.1080/02699931.2018.1504747
# Data can be found on page 7,





#### Workspace setup ####
# Load libraries
library(tidyverse)
library(here)

# Read the data
data <-  read_csv(
  file = here("inputs/data/T0002-10.1080_02699931.2018.1504747.csv"))
                 
data_clean <- data %>%
 slice(-1) %>% # Remove the first row which contains the headers 'Counts' and '%'
 rename(Group = `...1`, Regret_Counts = Regret, Regret_Percent = `...3`,
        Luck_Counts = Luck, Luck_Percent = `...5`)

long_data <- data_clean %>% 
 pivot_longer(
   cols = starts_with("Regret_") | starts_with("Luck_"),
   names_to = c("Type", ".value"),
   names_sep = "_"
 )

long_data <- long_data %>%
 mutate(Percent = as.numeric(as.character(Percent)) / 100)

ggplot(long_data, aes(x = Type, y = Percent, fill = Group)) + 
 geom_bar(stat = "identity", position = position_dodge(width = 1)) +
 scale_y_continuous(labels = scales::percent_format()) +
 scale_fill_manual(values = c("Routine Adams" = "blue", "Exception White" = "red")) +
 labs(x = NULL, y = "Percentage", fill = "Group", title = "Table 2: Proportions for perceived regret and luck") +
 theme_minimal()





