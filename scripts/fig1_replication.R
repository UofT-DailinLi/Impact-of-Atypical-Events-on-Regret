#### Preamble ####
# Purpose: Figure 1 replication
# Author: Xutao Chen
# Date: 12 February 2024
# Contact: xutao.chen@mail.utoronto.ca

## Data Sources:

# Kutscher, L., & Feldman, G. (2019). The impact of past behaviour normality on regret: 
# replication and extension of three experiments of the exceptionality effect. Cognition and Emotion, 33(5), 901–914.  
#https://doi.org/10.1080/02699931.2018.1504747
# Data can be found on page 7.

#install.packages("reshape2")

library(ggplot2)

data <- data.frame(
  Scenario = c("Smith - Routine", "Jones - Exception"),
  PerceivedRegret = c(7.9, 92.1),
  InjunctiveSocialNorms = c(4.7, 95.3),
  DescriptiveSocialNorms = c(90.6, 9.4),
  NegativeAffect = c(7.3, 92.7)
)


data_long <- reshape2::melt(data, id.vars = 'Scenario')


p <- ggplot(data_long, aes(x = variable, y = value, fill = Scenario)) +
  geom_bar(stat = 'identity', position = position_dodge(width = 0.7), width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Proportions for perceived regret, injunctive social norms, descriptive social norms, and negative affect',
       y = '',
       title = '',
       fill = 'Scenario') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top')

print(p)

