#### Preamble ####
# Purpose: Figure 3 replication
# Author: Dailin Li
# Date: 12 February 2024
# Contact: dailin.li@mail.utoronto.ca

## Data Sources:

# Kutscher, L., & Feldman, G. (2019). The impact of past behaviour normality on regret: 
# replication and extension of three experiments of the exceptionality effect. Cognition and Emotion, 33(5), 901–914.  
#https://doi.org/10.1080/02699931.2018.1504747


library(here)
library(dplyr)
library(ggplot2)

data <-  read_csv(
  file = here("inputs/data/osf-past-normality-regret-replication-exp2-data-v2.csv"))

# Filter out NA values in the condition before plotting
data_filtered <- data %>% filter(!is.na(Sc3conditionl1))

exceptioncombinedregretplot <- ggplot(data_filtered, aes(x=Sc3conditionl1, y=regretaggrecoded1, fill=Sc3conditionl1)) + 
  geom_violin(trim=FALSE, alpha=0.5) + # Set transparency for violins
  geom_jitter(shape=16, position=position_jitter(height=0.1, width=0.2), alpha=0.7, size=2) + # Adjust the alpha and size for jitter
  scale_fill_manual(values=c("Routine"="blue", "Exception"="red")) + # Change color scheme
  theme_minimal(base_size = 14) + # Use a minimal theme as a base and increase base font size
  theme(legend.position="none", # Remove legend if color distinction is clear
        axis.text.x = element_text(angle = 45, hjust = 1), # Angle x-axis text for better fit
        panel.grid.major = element_line(colour="grey80"), # Lighten grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        plot.title = element_text(hjust = 0.5)) + # Center the plot title
  labs(x="Condition", 
       y="Regret Level", 
       title="Comparison of Regret Levels between Routine and Exception Conditions") + # Add labels and title
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2, color="black") + # Error bars in black
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black") # Mean points in black

exceptioncombinedregretplot

