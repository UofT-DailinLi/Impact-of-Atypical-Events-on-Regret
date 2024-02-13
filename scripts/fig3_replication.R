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
library(ggplot2)
library(dplyr)
library(psych)



data <-  read_csv(
  file = here("inputs/data/osf-past-normality-regret-replication-exp2-data-v2.csv"))
str(data)


data$Sc3condition1 <- 0
data$compensationagg1 <- 0
data$regretagg1 <- 0
for (i in 1:nrow(data)){
  if (!is.na(data$Sc3_C1_text[i])){
    data$Sc3condition1[i] <- 1
    data$compensationagg1[i] <- data$sc3_c1_compensation[i]
    data$regretagg1[i] <- data$sc3_c1_regret[i]
  }
  else if (!is.na(data$Sc3_C2_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c2_compensation[i]
    data$regretagg1[i] <- data$sc3_c2_regret[i]
  }
  else if (!is.na(data$Sc3_C3_text[i])){
    data$Sc3condition1[i] <- 2
    data$compensationagg1[i] <- data$sc3_c3_compensation[i]
    data$regretagg1[i] <- data$sc3_c3_regret[i]
  }
  else {
    data$Sc3condition1[i] <- NA
    data$compensationagg1[i] <- NA
    data$regretagg1[i] <- NA
  }
}

#value labels
data$Sc3conditionl1<-factor(data$Sc3condition1,levels = c(1,2), labels=c("Routine", "Exception"))

# let's have a look at this
table(data$Sc3conditionl1)


#Adjust Copensation-Scale to original paper (Values from 0 to 10, instead of 1 to 11)
data$compensationaggrecoded1 = data$compensationagg1-1

#Adjust Regret Scale (Values 1 to 5, indead of 0 to 4)
data$regretaggrecoded1 = data$regretagg1+1

# Label Variables
names (data$compensationaggrecoded1) <- c("0", "100,000", "200,000", "300,000", "400,000", "500,000", "600,000", "700,000", "800,000", "900,000", "1,000,000")
names (data$regretaggrecoded1) <- c("no regret", "weak regret", "medium regret", "strong regret", "very strong regret")








data_filtered <- data %>% filter(!is.na(Sc3conditionl1))

# Enhanced compensation plot
exceptioncombinedcompensationplot <- ggplot(data_filtered, aes(x=Sc3conditionl1, y=compensationaggrecoded1, fill=Sc3conditionl1)) +
  geom_violin(trim=FALSE, alpha=0.6) + 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.7, size=2) +
  scale_fill_brewer(palette="Pastel1") + 
  theme_light(base_size = 16) + 
  labs(x="Condition", y="Compensation (in thousands)", title="Compensation Amount by Condition") +
  scale_y_continuous(labels=scales::comma) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color="black") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black")

exceptioncombinedcompensationplot


########
# Regret
########

exceptioncombinedregretplot <- ggplot(data_filtered, aes(x=Sc3conditionl1, y=regretaggrecoded1, fill=Sc3conditionl1)) +
  geom_violin(trim=FALSE, alpha=0.6) + 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.7, size=2) +
  scale_fill_brewer(palette="Pastel2") + 
  theme_light(base_size = 16) + 
  labs(x="Condition", y="Regret Level", title="Regret Level by Condition") +
  scale_y_continuous(breaks=1:5, labels=c("No Regret", "Weak", "Medium", "Strong", "Very Strong")) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color="black") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black")

exceptioncombinedregretplot

