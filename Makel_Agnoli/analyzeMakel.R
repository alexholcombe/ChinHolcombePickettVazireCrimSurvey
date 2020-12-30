#These are the required packages. This code will automatically load them or install then load
#if they are not present. 

load.lib <- c("reshape2", "scales", "ggplot2", "dplyr", "plyr", "data.table")
install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

#The first step is to split the dataset into two portions. The first portion is for the 
#QRP items, the second is the open science practices. 

data <- read.csv ("Makel_Agnoli/Makel_orig_OSF_files/Full_Responses.csv")
data_h <- cbind (data$R13_2, data$R14_2, data$R15_2, data$R16_2, data$R17_2)

data_g <- cbind (data$R3_2, data$R4_2, data$R5_2, data$R6_2, data$R7_2,
		     data$R8_2, data$R9_2, data$R10_2, data$R11_2, data$R12_2)

gtibble<- tibble(data.frame(data_g))

#The variables are then renamed to correspond to the names used in our study. 
colnames(gtibble) <- c("ONSV*", "Omitting nonSignificant covariates",
                  "HARKing", "Omitting analyses", "Rounding p-values", "Data Exclusion ARKing",
                  "Data peeking","Analysis Gaming", "Hiding methodological problems", "Filling in missing data")

library(tidyr)
longQRPs <- gtibble %>% pivot_longer(cols=everything(), names_to="practice")

# longQRPs <- longQRPs %>% mutate(
#   practiceLongName = case_when(
#     practice=="biasedreporting_should" ~ "File drawer",
#     practice=="aspredicted_should" ~ "HARK",
#     practice=="biasedcov_should" ~ "Drop covariates selectively",
#     practice=="stopping_should" ~ "Sample selectively",
#     practice=="omittinganalyses_should" ~ "Underreport results",
#     practice=="pvalround_should" ~ "Round p-values",
#     practice=="analysischanging_should" ~ "Switch analysis selectively",
#     practice=="outliers_should" ~ "Exclude data selectively",
#     practice=="hidingproblems_should" ~ "Hide data problems",
#     practice=="fillingin_should" ~ "Hide imputation",
#     TRUE ~ "ERROR! unknown practice"
#   )
# )
# longQRPs$practiceLongName <- as.factor(longQRPs$practiceLongName)

practiceOrderDesiredNeverAscending <- c(6,5,7,4,2,1,8,3,9,10) 
longQRPs$practiceLongName <- factor(longQRPs$practiceLongName,
                                    levels = levels(longQRPs$practiceLongName)[practiceOrderDesiredNeverAscending])

#https://datavizpyr.com/rain-cloud-plots-using-half-violin-plot-with-jittered-data-points-in-r/
#Load half violin plot: geom_flat_violin()
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")

QRPprevCloud <-  ggplot( drop_na(longQRPs), aes(x = practice, y = value) ) + 
  theme_bw() +
  geom_flat_violin(fill="gray32",color="gray32", position = position_nudge(x = .18, y = 0)) +
  geom_jitter(alpha=0.1, size=.5, width=0.15, height=0) +
  stat_summary(fun = mean, geom = "point", color='green4', size=4, alpha=0.82) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, width=0.1, color='green4', alpha=0.82) +
  #geom_jitter(width=0.2, size=.2, alpha=0.1)+
  xlab("QRP") + ylab("perceived % others use at least once") + ylim(0,100) +
  labs(title='Perceived prevalence of QRPs',
       subtitle='What percent of psychologists would you say have engaged in this practice on at least one occasion?') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) 
show(QRPprevCloud)
ggsave("Makel_Agnoli/Makel_QRPperceivedPrevalenceCloud.png", width = 30, height = 20, units = "cm")

#OSPs
htibble<- tibble(data.frame(data_h))

#The variables are then renamed to correspond to the names used in our study. 
colnames(htibble) <- c("Preregistration", "Data Sharing", "Materials Sharing", "Replication","Open Access")

longOSPs <- htibble %>% pivot_longer(cols=everything(), names_to="practice")

OSPprevCloud <-  ggplot( drop_na(longOSPs), aes(x = practice, y = value) ) + 
  theme_bw() +
  geom_flat_violin(fill="gray32",color="gray32", position = position_nudge(x = .18, y = 0)) +
  geom_jitter(alpha=0.1, size=.5, width=0.15, height=0) +
  stat_summary(fun = mean, geom = "point", color='green4', size=4, alpha=0.82) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, width=0.1, color='green4', alpha=0.82) +
  #geom_jitter(width=0.2, size=.2, alpha=0.1)+
  xlab("QRP") + ylab("perceived % others use at least once") + ylim(0,100) +
  labs(title='Perceived prevalence of QRPs',
       subtitle='What percent of psychologists would you say have engaged in this practice on at least one occasion?') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) 
show(OSPprevCloud)
ggsave("Makel_Agnoli/Makel_OSPperceivedPrevalenceCloud.png", width = 30, height = 20, units = "cm")

#Also calculate correlation matrix
all <- cbind(gtibble,htibble)
corrmatrix = cor(drop_na(all))

#install.packages("corrplot")
library(corrplot)
corrplot(corrmatrix)

#First need to pivot the data back to wide format, so each variable has its own column
#Could use gtibble and htibble because already wide but they have the old variable names
#longOSPs$id <- 1:nrow(longOSPs)
#longOSPs %>% pivot_wider(names_from = practice, values_from = value)
#  pivot_wider(cols=everything(), names_to="practice")

