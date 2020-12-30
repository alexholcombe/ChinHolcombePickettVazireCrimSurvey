library(readxl)

agnoli <- readxl::read_excel ("Makel_Agnoli/Agnoli_orig_OSF_files/ItalianQRPData.xlsx", skip=2)

library(dplyr)
library(tidyr)
library(ggplot2)

relevant<- agnoli %>% 
    dplyr::select_at(dplyr::vars(ends_with("have adopted this practice"),starts_with("In a research article")))
#yields 10 variables, which is same number as in their plot

shortNames <- gsub("^.*?In a research article, ","", colnames(relevant))
shortNames <- gsub(" : The percentage of psychological researchers that have adopted this practice","", shortNames)
shorterNames<- substr(shortNames,1,30)

colnames(relevant) <- shorterNames
longAgnoli <- relevant %>% pivot_longer(cols=everything(), names_to="practice")


# #The variables are then renamed to correspond to the names used in our study. 
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
# 
# practiceOrderDesiredNeverAscending <- c(6,5,7,4,2,1,8,3,9,10) 
# longQRPs$practiceLongName <- factor(longQRPs$practiceLongName,
#                                     levels = levels(longQRPs$practiceLongName)[practiceOrderDesiredNeverAscending])

prevCloud <-  ggplot( drop_na(longAgnoli), aes(x = practice, y = value) ) + 
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
show(prevCloud)
ggplot2::ggsave("Makel_Agnoli/Agnoli_QRPperceivedPrevalenceCloud.png", width = 30, height = 20, units = "cm")

#Also calculate correlation matrix
corrmatrix = cor(drop_na(relevant))

#install.packages("corrplot")
library(corrplot)
corrplot(corrmatrix)
