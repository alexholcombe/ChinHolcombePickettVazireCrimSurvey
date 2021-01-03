library(readxl)

agnoli <- readxl::read_excel ("Makel_Agnoli/Agnoli_orig_OSF_files/ItalianQRPData.xlsx", skip=2)

library(dplyr)
library(tidyr)
library(ggplot2)

relevant<- agnoli %>% 
    dplyr::select_at(dplyr::vars(ends_with("have adopted this practice"),starts_with("In a research article")))
#yields 10 variables, which is same number as in their plot

#Delete  In a research article  from the column names
shortNames <- gsub("^.*?In a research article, ", "", colnames(relevant))
#Delete another phrase from the column names
shortNames <- gsub(" : The percentage of psychological researchers that have adopted this practice","", shortNames)
shorterNames<- substr(shortNames,1,30)

relevantShorterNames <- relevant
colnames(relevantShorterNames) <- shorterNames
longAgnoli <- relevantShorterNames %>% pivot_longer(cols=everything(), names_to="practice")

longAgnoli$practice <- as.factor(longAgnoli$practice)
levels(longAgnoli$practice)

#Give practice same name as for other studies
longAgnoli <- longAgnoli %>% mutate(
  practiceStandardName = case_when(
    practice=="“round” a p value (for example" ~ "Round p-values",
    practice=="affirm that demographic variab" ~ "Mislead about demographic\nvariable effects",
    practice=="Decide to collect more data af" ~ "Sample selectively",
    practice=="Decide whether to exclude some" ~ "Exclude data selectively",
    practice=="Falsify the data"               ~ "Falsify data\n(closest to Hide imputation)",
    practice=="not report all the conditions " ~ "Not report all conditions\n(closest to Omit n.s. studies or variables)",
    practice=="not report all the dependent m" ~ "Not report all DVs\n(closest to Omit n.s. studies or variables)",
    practice=="report an unexpected result as" ~ "HARK",
    practice=="report in a selective manner o" ~ "Report only studies that worked\n(closest to Omit n.s. studies or variables)",
    practice=="Stop collecting data early tha" ~ "Stop collecting data bc sig\n(closest to Sample selectively)",
    TRUE    ~ "ERROR! unknown practice"
  )
)
longAgnoli$practiceStandardName <- as.factor(longAgnoli$practiceStandardName)

#Order the practices in the same order as am doing for all the other datasets.
#Order from least endorsed to most endorsed, as found in Jason Chin et al. criminology data.
#"Hide imputation", "Hide problems", "Round p-values", "HARK", "Exclude data selectively", "Drop covariates selectively",
# "Sample selectively", "Omitting nonsignificant studies or variables", "Switch analysis selectively", "Underreport results" 

longAgnoli$practiceStandardName <- factor(longAgnoli$practiceStandardName,
                                        levels = c("Falsify data\n(closest to Hide imputation)",
                                                   "Round p-values", 
                                                   "HARK", 
                                                   "Exclude data selectively",
                                                   "Stop collecting data bc sig\n(closest to Sample selectively)", 
                                                   "Not report all conditions\n(closest to Omit n.s. studies or variables)",
                                                   "Not report all DVs\n(closest to Omit n.s. studies or variables)",
                                                   "Sample selectively",
                                                   "Report only studies that worked\n(closest to Omit n.s. studies or variables)",
                                                   "Mislead about demographic\nvariable effects" ) )

saveRDS(longAgnoli, file = "Makel_Agnoli/Agnoli_LongQRPs.rds")

#Plot with truncated Agnoli practice names                                    
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

#With standard variable names
prevCloudStd <-  ggplot( drop_na(longAgnoli), aes(x = practiceStandardName, y = value) ) + 
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
show(prevCloudStd)
ggplot2::ggsave("Makel_Agnoli/Agnoli_QRPperceivedPrevalenceCloudStdNames.png", width = 30, height = 20, units = "cm")


#Also calculate correlation matrix
corrmatrix = cor(drop_na(relevant))

#install.packages("corrplot")
library(corrplot)
#corrplot(corrmatrix)
