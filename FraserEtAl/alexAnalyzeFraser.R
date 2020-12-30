library(readr)
fraser <- read_csv("FraserEtAl/Fraser-orig-OSF-files/combined.csv",col_names=T)
#based on their "Paper analysis.R"

#Hannah's questions:
#1.Not reporting studies or variables that failed to reach statistical significance (e.g. p ≤0.05) or some other desired statistical threshold.
#2.Not reporting covariates that failed to reach statistical significance (e.g. p ≤0.05) or some other desired statistical threshold.
#3.Reporting an unexpected finding or a result from exploratory analysis as having been predicted from the start.
#4.SLIGHTLY DIFFERENT FROM JASON'S VERSIONReporting a set of statistical models as the complete tested set when other candidate models were also tested.
#5.Rounding-off a p value or other quantity to meet a pre-specified threshold (e.g., reporting p = 0.054 as p = 0.05 or p = 0.013 as p = 0.01).
#6.Deciding to exclude data points after first checking the impact on statistical significance (e.g. p ≤ 0.05) or some other desired statistical threshold.
#7.Collecting more data for a study after first inspecting whether the results are statistically significant (e.g. p ≤ 0.05).
#8.Changing to another type of statistical analysis after the analysis initially chosen failed to reach statistical significance (e.g. p ≤ 0.05) or some other desired statistical threshold.
#9.Not disclosing known problems in the method and analysis, or problems with the data quality, that potentially impact conclusions.
#10.Filling in missing data points without identifying those data as simulated.

#Jason's questions:
#1.Not reporting studies or variables that failed to reach statistical significance (e.g. p < 0.05) or some other desired statistical threshold.”
#2.Not reporting covariates that failed to reach statistical significance (e.g. p < 0.05) or some other desired statistical threshold.”
#3.Reporting an unexpected finding or a result from exploratory analysis as having been predicted from the start.”
#4.SLIGHTLY DIFFERENT IN JASON'S VERSION. Reporting a set of results as the complete set of analyses when other analyses were also conducted.”
#5.Rounding-off a p value or other quantity to meet a pre-specified threshold (e.g., reporting p = 0.054 as p = 0.05 or p = 0.013 as p = 0.01).”
#6.Deciding to exclude data points after first checking the impact on statistical significance (e.g. p < 0.05) or some other desired statistical threshold.”
#7.Collecting more data for a study after first inspecting whether the results are statistically significant (e.g. p < 0.05).”
#8.Changing to another type of statistical analysis after the analysis initially chosen failed to reach statistical significance (e.g. p < 0.05) or some other desired statistical threshold.”
#9.Not disclosing known problems in the method and analysis, or problems with the data quality, that potentially impact conclusions.”
#10.Filling in missing data points without identifying those data as simulated.”


#Rows 1 to 573 are presumably ecologists
#Row 574 is a header row for evolutonary biologists
ecologists <- fraser[1:573,]
evobioQuestions<- fraser[574,] #These are the truncated questions in Hannah's csv file (already truncated in the file)

evobio <- fraser[575:nrow(fraser),]
ecologists$field<- "ecology"
evobio$field<- "evolutionary biology"

combined<- rbind(ecologists,evobio)

prevalenceCols<- c(73,9,14,19,24,29,34,39,44,49,54)
prevalenceQuestions<- as.list(evobioQuestions[prevalenceCols]) 

QRPs<- combined[prevalenceCols]

colnames(QRPs) <- c("field","QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10") #Hannah thinks of them this way
colnames(QRPs) <- c("field","unreported variables","unreported covariates","HARKing","unreported models",
                    "rounding ps","excluding data","adding data","switching analyses",
                    "undisclosed problems","fabrication")

#Change the variable names to the labels we use on our graphs, except for this field it's "Underreport models" instead of "Underreport results".
colnames(QRPs) <- c("field","File drawer","Drop covariates selectively","HARK","Underreport models",
                    "Round p-values","Exclude data selectively","Sample selectively","Switch analysis selectively",
                    "Hide problems","Hide imputation")

#Change all color schemes to Makel's black/white?

QRPs<- type_convert(QRPs) #let readr work out which columns are numeric now that I eliminated that pesky row 574
#QRPs<- QRPs %>% mutate_if(is.character,as.numeric)

library(dplyr)
library(tidyr)
library(ggplot2)


#Pivot all columns except field
longQRPs <- QRPs %>% pivot_longer(cols = "File drawer":"Hide imputation", names_to="practice")
#longQRPs <- QRPs %>% pivot_longer(cols = "unreported variables":"fabrication", names_to="practice")
#longQRPs <- QRPs %>% pivot_longer(cols=everything(), names_to="practice")

#https://datavizpyr.com/rain-cloud-plots-using-half-violin-plot-with-jittered-data-points-in-r/
#Load half violin plot: geom_flat_violin()
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")

prevCloud <-  ggplot( drop_na(longQRPs), aes(x = practice, y = value) ) + 
  facet_grid(field~.) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
  geom_jitter(alpha=0.1, size=.5, width=0.15, height=0) +
  stat_summary(fun = mean, geom = "point", color='green4', size=4, alpha=0.82) +
  stat_summary(fun.data = mean_cl_boot, fun.args=(conf.int=0.95), geom="errorbar", size=.9, width=0.1, color='green4', alpha=0.82) +
  geom_flat_violin(fill="gray32",color="gray32", position = position_nudge(x = .18, y = 0)) +
  #geom_jitter(width=0.2, size=.2, alpha=0.1)+
  xlab("QRP") + ylab("perceived % others use at least once") + ylim(0,100) +
  labs(title='Perceived prevalence of QRPs',
       subtitle='What percent of eco/evos would you say have engaged in this practice on at least one occasion?')
show(prevCloud)
ggsave("FraserEtAl/Fraser_QRPperceivedPrevalenceCloud.png", width = 30, height = 20, units = "cm")


#Also calculate correlation matrix
corrmatrix = cor(drop_na(  QRPs %>% select(-field)  ))

#install.packages("corrplot")
library(corrplot)
corrplot(corrmatrix)
