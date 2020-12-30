#Commentary on Code

#These are the required R packages.

library ("pgirmess")
library ("stats")
library ("dplyr")


#This portion of the code is seperated by item where R3-R17 corresponds 
#to individual survey items. The correspondence is as follow:
#R3- Omitting nonSignificant studies or variables 
#R4- Omitting nonSignificant covariates 
#R5- HARKing 
#R6- Omitting analyses 
#R7- Rounding p-values 
#R8- Data Exclusion ARKing 
#R9- Data peeking 
#R10- Analysis Gaming 
#R11- Hiding methodological problems 
#R12- Filling in missing data 
#R13- Preregistration 
#R14- Data Sharing 
#R15- Materials Sharing      
#R16- Replication 
#R17- Open Access 


#The code for each item is as follow 
#T_data_R3_2 <- dplyr::select(data, R3_2, US, Career): This section creates a subset dataset that
#contains the item of interest and variables. US and Career are included for both analyses covering
#career stage and location due to their similar coding structure.
#T_data_R3_2  <- T_data_R3_4 [!is.na(T_data_R3_2$R3_2),]: This section of the codes removes empty cells.
#test_1 <- aov (R3_2 ~ Career, data = T_data_R3_2): This section runs an ANOVA across career groups. 
#hsd_1 <- TukeyHSD(test_1): This section runs an  Tukey Honest Significant Differences across career groups. 
#A Tukey HSD is a multiple comparison test. 

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#ktest is used across scripts. To call a specific ktest, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 



data <- read.csv ("Full_Responses.csv")
T_data_R3_2 <- dplyr::select(data, R3_2, US, Career)
T_data_R3_2  <- T_data_R3_2 [!is.na(T_data_R3_2$R3_2),]
test_1 <- aov (R3_2 ~ Career, data = T_data_R3_2)
hsd_1 <- TukeyHSD(test_1)

T_data_R4_2 <- dplyr::select(data, R4_2, US, Career)
T_data_R4_2  <- T_data_R4_2 [!is.na(T_data_R4_2$R4_2),]
test_2 <- aov (R4_2 ~ Career, data = T_data_R4_2)
hsd_2 <- TukeyHSD(test_2)


T_data_R5_2 <- dplyr::select(data, R5_2, US, Career)
T_data_R5_2  <- T_data_R5_2 [!is.na(T_data_R5_2$R5_2),]
test_3 <- aov (R5_2 ~ Career, data = T_data_R5_2)
hsd_3 <- TukeyHSD(test_3)


T_data_R6_2 <- dplyr::select(data, R6_2, US, Career)
T_data_R6_2  <- T_data_R6_2 [!is.na(T_data_R6_2$R6_2),]
test_4 <- aov (R6_2 ~ Career, data = T_data_R6_2)
hsd_4 <- TukeyHSD(test_4)


T_data_R7_2 <- dplyr::select(data, R7_2, US, Career)
T_data_R7_2  <- T_data_R7_2 [!is.na(T_data_R7_2$R7_2),]
test_5 <- aov (R7_2 ~ Career, data = T_data_R7_2)
hsd_5 <- TukeyHSD(test_5)


T_data_R8_2 <- dplyr::select(data, R8_2, US, Career)
T_data_R8_2  <- T_data_R8_2 [!is.na(T_data_R8_2$R8_2),]
test_6 <- aov (R8_2 ~ Career, data = T_data_R8_2)
hsd_6 <- TukeyHSD(test_6)

T_data_R9_2 <- dplyr::select(data, R9_2, US, Career)
T_data_R9_2  <- T_data_R9_2 [!is.na(T_data_R9_2$R9_2),]
test_7 <- aov (R9_2 ~ Career, data = T_data_R9_2)
hsd_7 <- TukeyHSD(test_7)


T_data_R10_2 <- dplyr::select(data, R10_2, US, Career)
T_data_R10_2  <- T_data_R10_2 [!is.na(T_data_R10_2$R10_2),]
test_8 <- aov (R10_2 ~ Career, data = T_data_R10_2)
hsd_8 <- TukeyHSD(test_8)

T_data_R11_2 <- dplyr::select(data, R11_2, US, Career)
T_data_R11_2  <- T_data_R11_2 [!is.na(T_data_R11_2$R11_2),]
test_9 <- aov (R11_2 ~ Career, data = T_data_R11_2)
hsd_9 <- TukeyHSD(test_9)

T_data_R12_2 <- dplyr::select(data, R12_2, US, Career)
T_data_R12_2  <- T_data_R12_2 [!is.na(T_data_R12_2$R12_2),]
test_10 <- aov (R12_2 ~ Career, data = T_data_R12_2)
hsd_10 <- TukeyHSD(test_10)


T_data_R13_2 <- dplyr::select(data, R13_2, US, Career)
T_data_R13_2  <- T_data_R13_2 [!is.na(T_data_R13_2$R13_2),]
test_11 <- aov (R13_2 ~ Career, data = T_data_R13_2)
hsd_11 <- TukeyHSD(test_11)


T_data_R14_2 <- dplyr::select(data, R14_2, US, Career)
T_data_R14_2  <- T_data_R14_2 [!is.na(T_data_R14_2$R14_2),]
test_12 <- aov (R14_2 ~ Career, data = T_data_R14_2)
hsd_12 <- TukeyHSD(test_12)

T_data_R15_2 <- dplyr::select(data, R15_2, US, Career)
T_data_R15_2  <- T_data_R15_2 [!is.na(T_data_R15_2$R15_2),]
test_13 <- aov (R15_2 ~ Career, data = T_data_R15_2)
hsd_13 <- TukeyHSD(test_13)


T_data_R16_2 <- dplyr::select(data, R16_2, US, Career)
T_data_R16_2  <- T_data_R16_2 [!is.na(T_data_R16_2$R16_2),]
test_14 <- aov (R16_2 ~ Career, data = T_data_R16_2)
hsd_14 <- TukeyHSD(test_14)


T_data_R17_2 <- dplyr::select(data, R17_2, US, Career)
T_data_R17_2  <- T_data_R17_2 [!is.na(T_data_R17_2$R17_2),]
test_15 <- aov (R17_2 ~ Career, data = T_data_R17_2)
hsd_15 <- TukeyHSD(test_15)

#These are calls and will produe all output by item. The summary() refers to the ANOVA test. 
#The HSD refers to the Tukey Honest Significant Differences multiple comparison tests. 

summary(test_1)
hsd_1
summary(test_2)
hsd_2
summary(test_3)
hsd_3
summary(test_4)
hsd_4
summary(test_5)
hsd_5
summary(test_6)
hsd_6
summary(test_7)
hsd_7
summary(test_8)
hsd_8
summary(test_9)
hsd_9
summary(test_10)
hsd_10
summary(test_11)
hsd_11
summary(test_12)
hsd_12
summary(test_13)
hsd_13
summary(test_14)
hsd_14
summary(test_15)
hsd_15


#This section of the code produces descriptive statistics by groups. dplyr is used to summarize
#each item by career option using the group_by fundtion. 
#Total respondents per group and per item, mean of the group per item, 
#and standard deviation of the group per item is reported. 
#The option "%0.3f" dictates the number of significant digits. Changing this to "%0.1f" would
#correspond to a single significant digit instead of three.

#Note. This section of the code was debugged following the release of R 3.6.1. 
#A reader seeking to replicate this section of the code might experience an error associated with 
#the function n(). As of 10/28/2019, this section of the code works correctly and as it should. 
#Stability was achieved through forcing the functions to use dplyr through the "dplyr::" code. 
#Without this code portion, the code will not work though this might be updated in a new iteration 
#of dplyr making "dplyr::" redundant. A user wanting to replicate should be aware of this if 
#using this code and wishes to streamline it.


dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R3_2, na.rm = TRUE)),
    sd = sd(R3_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R4_2, na.rm = TRUE)),
    sd = sd(R4_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R5_2, na.rm = TRUE)),
    sd = sd(R5_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R6_2, na.rm = TRUE)),
    sd = sd(R6_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R7_2, na.rm = TRUE)),
    sd = sd(R7_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R8_2, na.rm = TRUE)),
    sd = sd(R8_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R9_2, na.rm = TRUE)),
    sd = sd(R9_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R10_2, na.rm = TRUE)),
    sd = sd(R10_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R11_2, na.rm = TRUE)),
    sd = sd(R11_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R12_2, na.rm = TRUE)),
    sd = sd(R12_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R13_2, na.rm = TRUE)),
    sd = sd(R13_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R14_2, na.rm = TRUE)),
    sd = sd(R14_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R15_2, na.rm = TRUE)),
    sd = sd(R15_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(R16_2, na.rm = TRUE)),
    sd = sd(R16_2, na.rm = TRUE)
  )

dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count = n(),
    mean = sprintf("%0.3f", mean(R17_2, na.rm = TRUE)),
    sd = sd(R17_2, na.rm = TRUE)
  )

