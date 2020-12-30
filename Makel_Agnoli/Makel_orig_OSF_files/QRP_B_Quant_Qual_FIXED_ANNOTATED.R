data <- read.csv ("Full_Responses.csv")

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
#T_data_R3_2 <- dplyr::select(data, R3_3, Quant): This section creates a subset dataset that
#contains the item of interest and variables. US and Career are included for both analyses covering
#career stage and location due to their similar coding structure.
#T_data_R3_3  <- T_data_R3_3 [!is.na(T_data_R3_3$R3_3),]: This section of the codes removes empty cells.
#test_1 <- aov (R3_2 ~ Career, data = T_data_R3_3): This section runs an ANOVA across career groups. 
#hsd_1 <- TukeyHSD(test_1): This section runs an  Tukey Honest Significant Differences across career groups. 
#A Tukey HSD is a multiple comparison test. 

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#ktest is used across scripts. To call a specific ktest, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 


T_data_R3_3 <- dplyr::select(data, R3_3, Quant)
T_data_R3_3  <- T_data_R3_3 [!is.na(T_data_R3_3$R3_3),]
Quant <- subset (T_data_R3_3, T_data_R3_3$Quant == 1)
Qual <- subset (T_data_R3_3, T_data_R3_3$Quant == 0)
q_wtest_1 <- wilcox.test (Quant$R3_3, Qual$R3_3)
summary (Quant)
summary (Qual)

T_data_R4_3 <- dplyr::select(data, R4_3, Quant)
T_data_R4_3  <- T_data_R4_3 [!is.na(T_data_R4_3$R4_3),]
Quant <- subset (T_data_R4_3, T_data_R4_3$Quant == 1)
Qual <- subset (T_data_R4_3, T_data_R4_3$Quant == 0)
q_wtest_2 <- wilcox.test (Quant$R4_3, Qual$R4_3)
summary (Quant)
summary (Qual)

T_data_R5_3 <- dplyr::select(data, R5_3, Quant)
T_data_R5_3  <- T_data_R5_3 [!is.na(T_data_R5_3$R5_3),]
Quant <- subset (T_data_R5_3, T_data_R5_3$Quant == 1)
Qual <- subset (T_data_R5_3, T_data_R5_3$Quant == 0)
q_wtest_3 <- wilcox.test (Quant$R5_3, Qual$R5_3)
summary (Quant)
summary (Qual)

T_data_R6_3 <- dplyr::select(data, R6_3, Quant)
T_data_R6_3  <- T_data_R6_3 [!is.na(T_data_R6_3$R6_3),]
Quant <- subset (T_data_R6_3, T_data_R6_3$Quant == 1)
Qual <- subset (T_data_R6_3, T_data_R6_3$Quant == 0)
q_wtest_4 <- wilcox.test (Quant$R6_3, Qual$R6_3)
summary (Quant)
summary (Qual)


T_data_R7_3 <- dplyr::select(data, R7_3, Quant)
T_data_R7_3  <- T_data_R7_3 [!is.na(T_data_R7_3$R7_3),]
Quant <- subset (T_data_R7_3, T_data_R7_3$Quant == 1)
Qual <- subset (T_data_R7_3, T_data_R7_3$Quant == 0)
q_wtest_5 <- wilcox.test (Quant$R7_3, Qual$R7_3)
summary (Quant)
summary (Qual)

T_data_R8_3 <- dplyr::select(data, R8_3, Quant)
T_data_R8_3  <- T_data_R8_3 [!is.na(T_data_R8_3$R8_3),]
Quant <- subset (T_data_R8_3, T_data_R8_3$Quant == 1)
Qual <- subset (T_data_R8_3, T_data_R8_3$Quant == 0)
q_wtest_6 <- wilcox.test (Quant$R8_3, Qual$R8_3)
summary (Quant)
summary (Qual)


T_data_R9_3 <- dplyr::select(data, R9_3, Quant)
T_data_R9_3  <- T_data_R9_3 [!is.na(T_data_R9_3$R9_3),]
Quant <- subset (T_data_R9_3, T_data_R9_3$Quant == 1)
Qual <- subset (T_data_R9_3, T_data_R9_3$Quant == 0)
q_wtest_7 <- wilcox.test (Quant$R9_3, Qual$R9_3)
summary (Quant)
summary (Qual)


T_data_R10_3 <- dplyr::select(data, R10_3, Quant)
T_data_R10_3  <- T_data_R10_3 [!is.na(T_data_R10_3$R10_3),]
Quant <- subset (T_data_R10_3, T_data_R10_3$Quant == 1)
Qual <- subset (T_data_R10_3, T_data_R10_3$Quant == 0)
q_wtest_8 <- wilcox.test (Quant$R10_3, Qual$R10_3)
summary (Quant)
summary (Qual)


T_data_R11_3 <- dplyr::select(data, R11_3, Quant)
T_data_R11_3  <- T_data_R11_3 [!is.na(T_data_R11_3$R11_3),]
Quant <- subset (T_data_R11_3, T_data_R11_3$Quant == 1)
Qual <- subset (T_data_R11_3, T_data_R11_3$Quant == 0)
q_wtest_9 <- wilcox.test (Quant$R11_3, Qual$R11_3)
summary (Quant)
summary (Qual)

T_data_R12_3 <- dplyr::select(data, R12_3, Quant)
T_data_R12_3  <- T_data_R12_3 [!is.na(T_data_R12_3$R12_3),]
Quant <- subset (T_data_R12_3, T_data_R12_3$Quant == 1)
Qual <- subset (T_data_R12_3, T_data_R12_3$Quant == 0)
q_wtest_10 <- wilcox.test (Quant$R12_3, Qual$R12_3)
summary (Quant)
summary (Qual)

T_data_R13_3 <- dplyr::select(data, R13_3, Quant)
T_data_R13_3  <- T_data_R13_3 [!is.na(T_data_R13_3$R13_3),]
Quant <- subset (T_data_R13_3, T_data_R13_3$Quant == 1)
Qual <- subset (T_data_R13_3, T_data_R13_3$Quant == 0)
q_wtest_11 <- wilcox.test (Quant$R13_3, Qual$R13_3)
summary (Quant)
summary (Qual)

T_data_R14_3 <- dplyr::select(data, R14_3, Quant)
T_data_R14_3  <- T_data_R14_3 [!is.na(T_data_R14_3$R14_3),]
Quant <- subset (T_data_R14_3, T_data_R14_3$Quant == 1)
Qual <- subset (T_data_R14_3, T_data_R14_3$Quant == 0)
q_wtest_12 <- wilcox.test (Quant$R14_3, Qual$R14_3)
summary (Quant)
summary (Qual)


T_data_R15_3 <- dplyr::select(data, R15_3, Quant)
T_data_R15_3  <- T_data_R15_3 [!is.na(T_data_R15_3$R15_3),]
Quant <- subset (T_data_R15_3, T_data_R15_3$Quant == 1)
Qual <- subset (T_data_R15_3, T_data_R15_3$Quant == 0)
q_wtest_13 <- wilcox.test (Quant$R15_3, Qual$R15_3)
summary (Quant)
summary (Qual)


T_data_R16_3 <- dplyr::select(data, R16_3, Quant)
T_data_R16_3  <- T_data_R16_3 [!is.na(T_data_R16_3$R16_3),]
Quant <- subset (T_data_R16_3, T_data_R16_3$Quant == 1)
Qual <- subset (T_data_R16_3, T_data_R16_3$Quant == 0)
q_wtest_14 <- wilcox.test (Quant$R16_3, Qual$R16_3)
summary (Quant)
summary (Qual)


T_data_R17_3 <- dplyr::select(data, R17_3, Quant)
T_data_R17_3  <- T_data_R17_3 [!is.na(T_data_R17_3$R17_3),]
Quant <- subset (T_data_R17_3, T_data_R17_3$Quant == 1)
Qual <- subset (T_data_R17_3, T_data_R17_3$Quant == 0)
q_wtest_15 <- wilcox.test (Quant$R17_3, Qual$R17_3)
summary (Quant)
summary (Qual)

#These are calls and will produce the wilcox test output by item. This output will
#also produce means. 


q_wtest_1
q_wtest_2
q_wtest_3
q_wtest_4
q_wtest_5
q_wtest_6
q_wtest_7
q_wtest_8
q_wtest_9
q_wtest_10
q_wtest_11
q_wtest_12
q_wtest_13
q_wtest_14
q_wtest_15





