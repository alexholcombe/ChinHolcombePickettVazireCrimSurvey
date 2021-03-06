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
#T_data_R3_2 <- dplyr::select(data, R3_2, Quant): This section creates a subset dataset that
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


T_data_R3_4 <- dplyr::select(data, R3_4, Quant)
T_data_R3_4  <- T_data_R3_4 [!is.na(T_data_R3_4$R3_4),]
Quant <- subset (T_data_R3_4, T_data_R3_4$Quant == 1)
Qual <- subset (T_data_R3_4, T_data_R3_4$Quant == 0)
q_wtest_1 <- wilcox.test (Quant$R3_4, Qual$R3_4)
summary (Quant)
summary (Qual)

T_data_R4_4 <- dplyr::select(data, R4_4, Quant)
T_data_R4_4  <- T_data_R4_4 [!is.na(T_data_R4_4$R4_4),]
Quant <- subset (T_data_R4_4, T_data_R4_4$Quant == 1)
Qual <- subset (T_data_R4_4, T_data_R4_4$Quant == 0)
q_wtest_2 <- wilcox.test (Quant$R4_4, Qual$R4_4)
summary (Quant)
summary (Qual)

T_data_R5_4 <- dplyr::select(data, R5_4, Quant)
T_data_R5_4  <- T_data_R5_4 [!is.na(T_data_R5_4$R5_4),]
Quant <- subset (T_data_R5_4, T_data_R5_4$Quant == 1)
Qual <- subset (T_data_R5_4, T_data_R5_4$Quant == 0)
q_wtest_3 <- wilcox.test (Quant$R5_4, Qual$R5_4)
summary (Quant)
summary (Qual)

T_data_R6_4 <- dplyr::select(data, R6_4, Quant)
T_data_R6_4  <- T_data_R6_4 [!is.na(T_data_R6_4$R6_4),]
Quant <- subset (T_data_R6_4, T_data_R6_4$Quant == 1)
Qual <- subset (T_data_R6_4, T_data_R6_4$Quant == 0)
q_wtest_4 <- wilcox.test (Quant$R6_4, Qual$R6_4)
summary (Quant)
summary (Qual)


T_data_R7_4 <- dplyr::select(data, R7_4, Quant)
T_data_R7_4  <- T_data_R7_4 [!is.na(T_data_R7_4$R7_4),]
Quant <- subset (T_data_R7_4, T_data_R7_4$Quant == 1)
Qual <- subset (T_data_R7_4, T_data_R7_4$Quant == 0)
q_wtest_5 <- wilcox.test (Quant$R7_4, Qual$R7_4)
summary (Quant)
summary (Qual)

T_data_R8_4 <- dplyr::select(data, R8_4, Quant)
T_data_R8_4  <- T_data_R8_4 [!is.na(T_data_R8_4$R8_4),]
Quant <- subset (T_data_R8_4, T_data_R8_4$Quant == 1)
Qual <- subset (T_data_R8_4, T_data_R8_4$Quant == 0)
q_wtest_6 <- wilcox.test (Quant$R8_4, Qual$R8_4)
summary (Quant)
summary (Qual)


T_data_R9_4 <- dplyr::select(data, R9_4, Quant)
T_data_R9_4  <- T_data_R9_4 [!is.na(T_data_R9_4$R9_4),]
Quant <- subset (T_data_R9_4, T_data_R9_4$Quant == 1)
Qual <- subset (T_data_R9_4, T_data_R9_4$Quant == 0)
q_wtest_7 <- wilcox.test (Quant$R9_4, Qual$R9_4)
summary (Quant)
summary (Qual)


T_data_R10_4 <- dplyr::select(data, R10_4, Quant)
T_data_R10_4  <- T_data_R10_4 [!is.na(T_data_R10_4$R10_4),]
Quant <- subset (T_data_R10_4, T_data_R10_4$Quant == 1)
Qual <- subset (T_data_R10_4, T_data_R10_4$Quant == 0)
q_wtest_8 <- wilcox.test (Quant$R10_4, Qual$R10_4)
summary (Quant)
summary (Qual)


T_data_R11_4 <- dplyr::select(data, R11_4, Quant)
T_data_R11_4  <- T_data_R11_4 [!is.na(T_data_R11_4$R11_4),]
Quant <- subset (T_data_R11_4, T_data_R11_4$Quant == 1)
Qual <- subset (T_data_R11_4, T_data_R11_4$Quant == 0)
q_wtest_9 <- wilcox.test (Quant$R11_4, Qual$R11_4)
summary (Quant)
summary (Qual)

T_data_R12_4 <- dplyr::select(data, R12_4, Quant)
T_data_R12_4  <- T_data_R12_4 [!is.na(T_data_R12_4$R12_4),]
Quant <- subset (T_data_R12_4, T_data_R12_4$Quant == 1)
Qual <- subset (T_data_R12_4, T_data_R12_4$Quant == 0)
q_wtest_10 <- wilcox.test (Quant$R12_4, Qual$R12_4)
summary (Quant)
summary (Qual)

T_data_R13_4 <- dplyr::select(data, R13_4, Quant)
T_data_R13_4  <- T_data_R13_4 [!is.na(T_data_R13_4$R13_4),]
Quant <- subset (T_data_R13_4, T_data_R13_4$Quant == 1)
Qual <- subset (T_data_R13_4, T_data_R13_4$Quant == 0)
q_wtest_11 <- wilcox.test (Quant$R13_4, Qual$R13_4)
summary (Quant)
summary (Qual)

T_data_R14_4 <- dplyr::select(data, R14_4, Quant)
T_data_R14_4  <- T_data_R14_4 [!is.na(T_data_R14_4$R14_4),]
Quant <- subset (T_data_R14_4, T_data_R14_4$Quant == 1)
Qual <- subset (T_data_R14_4, T_data_R14_4$Quant == 0)
q_wtest_12 <- wilcox.test (Quant$R14_4, Qual$R14_4)
summary (Quant)
summary (Qual)


T_data_R15_4 <- dplyr::select(data, R15_4, Quant)
T_data_R15_4  <- T_data_R15_4 [!is.na(T_data_R15_4$R15_4),]
Quant <- subset (T_data_R15_4, T_data_R15_4$Quant == 1)
Qual <- subset (T_data_R15_4, T_data_R15_4$Quant == 0)
q_wtest_13 <- wilcox.test (Quant$R15_4, Qual$R15_4)
summary (Quant)
summary (Qual)


T_data_R16_4 <- dplyr::select(data, R16_4, Quant)
T_data_R16_4  <- T_data_R16_4 [!is.na(T_data_R16_4$R16_4),]
Quant <- subset (T_data_R16_4, T_data_R16_4$Quant == 1)
Qual <- subset (T_data_R16_4, T_data_R16_4$Quant == 0)
q_wtest_14 <- wilcox.test (Quant$R16_4, Qual$R16_4)
summary (Quant)
summary (Qual)


T_data_R17_4 <- dplyr::select(data, R17_4, Quant)
T_data_R17_4  <- T_data_R17_4 [!is.na(T_data_R17_4$R17_4),]
Quant <- subset (T_data_R17_4, T_data_R17_4$Quant == 1)
Qual <- subset (T_data_R17_4, T_data_R17_4$Quant == 0)
q_wtest_15 <- wilcox.test (Quant$R17_4, Qual$R17_4)
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





