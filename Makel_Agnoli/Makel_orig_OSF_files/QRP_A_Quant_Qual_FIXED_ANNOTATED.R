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



T_data_R3_2 <- dplyr::select(data, R3_2, Quant)
T_data_R3_2  <- T_data_R3_2 [!is.na(T_data_R3_2$R3_2),]
Quant <- subset (T_data_R3_2, T_data_R3_2$Quant == 1)
Qual <- subset (T_data_R3_2, T_data_R3_2$Quant == 0)
ttest_1 <- t.test (Quant$R3_2, Qual$R3_2)
summary (Quant)
summary (Qual)

T_data_R4_2 <- dplyr::select(data, R4_2, Quant)
T_data_R4_2  <- T_data_R4_2 [!is.na(T_data_R4_2$R4_2),]
Quant <- subset (T_data_R4_2, T_data_R4_2$Quant == 1)
Qual <- subset (T_data_R4_2, T_data_R4_2$Quant == 0)
ttest_2 <- t.test (Quant$R4_2, Qual$R4_2)
summary (Quant)
summary (Qual)

T_data_R5_2 <- dplyr::select(data, R5_2, Quant)
T_data_R5_2  <- T_data_R5_2 [!is.na(T_data_R5_2$R5_2),]
Quant <- subset (T_data_R5_2, T_data_R5_2$Quant == 1)
Qual <- subset (T_data_R5_2, T_data_R5_2$Quant == 0)
ttest_3 <- t.test (Quant$R5_2, Qual$R5_2)
summary (Quant)
summary (Qual)

T_data_R6_2 <- dplyr::select(data, R6_2, Quant)
T_data_R6_2  <- T_data_R6_2 [!is.na(T_data_R6_2$R6_2),]
Quant <- subset (T_data_R6_2, T_data_R6_2$Quant == 1)
Qual <- subset (T_data_R6_2, T_data_R6_2$Quant == 0)
ttest_4 <- t.test (Quant$R6_2, Qual$R6_2)
summary (Quant)
summary (Qual)


T_data_R7_2 <- dplyr::select(data, R7_2, Quant)
T_data_R7_2  <- T_data_R7_2 [!is.na(T_data_R7_2$R7_2),]
Quant <- subset (T_data_R7_2, T_data_R7_2$Quant == 1)
Qual <- subset (T_data_R7_2, T_data_R7_2$Quant == 0)
ttest_5 <- t.test (Quant$R7_2, Qual$R7_2)
summary (Quant)
summary (Qual)

T_data_R8_2 <- dplyr::select(data, R8_2, Quant)
T_data_R8_2  <- T_data_R8_2 [!is.na(T_data_R8_2$R8_2),]
Quant <- subset (T_data_R8_2, T_data_R8_2$Quant == 1)
Qual <- subset (T_data_R8_2, T_data_R8_2$Quant == 0)
ttest_6 <- t.test (Quant$R8_2, Qual$R8_2)
summary (Quant)
summary (Qual)


T_data_R9_2 <- dplyr::select(data, R9_2, Quant)
T_data_R9_2  <- T_data_R9_2 [!is.na(T_data_R9_2$R9_2),]
Quant <- subset (T_data_R9_2, T_data_R9_2$Quant == 1)
Qual <- subset (T_data_R9_2, T_data_R9_2$Quant == 0)
ttest_7 <- t.test (Quant$R9_2, Qual$R9_2)
summary (Quant)
summary (Qual)


T_data_R10_2 <- dplyr::select(data, R10_2, Quant)
T_data_R10_2  <- T_data_R10_2 [!is.na(T_data_R10_2$R10_2),]
Quant <- subset (T_data_R10_2, T_data_R10_2$Quant == 1)
Qual <- subset (T_data_R10_2, T_data_R10_2$Quant == 0)
ttest_8 <- t.test (Quant$R10_2, Qual$R10_2)
summary (Quant)
summary (Qual)


T_data_R11_2 <- dplyr::select(data, R11_2, Quant)
T_data_R11_2  <- T_data_R11_2 [!is.na(T_data_R11_2$R11_2),]
Quant <- subset (T_data_R11_2, T_data_R11_2$Quant == 1)
Qual <- subset (T_data_R11_2, T_data_R11_2$Quant == 0)
ttest_9 <- t.test (Quant$R11_2, Qual$R11_2)
summary (Quant)
summary (Qual)

T_data_R12_2 <- dplyr::select(data, R12_2, Quant)
T_data_R12_2  <- T_data_R12_2 [!is.na(T_data_R12_2$R12_2),]
Quant <- subset (T_data_R12_2, T_data_R12_2$Quant == 1)
Qual <- subset (T_data_R12_2, T_data_R12_2$Quant == 0)
ttest_10 <- t.test (Quant$R12_2, Qual$R12_2)
summary (Quant)
summary (Qual)

T_data_R13_2 <- dplyr::select(data, R13_2, Quant)
T_data_R13_2  <- T_data_R13_2 [!is.na(T_data_R13_2$R13_2),]
Quant <- subset (T_data_R13_2, T_data_R13_2$Quant == 1)
Qual <- subset (T_data_R13_2, T_data_R13_2$Quant == 0)
ttest_11 <- t.test (Quant$R13_2, Qual$R13_2)
summary (Quant)
summary (Qual)

T_data_R14_2 <- dplyr::select(data, R14_2, Quant)
T_data_R14_2  <- T_data_R14_2 [!is.na(T_data_R14_2$R14_2),]
Quant <- subset (T_data_R14_2, T_data_R14_2$Quant == 1)
Qual <- subset (T_data_R14_2, T_data_R14_2$Quant == 0)
ttest_12 <- t.test (Quant$R14_2, Qual$R14_2)
summary (Quant)
summary (Qual)


T_data_R15_2 <- dplyr::select(data, R15_2, Quant)
T_data_R15_2  <- T_data_R15_2 [!is.na(T_data_R15_2$R15_2),]
Quant <- subset (T_data_R15_2, T_data_R15_2$Quant == 1)
Qual <- subset (T_data_R15_2, T_data_R15_2$Quant == 0)
ttest_13 <- t.test (Quant$R15_2, Qual$R15_2)
summary (Quant)
summary (Qual)


T_data_R16_2 <- dplyr::select(data, R16_2, Quant)
T_data_R16_2  <- T_data_R16_2 [!is.na(T_data_R16_2$R16_2),]
Quant <- subset (T_data_R16_2, T_data_R16_2$Quant == 1)
Qual <- subset (T_data_R16_2, T_data_R16_2$Quant == 0)
ttest_14 <- t.test (Quant$R16_2, Qual$R16_2)
summary (Quant)
summary (Qual)


T_data_R17_2 <- dplyr::select(data, R17_2, Quant)
T_data_R17_2  <- T_data_R17_2 [!is.na(T_data_R17_2$R17_2),]
Quant <- subset (T_data_R17_2, T_data_R17_2$Quant == 1)
Qual <- subset (T_data_R17_2, T_data_R17_2$Quant == 0)
ttest_15 <- t.test (Quant$R17_2, Qual$R17_2)
summary (Quant)
summary (Qual)

#These are calls and will produce the T test output by item. This output will
#also produce means. 

ttest_1
ttest_2
ttest_3
ttest_4
ttest_5
ttest_6
ttest_7
ttest_8
ttest_9
ttest_10
ttest_11
ttest_12
ttest_13
ttest_14
ttest_15





