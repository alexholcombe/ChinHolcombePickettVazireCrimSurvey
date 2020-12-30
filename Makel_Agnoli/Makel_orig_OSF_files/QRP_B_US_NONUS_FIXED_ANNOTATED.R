data <- read.csv ("Full_Responses.csv")



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
#T_data_R3_3 <- dplyr::select(data, R3_3, US, Career): This section creates a subset dataset that
#contains the item of interest and variables. US and Career are included for both analyses covering
#career stage and location due to their similar coding structure.
#T_data_R3_3 <- T_data_R3_3 [!is.na(T_data_R3_3$R3_3),]: This section of the codes removes empty cells.
#wtest_1 <- wilcox.test (US$R3_3, Non_US$R3_3, alternative = "less"): This section of the code runs
#a wilcox test. 
#The summary() call provides the descriptive statistics for each group.

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#equiv is used across scripts. To call a specific equiv, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 



T_data_R3_3 <- dplyr::select(data, R3_3, US, Career)
T_data_R3_3  <- T_data_R3_3 [!is.na(T_data_R3_3$R3_3),]
US <- subset (T_data_R3_3, T_data_R3_3$US == 1)
Non_US <- subset (T_data_R3_3, T_data_R3_3$US == 0)
wtest_1 <- wilcox.test (US$R3_3, Non_US$R3_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R4_3 <- dplyr::select(data, R4_3, US, Career)
T_data_R4_3  <- T_data_R4_3 [!is.na(T_data_R4_3$R4_3),]
US <- subset (T_data_R4_3, T_data_R4_3$US == 1)
Non_US <- subset (T_data_R4_3, T_data_R4_3$US == 0)
wtest_2 <- wilcox.test (US$R4_3, Non_US$R4_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R5_3 <- dplyr::select(data, R5_3, US, Career)
T_data_R5_3  <- T_data_R5_3 [!is.na(T_data_R5_3$R5_3),]
US <- subset (T_data_R5_3, T_data_R5_3$US == 1)
Non_US <- subset (T_data_R5_3, T_data_R5_3$US == 0)
wtest_3 <- wilcox.test (US$R5_3, Non_US$R5_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R6_3 <- dplyr::select(data, R6_3, US, Career)
T_data_R6_3  <- T_data_R6_3 [!is.na(T_data_R6_3$R6_3),]
US <- subset (T_data_R6_3, T_data_R6_3$US == 1)
Non_US <- subset (T_data_R6_3, T_data_R6_3$US == 0)
wtest_4 <- wilcox.test (US$R6_3, Non_US$R6_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R7_3 <- dplyr::select(data, R7_3, US, Career)
T_data_R7_3  <- T_data_R7_3 [!is.na(T_data_R7_3$R7_3),]
US <- subset (T_data_R7_3, T_data_R7_3$US == 1)
Non_US <- subset (T_data_R7_3, T_data_R7_3$US == 0)
wtest_5 <- wilcox.test (US$R7_3, Non_US$R7_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R8_3 <- dplyr::select(data, R8_3, US, Career)
T_data_R8_3  <- T_data_R8_3 [!is.na(T_data_R8_3$R8_3),]
US <- subset (T_data_R8_3, T_data_R8_3$US == 1)
Non_US <- subset (T_data_R8_3, T_data_R8_3$US == 0)
wtest_6 <- wilcox.test (US$R8_3, Non_US$R8_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R9_3 <- dplyr::select(data, R9_3, US, Career)
T_data_R9_3  <- T_data_R9_3 [!is.na(T_data_R9_3$R9_3),]
US <- subset (T_data_R9_3, T_data_R9_3$US == 1)
Non_US <- subset (T_data_R9_3, T_data_R9_3$US == 0)
wtest_7 <- wilcox.test (US$R9_3, Non_US$R9_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R10_3 <- dplyr::select(data, R10_3, US, Career)
T_data_R10_3  <- T_data_R10_3 [!is.na(T_data_R10_3$R10_3),]
US <- subset (T_data_R10_3, T_data_R10_3$US == 1)
Non_US <- subset (T_data_R10_3, T_data_R10_3$US == 0)
wtest_8 <- wilcox.test (US$R10_3, Non_US$R10_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R11_3 <- dplyr::select(data, R11_3, US, Career)
T_data_R11_3  <- T_data_R11_3 [!is.na(T_data_R11_3$R11_3),]
US <- subset (T_data_R11_3, T_data_R11_3$US == 1)
Non_US <- subset (T_data_R11_3, T_data_R11_3$US == 0)
wtest_9 <- wilcox.test (US$R11_3, Non_US$R11_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R12_3 <- dplyr::select(data, R12_3, US, Career)
T_data_R12_3  <- T_data_R12_3 [!is.na(T_data_R12_3$R12_3),]
US <- subset (T_data_R12_3, T_data_R12_3$US == 1)
Non_US <- subset (T_data_R12_3, T_data_R12_3$US == 0)
wtest_10 <- wilcox.test (US$R12_3, Non_US$R12_3, alternative = "less")
summary (US)
summary (Non_US)

T_data_R13_3 <- dplyr::select(data, R13_3, US, Career)
T_data_R13_3  <- T_data_R13_3 [!is.na(T_data_R13_3$R13_3),]
US <- subset (T_data_R13_3, T_data_R13_3$US == 1)
Non_US <- subset (T_data_R13_3, T_data_R13_3$US == 0)
wtest_11 <- wilcox.test (US$R13_3, Non_US$R13_3, alternative = "greater")
summary (US)
summary (Non_US)

T_data_R14_3 <- dplyr::select(data, R14_3, US, Career)
T_data_R14_3  <- T_data_R14_3 [!is.na(T_data_R14_3$R14_3),]
US <- subset (T_data_R14_3, T_data_R14_3$US == 1)
Non_US <- subset (T_data_R14_3, T_data_R14_3$US == 0)
wtest_12 <- wilcox.test (US$R14_3, Non_US$R14_3, alternative = "greater")
summary (US)
summary (Non_US)

T_data_R15_3 <- dplyr::select(data, R15_3, US, Career)
T_data_R15_3  <- T_data_R15_3 [!is.na(T_data_R15_3$R15_3),]
US <- subset (T_data_R15_3, T_data_R15_3$US == 1)
Non_US <- subset (T_data_R15_3, T_data_R15_3$US == 0)
wtest_13 <- wilcox.test (US$R15_3, Non_US$R15_3, alternative = "greater")
summary (US)
summary (Non_US)

T_data_R16_3 <- dplyr::select(data, R16_3, US, Career)
T_data_R16_3  <- T_data_R16_3 [!is.na(T_data_R16_3$R16_3),]
US <- subset (T_data_R16_3, T_data_R16_3$US == 1)
Non_US <- subset (T_data_R16_3, T_data_R16_3$US == 0)
wtest_14 <- wilcox.test (US$R16_3, Non_US$R16_3, alternative = "greater")
summary (US)
summary (Non_US)

T_data_R17_3 <- dplyr::select(data, R17_3, US, Career)
T_data_R17_3  <- T_data_R17_3 [!is.na(T_data_R17_3$R17_3),]
US <- subset (T_data_R17_3, T_data_R17_3$US == 1)
Non_US <- subset (T_data_R17_3, T_data_R17_3$US == 0)
wtest_15 <- wilcox.test (US$R17_3, Non_US$R17_3, alternative = "greater")
summary (US)
summary (Non_US)

#These are calls and will produce the wilcox test output by item. This output will
#also produce means. 


wtest_1
wtest_2
wtest_3
wtest_4
wtest_5
wtest_6
wtest_7
wtest_8
wtest_9
wtest_10
wtest_11
wtest_12
wtest_13
wtest_14
wtest_15





