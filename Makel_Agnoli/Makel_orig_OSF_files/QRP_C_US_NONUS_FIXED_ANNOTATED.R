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
#T_data_R3_4 <- dplyr::select(data, R3_4, US, Career): This section creates a subset dataset that
#contains the item of interest and variables. US and Career are included for both analyses covering
#career stage and location due to their similar coding structure.
#T_data_R3_4 <- T_data_R3_4 [!is.na(T_data_R3_4$R3_4),]: This section of the codes removes empty cells.
#wtest_1 <- wilcox.test (US$R3_4, Non_US$R3_4, alternative = "less"): This section of the code runs
#a wilcox test. 
#The summary() call provides the descriptive statistics for each group.

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#equiv is used across scripts. To call a specific equiv, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 


T_data_R3_4 <- dplyr::select(data, R3_4, US, Career)
T_data_R3_4  <- T_data_R3_4 [!is.na(T_data_R3_4$R3_4),]
US <- subset (T_data_R3_4, T_data_R3_4$US == 1)
Non_US <- subset (T_data_R3_4, T_data_R3_4$US == 0)
summary (US)
summary (Non_US)
wtest_1 <- wilcox.test (US$R3_4, Non_US$R3_4)

T_data_R4_4 <- dplyr::select(data, R4_4, US, Career)
T_data_R4_4  <- T_data_R4_4 [!is.na(T_data_R4_4$R4_4),]
US <- subset (T_data_R4_4, T_data_R4_4$US == 1)
Non_US <- subset (T_data_R4_4, T_data_R4_4$US == 0)
summary (US)
summary (Non_US)
wtest_2 <- wilcox.test (US$R4_4, Non_US$R4_4)

T_data_R5_4 <- dplyr::select(data, R5_4, US, Career)
T_data_R5_4  <- T_data_R5_4 [!is.na(T_data_R5_4$R5_4),]
US <- subset (T_data_R5_4, T_data_R5_4$US == 1)
Non_US <- subset (T_data_R5_4, T_data_R5_4$US == 0)
summary (US)
summary (Non_US)
wtest_3 <- wilcox.test (US$R5_4, Non_US$R5_4)

T_data_R6_4 <- dplyr::select(data, R6_4, US, Career)
T_data_R6_4  <- T_data_R6_4 [!is.na(T_data_R6_4$R6_4),]
US <- subset (T_data_R6_4, T_data_R6_4$US == 1)
Non_US <- subset (T_data_R6_4, T_data_R6_4$US == 0)
summary (US)
summary (Non_US)
wtest_4 <- wilcox.test (US$R6_4, Non_US$R6_4)

T_data_R7_4 <- dplyr::select(data, R7_4, US, Career)
T_data_R7_4  <- T_data_R7_4 [!is.na(T_data_R7_4$R7_4),]
US <- subset (T_data_R7_4, T_data_R7_4$US == 1)
Non_US <- subset (T_data_R7_4, T_data_R7_4$US == 0)
summary (US)
summary (Non_US)
wtest_5 <- wilcox.test (US$R7_4, Non_US$R7_4)

T_data_R8_4 <- dplyr::select(data, R8_4, US, Career)
T_data_R8_4  <- T_data_R8_4 [!is.na(T_data_R8_4$R8_4),]
US <- subset (T_data_R8_4, T_data_R8_4$US == 1)
Non_US <- subset (T_data_R8_4, T_data_R8_4$US == 0)
summary (US)
summary (Non_US)
wtest_6 <- wilcox.test (US$R8_4, Non_US$R8_4)

T_data_R9_4 <- dplyr::select(data, R9_4, US, Career)
T_data_R9_4  <- T_data_R9_4 [!is.na(T_data_R9_4$R9_4),]
US <- subset (T_data_R9_4, T_data_R9_4$US == 1)
Non_US <- subset (T_data_R9_4, T_data_R9_4$US == 0)
summary (US)
summary (Non_US)
wtest_7 <- wilcox.test (US$R9_4, Non_US$R9_4)

T_data_R10_4 <- dplyr::select(data, R10_4, US, Career)
T_data_R10_4  <- T_data_R10_4 [!is.na(T_data_R10_4$R10_4),]
US <- subset (T_data_R10_4, T_data_R10_4$US == 1)
Non_US <- subset (T_data_R10_4, T_data_R10_4$US == 0)
summary (US)
summary (Non_US)
wtest_8 <- wilcox.test (US$R10_4, Non_US$R10_4)

T_data_R11_4 <- dplyr::select(data, R11_4, US, Career)
T_data_R11_4  <- T_data_R11_4 [!is.na(T_data_R11_4$R11_4),]
US <- subset (T_data_R11_4, T_data_R11_4$US == 1)
Non_US <- subset (T_data_R11_4, T_data_R11_4$US == 0)
summary (US)
summary (Non_US)
wtest_9 <- wilcox.test (US$R11_4, Non_US$R11_4)

T_data_R12_4 <- dplyr::select(data, R12_4, US, Career)
T_data_R12_4  <- T_data_R12_4 [!is.na(T_data_R12_4$R12_4),]
US <- subset (T_data_R12_4, T_data_R12_4$US == 1)
Non_US <- subset (T_data_R12_4, T_data_R12_4$US == 0)
summary (US)
summary (Non_US)
wtest_10 <- wilcox.test (US$R12_4, Non_US$R12_4)

T_data_R13_4 <- dplyr::select(data, R13_4, US, Career)
T_data_R13_4  <- T_data_R13_4 [!is.na(T_data_R13_4$R13_4),]
US <- subset (T_data_R13_4, T_data_R13_4$US == 1)
Non_US <- subset (T_data_R13_4, T_data_R13_4$US == 0)
summary (US)
summary (Non_US)
wtest_11 <- wilcox.test (US$R13_4, Non_US$R13_4)

T_data_R14_4 <- dplyr::select(data, R14_4, US, Career)
T_data_R14_4  <- T_data_R14_4 [!is.na(T_data_R14_4$R14_4),]
US <- subset (T_data_R14_4, T_data_R14_4$US == 1)
Non_US <- subset (T_data_R14_4, T_data_R14_4$US == 0)
summary (US)
summary (Non_US)
wtest_12 <- wilcox.test (US$R14_4, Non_US$R14_4)

T_data_R15_4 <- dplyr::select(data, R15_4, US, Career)
T_data_R15_4  <- T_data_R15_4 [!is.na(T_data_R15_4$R15_4),]
US <- subset (T_data_R15_4, T_data_R15_4$US == 1)
Non_US <- subset (T_data_R15_4, T_data_R15_4$US == 0)
summary (US)
summary (Non_US)
wtest_13 <- wilcox.test (US$R15_4, Non_US$R15_4)

T_data_R16_4 <- dplyr::select(data, R16_4, US, Career)
T_data_R16_4  <- T_data_R16_4 [!is.na(T_data_R16_4$R16_4),]
US <- subset (T_data_R16_4, T_data_R16_4$US == 1)
Non_US <- subset (T_data_R16_4, T_data_R16_4$US == 0)
summary (US)
summary (Non_US)
wtest_14 <- wilcox.test (US$R16_4, Non_US$R16_4)

T_data_R17_4 <- dplyr::select(data, R17_4, US, Career)
T_data_R17_4  <- T_data_R17_4 [!is.na(T_data_R17_4$R17_4),]
US <- subset (T_data_R17_4, T_data_R17_4$US == 1)
Non_US <- subset (T_data_R17_4, T_data_R17_4$US == 0)
summary (US)
summary (Non_US)
wtest_15 <- wilcox.test (US$R17_4, Non_US$R17_4)


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





