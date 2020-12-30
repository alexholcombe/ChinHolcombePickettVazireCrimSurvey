#The only package used here is the "equivalence" package.

data <- read.csv ("Full_Responses.csv")
library ("equivalence")

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
#T_data_R3_2  <- T_data_R3_2 [!is.na(T_data_R3_2$R3_2),]: This section of the codes removes empty cells.
#equiv_1 <- tost(US$R3_2,Non_US$R3_2, epsilon = 1, paired = FALSE): This section of the code runs
#an equivelance test. 

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#equiv is used across scripts. To call a specific equiv, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 


T_data_R3_2 <- dplyr::select(data, R3_2, US, Career)
T_data_R3_2  <- T_data_R3_2 [!is.na(T_data_R3_2$R3_2),]
US <- subset (T_data_R3_2, T_data_R3_2$US == 1)
Non_US <- subset (T_data_R3_2, T_data_R3_2$US == 0)
equiv_1 <- tost(US$R3_2,Non_US$R3_2, epsilon = 1, paired = FALSE)

T_data_R4_2 <- dplyr::select(data, R4_2, US, Career)
T_data_R4_2  <- T_data_R4_2 [!is.na(T_data_R4_2$R4_2),]
US <- subset (T_data_R4_2, T_data_R4_2$US == 1)
Non_US <- subset (T_data_R4_2, T_data_R4_2$US == 0)
equiv_2 <- tost(US$R4_2,Non_US$R4_2, epsilon = 1, paired = FALSE)

T_data_R5_2 <- dplyr::select(data, R5_2, US, Career)
T_data_R5_2  <- T_data_R5_2 [!is.na(T_data_R5_2$R5_2),]
US <- subset (T_data_R5_2, T_data_R5_2$US == 1)
Non_US <- subset (T_data_R5_2, T_data_R5_2$US == 0)
equiv_3 <- tost(US$R5_2,Non_US$R5_2, epsilon = 1, paired = FALSE)

T_data_R6_2 <- dplyr::select(data, R6_2, US, Career)
T_data_R6_2  <- T_data_R6_2 [!is.na(T_data_R6_2$R6_2),]
US <- subset (T_data_R6_2, T_data_R6_2$US == 1)
Non_US <- subset (T_data_R6_2, T_data_R6_2$US == 0)
equiv_4 <- tost(US$R6_2,Non_US$R6_2, epsilon = 1, paired = FALSE)

T_data_R7_2 <- dplyr::select(data, R7_2, US, Career)
T_data_R7_2  <- T_data_R7_2 [!is.na(T_data_R7_2$R7_2),]
US <- subset (T_data_R7_2, T_data_R7_2$US == 1)
Non_US <- subset (T_data_R7_2, T_data_R7_2$US == 0)
equiv_5 <- tost(US$R7_2,Non_US$R7_2, epsilon = 1, paired = FALSE)

T_data_R8_2 <- dplyr::select(data, R8_2, US, Career)
T_data_R8_2  <- T_data_R8_2 [!is.na(T_data_R8_2$R8_2),]
US <- subset (T_data_R8_2, T_data_R8_2$US == 1)
Non_US <- subset (T_data_R8_2, T_data_R8_2$US == 0)
equiv_6 <- tost(US$R8_2,Non_US$R8_2, epsilon = 1, paired = FALSE)

T_data_R9_2 <- dplyr::select(data, R9_2, US, Career)
T_data_R9_2  <- T_data_R9_2 [!is.na(T_data_R9_2$R9_2),]
US <- subset (T_data_R9_2, T_data_R9_2$US == 1)
Non_US <- subset (T_data_R9_2, T_data_R9_2$US == 0)
equiv_7 <- tost(US$R9_2,Non_US$R9_2, epsilon = 1, paired = FALSE)

T_data_R10_2 <- dplyr::select(data, R10_2, US, Career)
T_data_R10_2  <- T_data_R10_2 [!is.na(T_data_R10_2$R10_2),]
US <- subset (T_data_R10_2, T_data_R10_2$US == 1)
Non_US <- subset (T_data_R10_2, T_data_R10_2$US == 0)
equiv_8 <- tost(US$R10_2,Non_US$R10_2, epsilon = 1, paired = FALSE)

T_data_R11_2 <- dplyr::select(data, R11_2, US, Career)
T_data_R11_2  <- T_data_R11_2 [!is.na(T_data_R11_2$R11_2),]
US <- subset (T_data_R11_2, T_data_R11_2$US == 1)
Non_US <- subset (T_data_R11_2, T_data_R11_2$US == 0)
equiv_9 <- tost(US$R11_2,Non_US$R11_2, epsilon = 1, paired = FALSE)

T_data_R12_2 <- dplyr::select(data, R12_2, US, Career)
T_data_R12_2  <- T_data_R12_2 [!is.na(T_data_R12_2$R12_2),]
US <- subset (T_data_R12_2, T_data_R12_2$US == 1)
Non_US <- subset (T_data_R12_2, T_data_R12_2$US == 0)
equiv_10 <- tost(US$R12_2,Non_US$R12_2, epsilon = 1, paired = FALSE)

T_data_R13_2 <- dplyr::select(data, R13_2, US, Career)
T_data_R13_2  <- T_data_R13_2 [!is.na(T_data_R13_2$R13_2),]
US <- subset (T_data_R13_2, T_data_R13_2$US == 1)
Non_US <- subset (T_data_R13_2, T_data_R13_2$US == 0)
ttest_1 <- t.test (US$R13_2, Non_US$R13_2, alternative = "greater")

T_data_R14_2 <- dplyr::select(data, R14_2, US, Career)
T_data_R14_2  <- T_data_R14_2 [!is.na(T_data_R14_2$R14_2),]
US <- subset (T_data_R14_2, T_data_R14_2$US == 1)
Non_US <- subset (T_data_R14_2, T_data_R14_2$US == 0)
ttest_2 <- t.test (US$R14_2, Non_US$R14_2, alternative = "greater")

T_data_R15_2 <- dplyr::select(data, R15_2, US, Career)
T_data_R15_2  <- T_data_R15_2 [!is.na(T_data_R15_2$R15_2),]
US <- subset (T_data_R15_2, T_data_R15_2$US == 1)
Non_US <- subset (T_data_R15_2, T_data_R15_2$US == 0)
ttest_3 <- t.test (US$R15_2, Non_US$R15_2, alternative = "greater")

T_data_R16_2 <- dplyr::select(data, R16_2, US, Career)
T_data_R16_2  <- T_data_R16_2 [!is.na(T_data_R16_2$R16_2),]
US <- subset (T_data_R16_2, T_data_R16_2$US == 1)
Non_US <- subset (T_data_R16_2, T_data_R16_2$US == 0)
ttest_4 <- t.test (US$R16_2, Non_US$R16_2, alternative = "greater")

T_data_R17_2 <- dplyr::select(data, R17_2, US, Career)
T_data_R17_2  <- T_data_R17_2 [!is.na(T_data_R17_2$R17_2),]
US <- subset (T_data_R17_2, T_data_R17_2$US == 1)
Non_US <- subset (T_data_R17_2, T_data_R17_2$US == 0)
ttest_5 <- t.test (US$R17_2, Non_US$R17_2, alternative = "greater")
summary (US)
summary (Non_US)

#These are calls and will produce the equivelance test output by item. This output will
#also produce means. 

equiv_1
equiv_2
equiv_3
equiv_4
equiv_5
equiv_6
equiv_7
equiv_8
equiv_9
equiv_10
ttest_1
ttest_2
ttest_3
ttest_4
ttest_5





