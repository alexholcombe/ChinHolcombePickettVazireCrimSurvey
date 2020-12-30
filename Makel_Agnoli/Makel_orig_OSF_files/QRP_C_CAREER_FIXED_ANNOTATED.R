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
#T_data_R3_4 <- dplyr::select(data, R3_4, US, Career): This section creates a subset dataset that
#contains the item of interest and variables. US and Career are included for both analyses covering
#career stage and location due to their similar coding structure.
#T_data_R3_4  <- T_data_R3_4 [!is.na(T_data_R3_4$R3_4),]: This section of the codes removes empty cells.
#ktest_1 <- kruskal.test (R3_4~Career, data = T_data_R3_4): This section of the code runs a kruskal-wallis test
#kmctest_1 <- kruskalmc (R3_4~Career, data = T_data_R3_4): This section of the code runs a kruskal multiple comparisons test.
#This is code is unique to the career stages script as other comparions were between two groups. 

#Notes on code- 
#Object names were selected for clarity. The sequential nature is the same across scripts. That stated, the naming convention
#ktest is used across scripts. To call a specific ktest, the associated script must be rerun.
#for loops were specifically not coded in order to allow for replicability of a single item or subset of items. 
#That said, a for loop can be created to streamline the code presented here. 

data <- read.csv ("Full_Responses.csv")
T_data_R3_4 <- dplyr::select(data, R3_4, US, Career)
T_data_R3_4  <- T_data_R3_4 [!is.na(T_data_R3_4$R3_4),]
ktest_1 <- kruskal.test (R3_4~Career, data = T_data_R3_4)
kmctest_1 <- kruskalmc (R3_4~Career, data = T_data_R3_4)

T_data_R4_4 <- dplyr::select(data, R4_4, US, Career)
T_data_R4_4  <- T_data_R4_4 [!is.na(T_data_R4_4$R4_4),]
ktest_2 <- kruskal.test (R4_4~Career, data = T_data_R4_4)
kmctest_2 <- kruskalmc (R4_4~Career, data = T_data_R4_4)

T_data_R5_4 <- dplyr::select(data, R5_4, US, Career)
T_data_R5_4  <- T_data_R5_4 [!is.na(T_data_R5_4$R5_4),]
ktest_3 <- kruskal.test (R5_4~Career, data = T_data_R5_4)
kmctest_3 <- kruskalmc (R5_4~Career, data = T_data_R5_4)

T_data_R6_4 <- dplyr::select(data, R6_4, US, Career)
T_data_R6_4  <- T_data_R6_4 [!is.na(T_data_R6_4$R6_4),]
ktest_4 <- kruskal.test (R6_4~Career, data = T_data_R6_4)
kmctest_4 <- kruskalmc (R6_4~Career, data = T_data_R6_4)

T_data_R7_4 <- dplyr::select(data, R7_4, US, Career)
T_data_R7_4  <- T_data_R7_4 [!is.na(T_data_R7_4$R7_4),]
ktest_5 <- kruskal.test (R7_4~Career, data = T_data_R7_4)
kmctest_5 <- kruskalmc (R7_4~Career, data = T_data_R7_4)

T_data_R8_4 <- dplyr::select(data, R8_4, US, Career)
T_data_R8_4  <- T_data_R8_4 [!is.na(T_data_R8_4$R8_4),]
ktest_6 <- kruskal.test (R8_4~Career, data = T_data_R8_4)
kmctest_6 <- kruskalmc (R8_4~Career, data = T_data_R8_4)

T_data_R9_4 <- dplyr::select(data, R9_4, US, Career)
T_data_R9_4  <- T_data_R9_4 [!is.na(T_data_R9_4$R9_4),]
ktest_7 <- kruskal.test (R9_4~Career, data = T_data_R9_4)
kmctest_7 <- kruskalmc (R9_4~Career, data = T_data_R9_4)

T_data_R10_4 <- dplyr::select(data, R10_4, US, Career)
T_data_R10_4  <- T_data_R10_4 [!is.na(T_data_R10_4$R10_4),]
ktest_8 <- kruskal.test (R10_4~Career, data = T_data_R10_4)
kmctest_8 <- kruskalmc (R10_4~Career, data = T_data_R10_4)

T_data_R11_4 <- dplyr::select(data, R11_4, US, Career)
T_data_R11_4  <- T_data_R11_4 [!is.na(T_data_R11_4$R11_4),]
ktest_9 <- kruskal.test (R11_4~Career, data = T_data_R11_4)
kmctest_9 <- kruskalmc (R11_4~Career, data = T_data_R11_4)

T_data_R12_4 <- dplyr::select(data, R12_4, US, Career)
T_data_R12_4  <- T_data_R12_4 [!is.na(T_data_R12_4$R12_4),]
ktest_10 <- kruskal.test (R12_4~Career, data = T_data_R12_4)
kmctest_10 <- kruskalmc (R12_4~Career, data = T_data_R12_4)

T_data_R13_4 <- dplyr::select(data, R13_4, US, Career)
T_data_R13_4  <- T_data_R13_4 [!is.na(T_data_R13_4$R13_4),]
ktest_11 <- kruskal.test (R13_4~Career, data = T_data_R13_4)
kmctest_11 <- kruskalmc (R13_4~Career, data = T_data_R13_4)

T_data_R14_4 <- dplyr::select(data, R14_4, US, Career)
T_data_R14_4  <- T_data_R14_4 [!is.na(T_data_R14_4$R14_4),]
ktest_12 <- kruskal.test (R14_4~Career, data = T_data_R14_4)
kmctest_12 <- kruskalmc (R14_4~Career, data = T_data_R14_4)

T_data_R15_4 <- dplyr::select(data, R15_4, US, Career)
T_data_R15_4  <- T_data_R15_4 [!is.na(T_data_R15_4$R15_4),]
ktest_13 <- kruskal.test (R15_4~Career, data = T_data_R15_4)
kmctest_13 <- kruskalmc (R15_4~Career, data = T_data_R15_4)

T_data_R16_4 <- dplyr::select(data, R16_4, US, Career)
T_data_R16_4  <- T_data_R16_4 [!is.na(T_data_R16_4$R16_4),]
ktest_14 <- kruskal.test (R16_4~Career, data = T_data_R16_4)
kmctest_14 <- kruskalmc (R16_4~Career, data = T_data_R16_4)

T_data_R17_4 <- dplyr::select(data, R17_4, US, Career)
T_data_R17_4  <- T_data_R17_4 [!is.na(T_data_R17_4$R17_4),]
ktest_15 <- kruskal.test (R17_4~Career, data = T_data_R17_4)
kmctest_15 <- kruskalmc (R17_4~Career, data = T_data_R17_4)


#This are calls and will produe all output by item. The ktest refers to the kruskal-wallis test. 
#The kmctest refers to kruskal multiple comparison tests. 

ktest_1 
kmctest_1
ktest_2 
kmctest_2
ktest_3
kmctest_3
ktest_4
kmctest_4
ktest_5
kmctest_5
ktest_6
kmctest_6
ktest_7
kmctest_7
ktest_8
kmctest_8
ktest_9
kmctest_9
ktest_10
kmctest_10
ktest_11
kmctest_11
ktest_12
kmctest_12
ktest_13
kmctest_13
ktest_14
kmctest_14
ktest_15
kmctest_15


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

R3 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_4 = n(),
    mean_4 = sprintf("%0.3f",mean(R3_4, na.rm = TRUE)),
    sd_4 = sd(R3_4, na.rm = TRUE)
  )
)

R4 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_4 = n(),
    mean_4 = sprintf("%0.3f",mean(R4_4, na.rm = TRUE)),
    sd_4 = sd(R4_4, na.rm = TRUE)
  )
)

R5 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_5 = n(),
    mean_5 = sprintf("%0.3f",mean(R5_4, na.rm = TRUE)),
    sd_5 = sd(R5_4, na.rm = TRUE)
  )
)

R6 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_6 = n(),
    mean_6 = sprintf("%0.3f",mean(R6_4, na.rm = TRUE)),
    sd_6 = sd(R6_4, na.rm = TRUE)
  )
)

R7 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_7 = n(),
    mean_7 = sprintf("%0.3f",mean(R7_4, na.rm = TRUE)),
    sd_7 = sd(R7_4, na.rm = TRUE)
  )
)

R8 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_8 = n(),
    mean_8 = sprintf("%0.3f",mean(R8_4, na.rm = TRUE)),
    sd_8 = sd(R8_4, na.rm = TRUE)
  )
)

R9 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_9 = n(),
    mean_9 = sprintf("%0.3f",mean(R9_4, na.rm = TRUE)),
    sd_9 = sd(R9_4, na.rm = TRUE)
  )
)

R10 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_10 = n(),
    mean_10 = sprintf("%0.3f",mean(R10_4, na.rm = TRUE)),
    sd_10 = sd(R10_4, na.rm = TRUE)
  )
)

R11 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_11 = n(),
    mean_11 = sprintf("%0.3f", mean(R11_4, na.rm = TRUE)),
    sd_11 = sd(R11_4, na.rm = TRUE)
  )
)

R12 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_12 = n(),
    mean_12 = sprintf("%0.3f",mean(R12_4, na.rm = TRUE)),
    sd_12 = sd(R12_4, na.rm = TRUE)
  )
)

R13 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_13 = n(),
    mean_13 = sprintf("%0.3f",mean(R13_4, na.rm = TRUE)),
    sd_13 = sd(R13_4, na.rm = TRUE)
  )
)

R14 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_14 = n(),
    mean_14 = sprintf("%0.3f", mean(R14_4, na.rm = TRUE)),
    sd_14 = sd(R14_4, na.rm = TRUE)
  )
)

R15 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_15 = n(),
    mean_15 = sprintf("%0.3f",mean(R15_4, na.rm = TRUE)),
    sd_15 = sd(R15_4, na.rm = TRUE)
  )
)

R16 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_16 = n(),
    mean_16 = sprintf("%0.3f",mean(R16_4, na.rm = TRUE)),
    sd_16 = sd(R16_4, na.rm = TRUE)
  )
)

R17 <- 
as.data.frame (
dplyr::group_by(data, Career) %>%
  dplyr::summarise(
    count_17 = n(),
    mean_17 = sprintf("%0.3f", mean(R17_4, na.rm = TRUE)),
    sd_17 = sd(R17_4, na.rm = TRUE)
  )
)

#This are calls and will produe all output by item. 

R3
R4
R5
R6
R7
R8
R9
R10
R11
R12
R13
R14
R15
R16
R17





