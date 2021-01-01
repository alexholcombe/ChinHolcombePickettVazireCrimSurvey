### Info ====
## Code for "Questionable research practices among Brazilian psychological
## researchers: Results from a replication study"

## by Andre L. A. Rabelo & Mauricio Sarmet (15/09/2017)

### A priori power analysis ====
## Power analysis for correlation
# Install and load packages
install.packages("pwr")
library(pwr)

# Main command
pwr.p.test(h = .2, sig.level =.05, power =.8, alternative="two.sided")

### Read data ====
setwd("INSERT PATH TO WORKING DIRECTORY HERE") # set working directory

## Uploading raw data frame (make sure the file is on the working directory)
df_br <- read.csv("BR_raw_data.csv", header=TRUE, row.names = 1, sep=";")

### Data cleaning ====
## Recode -77, -66, and -99 as NA
df_br[df_br == -77] <- NA # assign value -77 as NA
df_br[df_br == -66] <- NA # assign value -66 as NA
df_br[df_br == -99] <- NA # assign value -99 as NA

## Include ID column
df_br$ID <- c(1:649)

## Reorder data frame so that ID is the first one
df_br <- df_br[,c(185,1:184)]

## Exclude useless columns
df_br <- df_br[ ,-c(2:8, 102:185)]

# Delete two weird cases in age ("3-" and "xx")
df_br <- df_br[order(df_br$age),]
df_br <- edit(df_br)

### Headers ====
## Check headers names
names(df_br) 

# Code explanation for changing headers:
# AX_1 = % Pesquisadores que se engajam na pratica
# AX_2 = % Pesquisadores que admitem a pratica
# B = Voce ja se engajou na pratica
# CX_1 = Considera a pratica justificavel?
# CX_2 = Se sim ou possivelmente, explique as razoes
#CX_3 = Se nao, explique as razoes

colnames(df_br) <- c("ID", "A1_1", "A1_2", "B1", "C1_1",
  "C1_2", "C1_3", "A2_1", "A2_2", "B2", "C2_1",
  "C2_2", "C2_3", "A3_1", "A3_2", "B3", "C3_1", 
  "C3_2", "C3_3", "A4_1", "A4_2", "B4", "C4_1",
  "C4_2", "C4_3", "A5_1", "A5_2", "B5", "C5_1",
  "C5_2", "C5_3", "A6_1", "A6_2", "B6", "C6_1",
  "C6_2", "C6_3", "A7_1", "A7_2", "B7", "C7_1",
  "C7_2", "C7_3", "A8_1", "A8_2", "B8", "C8_1",
  "C8_2", "C8_3", "A9_1", "A9_2", "B9", "C9_1",
  "C9_2", "C9_3", "A10_1", "A10_2", "B10", "C10_1",
  "C10_2", "C10_3", "doubt_other_inst",
  "doubt_same_inst", "doubt_students", "doubt_yourself",
  "main_area", "area_other", "do_research", "funding",
  "funding_5_years", "observation", "experiment",
  "modelling", "survey", "method_other",
  "method_other_descript", "clinic", "lab", "field", 
  "location_other","location_other_descript", "institution_priority", 
  "institution_type", "position", "position_other", 
  "higher_degree", "sex", "skin_colour",
  "age", "brazilian", "other_nation", "other_nation_descript",
  "country_higher_degree", "suggestions")

### Factors and labels ====
## Running all the following commands in this section
## will create proper factors and labels
df_br$doubt_other_inst <- factor(df_br$doubt_other_inst,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "Once or twice", "Occasionally", "Often"))

df_br$doubt_same_inst <- factor(df_br$doubt_same_inst,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "Once or twice", "Occasionally", "Often"))

df_br$doubt_students <- factor(df_br$doubt_students,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "Once or twice", "Occasionally", "Often"))

df_br$doubt_yourself <- factor(df_br$doubt_yourself,
  levels = c(1, 2, 3, 4),
  labels = c("Never", "Once or twice", "Occasionally", "Often"))

df_br$main_area <- factor(df_br$main_area,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
    14, 15, 16, 17),
  labels = c("Behavior Analysis", "Environmental",
    "Psychological Assessment", "Clinical",
    "Cognitive", "Development",
    "School and Education", "Evolutionary",
    "Family and Community", 
    "Legal, Forensic, and Criminal",
    "Neuropsychology", "Organizational and Work",
    "Health", "Mental Health", "Social",
    "Traffic", "other"))

df_br$do_research <- factor(df_br$do_research,
  levels = c(1, 2),
  labels = c("Yes", "No"))

df_br$funding <- factor(df_br$funding,
  levels = c(1, 2),
  labels = c("Yes", "No"))

df_br$funding_5_years <- factor(df_br$funding_5_years,
  levels = c(1, 2),
  labels = c("Yes", "No"))

df_br$observation <- factor(df_br$observation,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$experiment <- factor(df_br$experiment,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$modelling <- factor(df_br$modelling,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$survey <- factor(df_br$survey,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$clinic <- factor(df_br$clinic,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$lab <- factor(df_br$lab,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$field <- factor(df_br$field,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$location_other<- factor(df_br$location_other,
  levels = c(1, 0),
  labels = c("Yes", "No"))

df_br$institution_priority<- factor(df_br$institution_priority,
  levels = c(1, 2, 3),
  labels = c("Teaching", "Research", "Equal emphasis"))

df_br$institution_type<- factor(df_br$institution_type,
  levels = c(1, 2),
  labels = c("Public", "Private"))

df_br$position<- factor(df_br$position,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8),
  labels = c("Graduate student", "Postdoc",
    "Substitute professor", "Assistant professor",
    "Adjunct professor", "Associate professor",
    "Emeritus professor", "other"))

df_br$higher_degree<- factor(df_br$higher_degree,
  levels = c(1, 2, 3),
  labels = c("Undergraduate", "Masters", "Doctorate"))

df_br$sex <- factor(df_br$sex,
  levels = c(1, 2),
  labels = c("Male", "Female"))

df_br$skin_colour <- factor(df_br$skin_colour,
  levels = c(1,2,3,4,5),
  labels = c("White", "Black", "Yellow",
    "Parda", "Indigenous"))

df_br$brazilian<- factor(df_br$brazilian,
  levels = c(1,0),
  labels = c("Yes", "No"))

df_br$other_nation<- factor(df_br$other_nation,
  levels = c(1,0),
  labels = c("Yes", "No"))

# Convert age from a factor into a numeric variable
df_br$age <- as.numeric(as.character(df_br$age))
class(df_br$age) #verify if it worked
df_br <- df_br[order(df_br$ID),] #go back to original order

### Save/load tidy data frame ====
## Load it
setwd("INSERT PATH TO WORKING DIRECTORY HERE") # set working directory

## Uploading tidy data frame (make sure the file is on the working directory)
df_br <- read.csv("tidy_data.csv", header=TRUE, sep=",")

## Save it
write.csv(df_br, "tidy_data.csv", row.names=FALSE)

### Checking data ====
names(df_br) # names of the variables
View(df_br) # view data frame as a spreadsheet
duplicated(df_br) # check for duplicates
summary(df_br) # statistics such as mean, median, and SD

## Demographics
# Install and load packages
install.packages("psych")
library(psych)

prop.table(table(df_br$sex)) # sex
prop.table(table(df_br$brazilian)) # Brazilian
prop.table(table(df_br$skin_colour)) # skin colour
describe(df_br$age) # age

### Missing data ====
## Check percentage of NA for variables associated with completing 59%
## of the study (A10_1)
sum(is.na(df_br$A10_1))

## Create new data frame with complete cases in this column
df_br <- df_br[!is.na(df_br["A10_1"]),]

## Check percentage of NA in the final columns
sum(is.na(df_br$brazilian))

## As only 7 participants remained with NA, we opted to exclude them too
## as power would not be considerably affected by a few cases
df_br <- df_br[!is.na(df_br["brazilian"]),]

## Save this data frame
write.csv(df_br, "pqp_br.csv", row.names=FALSE)

## Load it
df_br <- read.csv("pqp_br.csv", header=TRUE, sep=",")

### Data frame including other countries ====
# Install and load packages
install.packages("readxl")
library(readxl)

setwd("INSERT PATH TO WORKING DIRECTORY HERE") # set working directory

## Upload data frames for each country (make sure the file is on the 
## working directory)
# Brazil
df_br <- read.csv("tidy_data.csv", header=TRUE, sep=";")

# Italy
df_it <- read_excel("ItalianQRPData.xlsx")

# US
df_us <- read_excel("US_data.xlsx")

## Brazil data cleaning
# Exclude demographics and open questions from Brazillian data frame
df_comp <- subset(df_br, select = -c(
  C1_2, C1_3, C2_2, C2_3, C3_2, C3_3, C4_2, C4_3, C5_2, C5_3,
  C6_2, C6_3, C7_2, C7_3, C8_2, C8_3, C9_2, C9_3, C10_2, C10_3, main_area,
  area_other, do_research, funding, funding_5_years, observation, experiment,
  modelling, survey, method_other, method_other_descript, clinic, lab,
  field, location_other, location_other_descript, institution_priority,
  institution_type, position, position_other, higher_degree, sex, skin_colour,
  age, brazilian, other_nation, other_nation_descript, country_higher_degree,
  suggestions))

df_comp$study <- "1" #creating study variable for BR data frame

# Check variable types of the columns
sapply(df_comp, class)

# Change variables into numeric
i <- c(2:45)
df_comp[ , i] <- lapply(df_comp[ , i], as.numeric)

# Change variables into  character
df_comp$ID <- as.character(df_comp$ID)

## IT data cleaning
# Exclude useless variables 
df_it[ ,c(2, 3, 47, 49:54)] <- list(NULL)

# Rename variables
colnames(df_it) <- c(
  "ID", "A1_1", "A1_2", "B1", "C1_1",                     
  "A2_1", "A2_2", "B2", "C2_1", "A3_1", "A3_2",                     
  "B3", "C3_1", "A4_1", "A4_2", "B4", "C4_1",                    
  "A5_1", "A5_2", "B5", "C5_1", "A6_1", "A6_2",                      
  "B6", "C6_1", "A7_1", "A7_2", "B7", "C7_1",                     
  "A8_1", "A8_2", "B8", "C8_1", "A9_1", "A9_2",                   
  "B9", "C9_1","A10_1", "A10_2", "B10", "C10_1",
  "doubt_other_inst", "doubt_same_inst", "doubt_students", "doubt_yourself")

df_it$study <- "2" # create study variable for IT data frame

write.csv(df_it, "QRP_it.csv") # save US database

## US data cleaning
# Excluding demographics and open questions from US data frame and 
# standardizing variable names
df_us <- subset(df_us, finished == 1 & bts == 0) #selecting only complete cases from control group

# Exclude useless variables 
df_us <- subset(df_us, select = -c(
  wave, finished, StartDate, EndDate, bts, charity, displayorder,
  instructions, quiz, incorrect1, instructions2, quiz2, incorrect2,
  instructions3, quiz3, incorrect3, quizanscorrect, doubts_collab))

# Rename variables
colnames(df_us) <- c(
  "ID", "A1_1", "A1_2", "B1", "C1_1",                     
  "A2_1", "A2_2", "B2", "C2_1", "A3_1", "A3_2",                     
  "B3", "C3_1", "A4_1", "A4_2", "B4", "C4_1",                    
  "A5_1", "A5_2", "B5", "C5_1", "A6_1", "A6_2",                      
  "B6", "C6_1", "A7_1", "A7_2", "B7", "C7_1",                     
  "A8_1", "A8_2", "B8", "C8_1", "A9_1", "A9_2",                   
  "B9", "C9_1","A10_1", "A10_2", "B10", "C10_1",
  "doubt_other_inst", "doubt_same_inst", "doubt_students", "doubt_yourself")

df_us$study <- "3" # create study variable for US data frame

write.csv(df_us, "QRP_us.csv") # save US data frame

## merging data frames
df_total <- rbind(df_comp, df_it, df_us)

### Create factors and labels for merged data frame ====
df_total$`B1` <- factor(df_total$`B1`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C1_1` <- factor(df_total$`C1_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B2` <- factor(df_total$`B2`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C2_1` <- factor(df_total$`C2_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B3` <- factor(df_total$`B3`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C3_1` <- factor(df_total$`C3_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B4` <- factor(df_total$`B4`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C4_1` <- factor(df_total$`C4_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B5` <- factor(df_total$`B5`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C5_1` <- factor(df_total$`C5_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B6` <- factor(df_total$`B6`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C6_1` <- factor(df_total$`C6_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B7` <- factor(df_total$`B7`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C7_1` <- factor(df_total$`C7_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B8` <- factor(df_total$`B8`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C8_1` <- factor(df_total$`C8_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B9` <- factor(df_total$`B9`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C9_1` <- factor(df_total$`C9_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`B10` <- factor(df_total$`B10`,
  levels = c(1, 2, 0),
  labels = c("Yes", "No", "No"))

df_total$`C10_1` <- factor(df_total$`C10_1`,
  levels = c(1, 2, 3),
  labels = c("No", "Possibly", "Yes"))

df_total$`study` <- factor(df_total$`study`,
  levels = c(1, 2, 3),
  labels = c("Brazil", "Italy", "US"))

df_total[df_total == -77] <- NA # assign value -77 as NA
df_total[df_total == -66] <- NA # assign value -66 as NA

# Install and load packages
install.packages("psych")
library(psych)

# Descriptive statistics
describeBy(df_total, df_total$study)

### Self-admission ====
# Install and load packages
install.packages("psych")
install.packages("MASS")
library(psych)
library(MASS)

## Create separate data frame with self-admission columns
df_br_2 <- df_br[ , c(4,10,16,22,28,34,40,46,52,58)]

## Reorder new data frame to be more easily compared to Agnoli et al. (2017)
df_br_2 <- df_br_2[,c(3,4,2,5,9,1,8,6,10,7)]

## Create this function
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}
## Apply the function "tblFun" to the "B" columns of self-admission
do.call(rbind,lapply(df_br_2[c(1:10)],tblFun))

## Calculate 95% CI for proportions
#B3
n = length(df_br$B3)    # valid responses count 
k = sum(df_br$B3 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B4
n = length(df_br$B4)    # valid responses count 
k = sum(df_br$B4 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B2
n = length(df_br$B2)    # valid responses count 
k = sum(df_br$B2 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B5
n = length(df_br$B5)    # valid responses count 
k = sum(df_br$B5 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B9
n = length(df_br$B9)    # valid responses count 
k = sum(df_br$B9 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E)  

#B1
n = length(df_br$B1)    # valid responses count 
k = sum(df_br$B1 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B8
n = length(df_br$B8)    # valid responses count 
k = sum(df_br$B8 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B6
n = length(df_br$B6)    # valid responses count 
k = sum(df_br$B6 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E)  

#B10
n = length(df_br$B10)    # valid responses count 
k = sum(df_br$B10 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

#B7
n = length(df_br$B7)    # valid responses count 
k = sum(df_br$B7 == 1) 
pbar = k/n; pbar 
SE = sqrt(pbar*(1-pbar)/n); SE     # standard error 
E = qnorm(.975)*SE; E              # margin of error 
pbar + c(-E, E) 

## Frequencies of self-admission
# Install and load packages
install.packages("plyr")
library(plyr)

# Count each QRP self-admission N
count(df_br_2$B3)
count(df_br_2$B4)
count(df_br_2$B2)
count(df_br_2$B5)
count(df_br_2$B9)
count(df_br_2$B1)
count(df_br_2$B8)
count(df_br_2$B6)
count(df_br_2$B10)
count(df_br_2$B7)

## Calculate the percentage of researchers who admitted
## engaging in at least one QRP
# Create the new column (least1)
df_br_2$least1 <- ((rowSums(df_br_2 == 1, na.rm=T) > 0) * 1)

# Calculate the percentage
prop.table(table(df_br_2$least1))

### Defensibility ====
# Install and load packages
install.packages("plyr")
library(plyr)

## Create data frame with participants that admitted commiting QRP 3
df_br_3 <- df_br[ which(df_br$B3==1), ]

## Check if it worked
count(df_br$B3) # check frequency of "yes" (1) in the original data frame
count(df_br_3$B3) # check frequency only with self-admitted participants
prop.table(table(df_br_3$C3_1)) #calculate percentage to include in Table 2

## Create data frame only with participants who admitted commiting QRP 4
df_br_4 <- df_br[ which(df_br$B4==1), ]

## Check if it worked
count(df_br$B4) # check frequency of "yes" (1) in the original data frame
count(df_br_4$B4) # check frequency only with self-admitted participants
prop.table(table(df_br_4$C4_1)) #calculate percentage to include in Table 2

## Create data frame only with participants who admitted commiting QRP 2
df_br_2 <- df_br[ which(df_br$B2==1), ]

## Check if it worked
count(df_br$B2) # check frequency of "yes" (1) in the original data frame
count(df_br_2$B2) # check frequency only with self-admitted participants
prop.table(table(df_br_2$C2_1)) #calculate percentage to include in Table 2

## Create data frame only with participants who admitted commiting QRP 5
df_br_5 <- df_br[ which(df_br$B5==1), ]

## Check if it worked
count(df_br$B5) # check frequency of "yes" (1) in the original data frame
count(df_br_5$B5) # check frequency only with self-admitted participants
prop.table(table(df_br_5$C5_1)) #calculate percentage to include in Table 5

## Create data frame only with participants who admitted commiting QRP 9
df_br_9 <- df_br[ which(df_br$B9==1), ]

## Check if it worked
count(df_br$B9) # check frequency of "yes" (1) in the original data frame
count(df_br_9$B9) # check frequency only with self-admitted participants
prop.table(table(df_br_9$C9_1)) #calculate percentage to include in Table 9

## Create data frame only with participants who admitted commiting QRP 1
df_br_1 <- df_br[ which(df_br$B1==1), ]

## Check if it worked
count(df_br$B1) # check frequency of "yes" (1) in the original data frame
count(df_br_1$B1) # check frequency only with self-admitted participants
prop.table(table(df_br_1$C1_1)) #calculate percentage to include in Table 1

## Create data frame only with participants who admitted commiting QRP 8
df_br_8 <- df_br[ which(df_br$B8==1), ]

## Check if it worked
count(df_br$B8) # check frequency of "yes" (8) in the original data frame
count(df_br_8$B8) # check frequency only with self-admitted participants
prop.table(table(df_br_8$C8_1)) #calculate percentage to include in Table 8

## Create data frame only with participants who admitted commiting QRP 6
df_br_6 <- df_br[ which(df_br$B6==1), ]

## Check if it worked
count(df_br$B6) # check frequency of "yes" (6) in the original data frame
count(df_br_6$B6) # check frequency only with self-admitted participants
prop.table(table(df_br_6$C6_1)) #calculate percentage to include in Table 6

## Create data frame only with participants who admitted commiting QRP 10
df_br_10 <- df_br[ which(df_br$B10==1), ]

## Check if it worked
count(df_br$B10) # check frequency of "yes" (10) in the original data frame
count(df_br_10$B10) # check frequency only with self-admitted participants
prop.table(table(df_br_10$C10_1)) #calculate percentage to include in Table 10

## Create data frame only with participants who admitted commiting QRP 7
df_br_7 <- df_br[ which(df_br$B7==1), ]

## Check if it worked
count(df_br$B7) # check frequency of "yes" (7) in the original data frame
count(df_br_7$B7) # check frequency only with self-admitted participants
prop.table(table(df_br_7$C7_1)) #calculate percentage to include in Table 7

### Graph (defensibility) ====
## Install and load packages
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyr)
library(ggplot2)
library(dplyr)

## Create data frame with the information that will be ploted
Country <- c("US", "US", "US", "US", "US", "US", "US", "US", "US", "US",
  "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "IT", "BR", "BR",
  "BR", "BR", "BR", "BR", "BR", "BR", "BR", "BR")
factor(Country)

QRP <- c("QRP 1", "QRP 2", "QRP 3", "QRP 4", "QRP 5", "QRP 6", "QRP 7",
  "QRP 8", "QRP 9", "QRP 10", "QRP 1", "QRP 2", "QRP 3", "QRP 4", "QRP 5",
  "QRP 6", "QRP 7", "QRP 8", "QRP 9", "QRP 10", "QRP 1", "QRP 2", "QRP 3",
  "QRP 4", "QRP 5", "QRP 6", "QRP 7", "QRP 8", "QRP 9", "QRP 10")
factor(QRP)

No <- c(1, 1, 0, 3, 4, 3, 5, 8, 0, 100, 6, 3, 14, 9, 16, 14, 9, 9, 43, 60,
  6, 4, 6, 4, 14.6, 3, 9, 10, 20, 50) 

Pos <- c(10, 21, 17, 16, 22, 26, 25, 35, 50, 0, 60, 42, 72, 70, 47, 66, 82,
  73, 29, 20, 52, 31, 40, 21, 31.7, 42, 52, 57, 70, 0)

Yes <- c(89, 78, 83, 81, 74, 71, 70, 57, 50, 0, 34, 55, 14, 22, 37, 21, 9,
  18, 29, 20, 42, 65, 54, 75, 53.6, 55, 39, 33, 10, 50)

df <- data.frame(Country, QRP, No, Pos, Yes)

## Prepare data frame with long format
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor
# - (instead of character vector)
keycol <- "Defensible"
valuecol <- "val"
gathercols <- c("No", "Pos", "Yes")
df_long <- gather_(df, keycol, valuecol, gathercols)

df_long$Defensible <- as.factor(df_long$Defensible) # convert into a factor

# Reorder it to be aligned with Agnoli et al.
df_long$QRP <- factor(df_long$QRP,
  levels = c("QRP 1", "QRP 2", "QRP 3", "QRP 4", "QRP 5", "QRP 6",
    "QRP 7", "QRP 8", "QRP 9", "QRP 10"))

## Main plot
tiff("test.tiff", units="in", width=9, height=4, res=400)
ggplot(data = df_long, aes(factor(QRP), y = val, fill = Defensible)) +
  stat_summary(fun.y="mean", geom="bar",
    alpha = .6, position=position_dodge()) +
  scale_fill_manual(name="Defensible",
    values=c("firebrick1", "yellow3", "deepskyblue3")) +
  xlab("QRP") + ylab("Percentage") +
  theme(axis.text.x=element_text(angle = 30,hjust = 1,vjust = 1)) +
  coord_cartesian(ylim = c(1, 100)) +
  facet_wrap( ~ Country)
dev.off()
    
### Prevalence estimates ====
install.packages("plyr")
install.packages("dplyr")
install.packages("rlang")
library(plyr)
library(dplyr)
library(rlang)

## Mean prevalence (already in percentage)
attach(df_br)
mean_prev <- mean(c(A1_1,A2_1,A3_1,A4_1,A5_1,A6_1,A7_1,A8_1,A9_1,A10_1)) 
mean_prev

## Mean admission estimate (already in percentage)
attach(df_br)
mean_adm <- mean(c(A1_2,A2_2,A3_2,A4_2,A5_2,A6_2,A7_2,A8_2,A9_2,A10_2)) 
mean_adm

## Mean self-admission
# Separate data frame with self-admission columns and ID
df_br_2 <- df_br[ , c(1,4,10,16,22,28,34,40,46,52,58)]

# Recode variable values to 0 and 1
df_br_2 <- df_br_2 %>% mutate_at(vars(2:11),
  funs(recode(., '1' = 1, '2' = 0, .default = NaN)))

# % of self-admission (B1)
attach(df_br_2)
x <- length(which(B1 == 1))
x <- as.numeric(x)
y <- x/232
self_B1 <- y*100
self_B1
# 54.74

# % of self-admission (B2)
attach(df_br_2)
x <- length(which(B2 == 1))
x <- as.numeric(x)
y <- x/232
self_B2 <- y*100
self_B2
# 34.48

# % of self-admission (B3)
attach(df_br_2)
x <- length(which(B3 == 1))
x <- as.numeric(x)
y <- x/232
self_B3 <- y*100
self_B3
# 22.41

# % of self-admission (B4)
attach(df_br_2)
x <- length(which(B4 == 1))
x <- as.numeric(x)
y <- x/232
self_B4 <- y*100
self_B4
# 22.41

# % of self-admission (B5)
attach(df_br_2)
x <- length(which(B5 == 1))
x <- as.numeric(x)
y <- x/232
self_B5 <- y*100
self_B5
# 10.34

# % of self-admission (B6)
attach(df_br_2)
x <- length(which(B6 == 1))
x <- as.numeric(x)
y <- x/232
self_B6 <- y*100
self_B6
# 9.05

# % of self-admission (B7)
attach(df_br_2)
x <- length(which(B7 == 1))
x <- as.numeric(x)
y <- x/232
self_B7 <- y*100
self_B7
# .86

# % of self-admission (B8)
attach(df_br_2)
x <- length(which(B8 == 1))
x <- as.numeric(x)
y <- x/232
self_B8 <- y*100
self_B8
# 19.83

# % of self-admission (B9)
attach(df_br_2)
x <- length(which(B9 == 1))
x <- as.numeric(x)
y <- x/232
self_B9 <- y*100
self_B9
# 17.67

# % of self-admission (B10)
attach(df_br_2)
x <- length(which(B10 == 1))
x <- as.numeric(x)
y <- x/232
self_B10 <- y*100
self_B10
# 4.31

# Mean self-admission
mean_admission <- mean(c(self_B1, self_B2, self_B3, self_B4, self_B5,
  self_B6, self_B7, self_B8, self_B9, self_B10)) 
mean_admission
# 19.61

## Mean derived prevalence estimate
# Derived prevalence estimate (B1)
y <- mean(df_br$A1_2)
B1_derived <- self_B1/y
B1_derived_perc <- B1_derived*100
B1_derived_perc
# 109.56

# Derived prevalence estimate (B2)
y <- mean(df_br$A2_2)
B2_derived <- self_B2/y
B2_derived_perc <- B2_derived*100
B2_derived_perc
# 82.76

# Derived prevalence estimate (B3)
y <- mean(df_br$A3_2)
B3_derived <- self_B3/y
B3_derived_perc <- B3_derived*100
B3_derived_perc
# 55.46

# Derived prevalence estimate (B4)
y <- mean(df_br$A4_2)
B4_derived <- self_B4/y
B4_derived_perc <- B4_derived*100
B4_derived_perc
# 50.87

# Derived prevalence estimate (B5)
y <- mean(df_br$A5_2)
B5_derived <- self_B5/y
B5_derived_perc <- B5_derived*100
B5_derived_perc
# 28.18

# Derived prevalence estimate (B6)
y <- mean(df_br$A6_2)
B6_derived <- self_B6/y
B6_derived_perc <- B6_derived*100
B6_derived_perc
# 30.50

# Derived prevalence estimate (B7)
y <- mean(df_br$A7_2)
B7_derived <- self_B7/y
B7_derived_perc <- B7_derived*100
B7_derived_perc
# 8.79

# Derived prevalence estimate (B8)
y <- mean(df_br$A8_2)
B8_derived <- self_B8/y
B8_derived_perc <- B8_derived*100
B8_derived_perc
# 66.40

# Derived prevalence estimate (B9)
y <- mean(df_br$A9_2)
B9_derived <- self_B9/y
B9_derived_perc <- B9_derived*100
B9_derived_perc
# 50.16

# Derived prevalence estimate (B10)
y <- mean(df_br$A10_2)
B10_derived <- self_B10/y
B10_derived_perc <- B10_derived*100
B10_derived_perc
# 18.56

# Mean derived prevalence estimate
mean_derived <- mean(c(B1_derived_perc, B2_derived_perc, B3_derived_perc,
  B4_derived_perc, B5_derived_perc, B6_derived_perc, B7_derived_perc,
  B8_derived_perc, B9_derived_perc, B10_derived_perc)) 
mean_derived
# 50.12

### Graph (self-admission x prevalence x derived) ====
## Install and load packages
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyr)
library(ggplot2)
library(dplyr)

## Create separate data frame with self-admission rate,
## prevalence estimates, and admission estimates
df_br_2 <- data.frame(df_br[ , c(4,10,16,22,28,34,40,46,52,58,
  2,8,14,20,26,32,38,44,50,56,
  3,9,15,21,27,33,39,45,51,57)])

## Recode self-admission columns' values to 100 and 0
df_br_2 <- df_br_2 %>% mutate_at(vars(1:10),
  funs(recode(., '1' = 100, '2' = 0, .default = NaN)))

## Create derived prevalence columns
# QRP 1
df_br_2 <- transform(df_br_2, D1 = B1 / A1_2)
df_br_2$D1 <- df_br_2$D1*100
df_br_2$D1[!is.finite(df_br_2$D1)] <- 0

# QRP 2
df_br_2 <- transform(df_br_2, D2 = B2 / A2_2)
df_br_2$D2 <- df_br_2$D2*100
df_br_2$D2[!is.finite(df_br_2$D2)] <- 0

# QRP 3
df_br_2 <- transform(df_br_2, D3 = B3 / A3_2)
df_br_2$D3 <- df_br_2$D3*100
df_br_2$D3[!is.finite(df_br_2$D3)] <- 0

# QRP 4
df_br_2 <- transform(df_br_2, D4 = B4 / A4_2)
df_br_2$D4 <- df_br_2$D4*100
df_br_2$D4[!is.finite(df_br_2$D4)] <- 0

# QRP 5
df_br_2 <- transform(df_br_2, D5 = B5 / A5_2)
df_br_2$D5 <- df_br_2$D5*100
df_br_2$D5[!is.finite(df_br_2$D5)] <- 0

# QRP 6
df_br_2 <- transform(df_br_2, D6 = B6 / A6_2)
df_br_2$D6 <- df_br_2$D6*100
df_br_2$D6[!is.finite(df_br_2$D6)] <- 0

# QRP 7
df_br_2 <- transform(df_br_2, D7 = B7 / A7_2)
df_br_2$D7 <- df_br_2$D7*100
df_br_2$D7[!is.finite(df_br_2$D7)] <- 0

# QRP 8
df_br_2 <- transform(df_br_2, D8 = B8 / A8_2)
df_br_2$D8 <- df_br_2$D8*100
df_br_2$D8[!is.finite(df_br_2$D8)] <- 0

# QRP 9
df_br_2 <- transform(df_br_2, D9 = B9 / A9_2)
df_br_2$D9 <- df_br_2$D9*100
df_br_2$D9[!is.finite(df_br_2$D9)] <- 0

# QRP 10
df_br_2 <- transform(df_br_2, D10 = B10 / A10_2)
df_br_2$D10 <- df_br_2$D10*100
df_br_2$D10[!is.finite(df_br_2$D10)] <- 0

## Prepare data frame with long format
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
keycol <- "QRP"
valuecol <- "val"
gathercols <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10",
  "A1_1", "A2_1", "A3_1", "A4_1", "A5_1", "A6_1", "A7_1", "A8_1", "A9_1",
  "A10_1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10")
df_long <- gather_(df_br_2, keycol, valuecol, gathercols)

# Create new dummy variable for QRP type
df_long$QRP_type <- 1
df_long[1:min(2320, nrow(df_long)),]$QRP_type <- 0
df_long[4641:min(6960, nrow(df_long)),]$QRP_type <- 2

# Give labels to dummy variable
df_long$QRP_type <- factor(df_long$QRP_type,
  levels = c(0, 1, 2),
  labels = c("Self-admission", "Prevalence estimate",
    "Derived prevalence")) ## insert labels

# QRP preparation
df_long$QRP <- as.factor(df_long$QRP) # convert into a factor

# Rename levels of QRP column 
levels(df_long$QRP)[levels(df_long$QRP)=="B1"] <- "Reporting studies that worked"
levels(df_long$QRP)[levels(df_long$QRP)=="B2"] <- "Omitting conditions"
levels(df_long$QRP)[levels(df_long$QRP)=="B3"] <- "Omitting measures"
levels(df_long$QRP)[levels(df_long$QRP)=="B4"] <- "Collecting data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="B5"] <- "Stopping data collection"
levels(df_long$QRP)[levels(df_long$QRP)=="B6"] <- "A posteriori hypothesizing"
levels(df_long$QRP)[levels(df_long$QRP)=="B7"] <- "Falsifying data"
levels(df_long$QRP)[levels(df_long$QRP)=="B8"] <- "Excluding data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="B9"] <- "Rounding off p-values"
levels(df_long$QRP)[levels(df_long$QRP)=="B10"] <- "Demographics"
levels(df_long$QRP)[levels(df_long$QRP)=="A1_1"] <- "Reporting studies that worked"
levels(df_long$QRP)[levels(df_long$QRP)=="A2_1"] <- "Omitting conditions"
levels(df_long$QRP)[levels(df_long$QRP)=="A3_1"] <- "Omitting measures"
levels(df_long$QRP)[levels(df_long$QRP)=="A4_1"] <- "Collecting data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="A5_1"] <- "Stopping data collection"
levels(df_long$QRP)[levels(df_long$QRP)=="A6_1"] <- "A posteriori hypothesizing"
levels(df_long$QRP)[levels(df_long$QRP)=="A7_1"] <- "Falsifying data"
levels(df_long$QRP)[levels(df_long$QRP)=="A8_1"] <- "Excluding data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="A9_1"] <- "Rounding off p-values"
levels(df_long$QRP)[levels(df_long$QRP)=="A10_1"] <- "Demographics"
levels(df_long$QRP)[levels(df_long$QRP)=="D1"] <- "Reporting studies that worked"
levels(df_long$QRP)[levels(df_long$QRP)=="D2"] <- "Omitting conditions"
levels(df_long$QRP)[levels(df_long$QRP)=="D3"] <- "Omitting measures"
levels(df_long$QRP)[levels(df_long$QRP)=="D4"] <- "Collecting data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="D5"] <- "Stopping data collection"
levels(df_long$QRP)[levels(df_long$QRP)=="D6"] <- "A posteriori hypothesizing"
levels(df_long$QRP)[levels(df_long$QRP)=="D7"] <- "Falsifying data"
levels(df_long$QRP)[levels(df_long$QRP)=="D8"] <- "Excluding data based on results"
levels(df_long$QRP)[levels(df_long$QRP)=="D9"] <- "Rounding off p-values"
levels(df_long$QRP)[levels(df_long$QRP)=="D10"] <- "Demographics"

# Reorder it to be aligned with Agnoli et al.
df_long$QRP <- factor(df_long$QRP,
  levels = c("Omitting measures", "Collecting data based on results",
    "Omitting conditions", "Stopping data collection",
    "Rounding off p-values", "Reporting studies that worked",
    "Excluding data based on results", "A posteriori hypothesizing",
    "Demographics", "Falsifying data"))

# CI and error bars
# Install and load package
install.packages('Rmisc', dependencies = TRUE)
library(Rmisc)

# Generate data frame with CI
bars <- summarySE(df_long, measurevar="val", groupvars=c("QRP","QRP_type"))

# If value is greater than 100, change it to 100
bars$val[bars$val > 100] <- 100

## Main plot
tiff("test.tiff", units="in", width=9, height=4, res=400)
ggplot(data = bars, aes(factor(QRP), y = val, fill = QRP_type)) +
  stat_summary(fun.y="mean", geom="bar",
    alpha = .6, position=position_dodge()) +
  scale_fill_manual(name="Estimate",
    values=c("chartreuse1", "turquoise3", "darkorchid2")) +
  xlab("QRP") + ylab("Percentage") +
  geom_errorbar(aes(ymin =val-ci, ymax =val+ci),
    width=.2, position=position_dodge(.9)) +
  theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1)) +
  coord_cartesian(ylim = c(1, 100)) 
dev.off()

### Graph (doubt about integrity) ====
## Install and load packages
install.packages("ggplot2")
install.packages("scales")
install.packages("RColorBrewer")
install.packages("reshape")
library(ggplot2)
library(scales)
library(RColorBrewer)
library(reshape)

## Create factors
raw <- df_br
raw[,62]<-factor(raw[,62],
  levels=c("Never", "Once or twice", "Occasionally", "Often"),
  ordered=FALSE)
raw[,63]<-factor(raw[,63],
  levels=c("Never", "Once or twice", "Occasionally", "Often"),
  ordered=FALSE)
raw[,64]<-factor(raw[,64],
  levels=c("Never", "Once or twice", "Occasionally", "Often"),
  ordered=FALSE)
raw[,65]<-factor(raw[,65],
  levels=c("Never", "Once or twice", "Occasionally", "Often"),
  ordered=FALSE)

raw=raw[,c(62:65)] # selecting only the necessary columns
freq=table(col(raw), as.matrix(raw)) # get the counts of each factor level

# Create list of names
Names=c("Other Institutions", "Your Institution",
  "Graduate Students", "Yourself")  
data=data.frame(cbind(freq),Names)   # combine them into a data frame
data=data[,c(5,1,4,2,3)]             # sort columns

# Melt the data frame for plotting
data.m <- melt(data, id.vars='Names')

# Change "variable" column's name
colnames(data.m) <- c("Names","Responses","value")

# Change levels' labels
levels(data.m$Responses)[levels(data.m$Responses)=="Never"] <- "Never"
levels(data.m$Responses)[levels(data.m$Responses)=="Once.or.twice"] <- "Once or twice"
levels(data.m$Responses)[levels(data.m$Responses)=="Ocasionally"] <- "Ocasionally"
levels(data.m$Responses)[levels(data.m$Responses)=="Often"] <- "Often"

# Main plot
tiff("test.tiff", units="in", width=7, height=5, res=400)
ggplot(data.m,aes(x = Names, y = value,fill = Responses)) + 
  geom_bar(position = "fill",stat = "identity", alpha = .6) +
  labs(x = "Category of Researcher") +
  labs(y = "Percentage") +
  scale_y_continuous(labels = percent_format())
dev.off()

# Additional descriptive statistics for comparison
# Doubt about "Yourself"
71/232
# Occasionaly or often (doubts about others)
103/232

### Table (self-admission x academic demographics) ====
## Install and load packages
install.packages("plyr")
install.packages("dplyr")
install.packages("rlang")
library(plyr)
library(dplyr)
library(rlang)

## Number of members in each category
table(df_br$main_area) # area (N)
table(df_br$position) # position (N)

## Separate data frame with self-admission columns and ID
df_br_2 <- df_br[ , c(1,4,10,16,22,28,34,40,46,52,58)]

## Reorder new data frame to be more easily comparable to other studies
df_br_2 <- df_br_2[,c(1,4,5,3,6,10,2,9,7,11,8)]

## Recode variable values to 0 and 1
df_br_2 <- df_br_2 %>% mutate_at(vars(2:11),
  funs(recode(., '1' = 1, '2' = 0, .default = NaN)))

## Total number of self-admissions for each respondent
df_br_2$sum_self_admission <- apply(df_br_2[ ,2:11], 1, function(x)
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE)))

## Add demographic variables
# Create data frame (demo) with ID and demographic variables
demo <- df_br[ , c(1,66,84)]

# Add demo columns to df_br_2
df_br_3 <- merge(df_br_2, demo[, c(1:3)], by = "ID", all.x = TRUE)

# Calculate mean self-admission by area
mean_score <- aggregate(sum_self_admission ~ main_area, df_br_3, mean)
mean_rate <- mean_score$sum_self_admission/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$sum_self_admission <- NULL
mean_score

# Calculate mean self-admission by career position
mean_score <- aggregate(sum_self_admission ~ position, df_br_3, mean)
mean_rate <- mean_score$sum_self_admission/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$sum_self_admission <- NULL
mean_score

### Table (defensibility x academic demographics) ====
## Install and load necessary packages
install.packages("plyr")
install.packages("dplyr")
install.packages("rlang")
library(plyr)
library(dplyr)
library(rlang)

## Create a separate data frame with defensibility columns and ID
attach(df_br)
df_br_2 <- df_br[ , c("ID","C1_1","C2_1","C3_1","C4_1","C5_1","C6_1",
  "C7_1","C8_1","C9_1","C10_1","main_area","position")]

## Recode variable values so that "yes" is the only "1"
df_br_yes <- df_br_2 %>% mutate_at(vars(2:11),
  funs(recode(., '3' = 1, '2' = 0, '1' = 0, .default = NaN)))
summary(df_br_2) #check it

## Sum of "yes" to defensibility question for each respondent
df_br_yes$yes <- apply(df_br_yes[ ,2:11], 1, function(x)
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE)))

# Mean "yes" by area
mean_score <- aggregate(yes ~ main_area, df_br_yes, mean)
mean_rate <- mean_score$yes/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$yes <- NULL
mean_score

# Mean "yes" by career position
mean_score <- aggregate(yes ~ position, df_br_yes, mean)
mean_rate <- mean_score$yes/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$yes <- NULL
mean_score

## Recode variable values so that "possibly" is the only "1"
df_br_possibly <- df_br_2 %>% mutate_at(vars(2:11),
  funs(recode(., '3' = 0, '2' = 1, '1' = 0, .default = NaN)))

## Sum of "possibly" to defensibility question for each respondent
df_br_possibly$possibly <- apply(df_br_possibly[ ,2:11], 1, function(x)
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE)))

# Mean "possibly" by area
mean_score <- aggregate(possibly ~ main_area, df_br_possibly, mean)
mean_rate <- mean_score$possibly/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$possibly <- NULL
mean_score

# Mean "possibly" by career position
mean_score <- aggregate(possibly ~ position, df_br_possibly, mean)
mean_rate <- mean_score$possibly/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$possibly <- NULL
mean_score

## Recode variable values so that "no" is the only "1"
df_br_no <- df_br_2 %>% mutate_at(vars(2:11),
  funs(recode(., '3' = 0, '2' = 0, '1' = 1, .default = NaN)))

## Sum of "no" to defensibility question for each respondent
df_br_no$no <- apply(df_br_no[ ,2:11], 1, function(x)
  ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE)))

# Mean "no" by area
mean_score <- aggregate(no ~ main_area, df_br_no, mean)
mean_rate <- mean_score$no/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$no <- NULL
mean_score

# Mean "no" by career position
mean_score <- aggregate(no ~ position, df_br_no, mean)
mean_rate <- mean_score$no/10
percentage_rate <- mean_rate*100
mean_score$percentage_rate <- percentage_rate
mean_score$no <- NULL
mean_score

### Correlations between countries ====
## Download the "corr.csv" file at the project's OSF page
## and run the following code
## Get the file
correlations <- read.csv("corr.csv", sep = ";")

## Main command
correlations <- correlations[,-c(1)]
correlations$US <- as.numeric(correlations$US)
correlations$IT <- as.numeric(correlations$IT)
correlations$BR <- as.numeric(correlations$BR)
cor(correlations, method = "pearson")