#These are the required packages. This code will automatically load them or install then load
#if they are not present. 

load.lib <- c("reshape2", "scales", "ggplot2", "dplyr", "plyr", "data.table")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

#The first step is to split the dataset into two portions. The first portion is for the 
QRP items, the second is the open science practices. 

data <- read.csv ("Makel_R_Code/Full_Responses.csv")
data_h <- cbind (data$R13_2, data$R14_2, data$R15_2, data$R16_2, data$R17_2)

data_g <- cbind (data$R3_2, data$R4_2, data$R5_2, data$R6_2, data$R7_2,
		     data$R8_2, data$R9_2, data$R10_2, data$R11_2, data$R12_2)

#The second step is to calculate the means and standard deviations for each item.
#This is done using the apply function. 

SD <- apply(na.omit(data_g), 2, sd)
Mean <- apply(na.omit(data_g), 2, mean)

#Following this, a dataset is created with the means and standard deviations.

data1 <- data.frame (rbind (SD, Mean))

#The variables are then renamed to correspond to the names used in our study. 
names(data1) <- c("ONSV*", "Omitting nonSignificant covariates",
			  		  "HARKing", "Omitting analyses", "Rounding p-values", "Data Exclusion ARKing",
					  "Data peeking","Analysis Gaming", "Hiding methodological problems", "Filling in missing data")

#The next portion of the code is used to create rankings from the smallest proportion of 
#individuals who selected an answer choice (in this case the first in the likert scale)
#to the greatest. "1" can be substituted for any rank answer choice. For example "5" would rank
#the items by the proportion of people who selected the fifth answer in the likert scale.
#Finally, this rank variable is merged back into the original dataset. This variable "Var1"
#will be used to order our bar plots in ggplot2.

data1 <- t(data1)
data1 <- data.frame(data1)
data1['Practice'] <- row.names(data1)
row.names(data1) <- NULL
new_row <- data1[order(data1$Mean),]
dataa <- tibble::rowid_to_column(data1[order(data1$Mean),], "Rank")

#The steps for the QRP items is repeated for the open science items. The only exception is that
#The ranks variable is increased by 12. This is to rank them behind the QRP items in the figures.
#QRP items are ranked 1-11, and then OSP are ranked 12-17. 

SD <- apply(na.omit(data_h), 2, sd)
Mean <- apply(na.omit(data_h), 2, mean)

data2 <- data.frame (rbind (SD, Mean))
names(data2) <- c("Pregregisteration", "Data Sharing", "Materials Sharing", "Replication","Open Access")
data2 <- t(data2)
data2 <- data.frame(data2)
data2['Practice'] <- row.names(data2)
row.names(data2) <- NULL
new_row <- data2[order(data2$Mean),]
datab <- tibble::rowid_to_column(data2[order(data2$Mean),], "Rank")
datab$Rank <- datab$Rank + 12

#These two data portions are merged back togeter into a single dataset. 

data <- rbind (dataa, datab)

#This code employs a coding "trick" to properly rank the items in ggplot2. The package 
#uses alphabetical order as the display order for items. Converting the ranks from numbers
#to letter will force ggplot2 to display ranks in the order intended with minimal coding.

Ranks <- letters [data$Rank]
Names <- as.vector(data$Practice)

#Here we rescale the data to put it on a 0 to 1 scale from 0 to 100. 

data$Mean = data$Mean/100
data$SD = data$SD/100

#This is a simple function that changes the displayed number of decimals in ggplot2's axis. 
#The "decimals= " option can be used to dictate the number of decimals. Here it is 0. 

fmt_dcimals <- function(decimals=0){
   function(x) as.character(round(x,decimals))
}

#ggplot2 is used to produce the graphs in this research. Most of the code here deals with the aesthetics of the graph
#rather than its statistical features. For those wishing to use this code, the only vital portions of code in which
#to reproduce the statistical information are the first and second lines of code.

ggplot(data) +
	geom_bar( aes(x=Ranks, y=Mean), stat="identity", fill="gray60", alpha=0.7) +
	geom_errorbar( aes(x=Ranks, ymin=Mean-SD, ymax=Mean+SD), width=0.4, colour="black", alpha=0.9, size=1.3) +
	scale_x_discrete(name = "Practice", labels  = data$Practice ) +
	scale_y_continuous(name = "Estimate of Percentage of Researchers", labels = percent_format(accuracy = 1), expand = c(0,0)) +
	theme(axis.text.x = element_text(face = "bold", size = 15, angle = 45, hjust = 1), panel.grid.major = element_blank(), 
		axis.title.y = element_text(face = "bold", size = 16), axis.text.y = element_text(face = "bold", size = 16),
		legend.text = element_text(face = "bold", size = 16),
		panel.grid.minor = element_blank(), axis.title.x=element_blank(),
		panel.background = element_blank(), axis.line = element_line(colour = "black"))





View (data)

