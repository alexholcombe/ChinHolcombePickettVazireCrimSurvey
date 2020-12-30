#These are the required packages. This code will automatically load them or install then load
#if they are not present. 

load.lib <- c("reshape2", "scales", "ggplot2", "dplyr", "plyr")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


#The first section of code creates proportion tables by items. The code works
#in two steps. the first is to get the frequency table per item using "table".
#Following that, a proportion table is taken of the frequency using "prop.table".


data <- read.csv ("Full_Responses.csv")
R3_3 <- prop.table(table(data$R3_3)) 
R4_3 <- prop.table(table(data$R4_3)) 
R5_3 <- prop.table(table(data$R5_3))
R6_3 <- prop.table(table(data$R6_3))
R7_3 <- prop.table(table(data$R7_3)) 
R8_3 <- prop.table(table(data$R8_3))
R9_3 <- prop.table(table(data$R9_3)) 
R10_3 <- prop.table(table(data$R10_3)) 
R11_3 <- prop.table(table(data$R11_3)) 
R12_3 <- prop.table(table(data$R12_3))
R13_3 <- prop.table(table(data$R13_3)) 
R14_3 <- prop.table(table(data$R14_3))
R15_3 <- prop.table(table(data$R15_3))
R16_3 <- prop.table(table(data$R16_3)) 
R17_3 <- prop.table(table(data$R17_3))

#The next step is to split the dataset into two portions. The first portion is for the 
QRP items, the second is the open science practices. This is done to seperate the 
ordering in the rank order step of the code.  I use rbind to merge the proportions I want
into a single dataset..

data_g <- rbind (R3_3, R4_3, R5_3, R6_3, R7_3, R8_3,
		   R9_3, R10_3, R11_3, R12_3)

#The melt function will turn our 5 columns (one for each choice in our likert scale
#into a single column#

data_gm <- melt (data_g, id.vars = row.names)

#The next portion of the code is used to create rankings from the smallest proportion of 
#individuals who selected an answer choice (in this case the first in the likert scale)
#to the greatest. "1" can be substituted for any rank answer choice. For example "5" would rank
#the items by the proportion of people who selected the fifth answer in the likert scale.
#Finally, this rank variable is merged back into the original dataset. This variable "Var1"
#will be used to order our bar plots in ggplot2.

data_1 <-subset (data_gm, data_gm$Var2== "1")
new_row <- data_1[order(data_1$value),]
data <- tibble::rowid_to_column(data_1[order(data_1$value),], "Rank")
data_f3 <- merge (data_gm, data, by = "Var1")

#The steps for the QRP items is repeated for the open science items. The only exception is that
#The ranks variable is increased by 12. This is to rank them behind the QRP items in the figures.
#QRP items are ranked 1-11, and then OSP are ranked 12-17. 

data_h <- rbind ( R13_3, R14_3, R15_3, R16_3, R17_3)
data_hm <- melt (data_h, id.vars = row.names)
data_2 <-subset (data_hm, data_hm$Var2== "1")
new_row <- data_2[order(data_2$value),]
dataa <- tibble::rowid_to_column(data_2[order(data_2$value),], "Rank")
data_f4 <- merge (data_hm, dataa, by = "Var1")
data_f4$Rank <- data_f4$Rank + 12

#These two data portions are merged back togeter into a single dataset. The columns are then 
#renamed to their practice. 

data_f2 <- rbind (data_f3, data_f4)
data <- rbind (data, dataa)
data$Var1 <- revalue(data$Var1, c("R3_3"="ONSV*", "R4_3"="Omitting nonSignificant covariates",
			  		   "R5_3"="HARKing", "R6_3"="Omitting analyses",
					   "R7_3"="Rounding p-values", "R8_3"="Data Exclusion ARKing","R9_3"="Data peeking", 
					   "R10_3"="Analysis Gaming", "R11_3"="Hiding methodological problems", "R12_3"="Filling in missing data",
				         "R13_3"="Pregregisteration", "R14_3"="Data Sharing",
					   "R15_3"="Materials Sharing", "R16_3"="Replication","R17_3"="Open Access"))

#This code employs a coding "trick" to properly rank the items in ggplot2. The package 
#uses alphabetical order as the display order for items. Converting the ranks from numbers
#to letter will force ggplot2 to display ranks in the order intended with minimal coding.

Ranks <- letters [data_f2$Rank]
Names <- as.vector(data$Var1)

#This code forces the data into a numeric (i.e. continuous) format.

data_f2$value.x <- as.numeric (data_f2$value.x)
data_f2$value.y <- as.numeric (data_f2$value.y)


#ggplot2 is used to produce the graphs in this research. Most of the code here deals with the aesthetics of the graph
#rather than its statistical features. For those wishing to use this code, the only vital portions of code in which
#to reproduce the statistical information are the first and second lines of code.

ggplot(data_f2, aes(x = Ranks, y = value.x, fill=factor(Var2.x, levels = c("5", "4", "3", "2", "1")))) +
	geom_bar(stat="identity") + 
	scale_x_discrete(labels  = Names ) +
	scale_y_continuous(name = "Percent of Respondents", labels = percent, expand = c(0,0)) + 
	scale_fill_manual(values=c("grey80", "grey60", "grey45", "grey30", "grey1"), 
                       name="Choice",
                       breaks=c("5", "4", "3", "2", "1"),
                       labels=c("Almost Always", "Frequently", "Occasionally", "Once", "Never")) +
	theme(axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1), panel.grid.major = element_blank(), 
		axis.title.y = element_text(face = "bold", size = 16), axis.text.y = element_text(face = "bold", size = 16),
		legend.text = element_text(face = "bold", size = 16),
		panel.grid.minor = element_blank(), axis.title.x=element_blank(),
		panel.background = element_blank(), axis.line = element_line(colour = "black"))

