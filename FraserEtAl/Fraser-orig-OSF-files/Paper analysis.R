#.libPaths("C:/R-3.3.2/library")
#Questionable Research Practice analyses. Please excuse the clumsy coding

library(ggplot2)
library(MASS)  
library(Rmisc)
library(binom)
library(ordinal)
library(cowplot)
library(tidyr)

combined<-read.csv("FraserEtAl/combined.csv",na.strings="",stringsAsFactors = FALSE)
ecologists1<-subset(combined,field=='ecology')
evbiologists1<-subset(combined,field=='evolutionary')

#Headline data, proportion of researchers that do it at all
#ecologists
ecologists<-ecologists1[c(10,15,20,25,30,35,40,45,50,55)]
nec<-sapply(ecologists, function(x) length(na.omit(as.numeric(x))))
kec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))!=1))
lowerciec<-binom.confint(kec, nec, method=c("wilson"),type="central")[5]
upperciec<-binom.confint(kec, nec, method=c("wilson"),type="central")[6]
meanec<-binom.confint(kec, nec, method=c("wilson"),type="central")[4]
ecodat<-as.data.frame(cbind(meanec,lowerciec,upperciec))

#evolutionary biologists
evbiologists<-evbiologists1[c(10,15,20,25,30,35,40,45,50,55)]
nev<-sapply(evbiologists, function(x) length(na.omit(as.numeric(x))))
kev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))!=1))
lowerciev<-binom.confint(kev, nev, method=c("wilson"),type="central")[5]
upperciev<-binom.confint(kev, nev, method=c("wilson"),type="central")[6]
meanev<-binom.confint(kev, nev, method=c("wilson"),type="central")[4]
evodat<-as.data.frame(cbind(meanev,lowerciev,upperciev))


#Headline data, suspected proportion of researchers that do it
#ecologists
suspecteco<-ecologists1[c(9,14,19,24,29,34,39,44,49,54)]
suspecteco$Q2.2_1<-as.numeric(as.character(suspecteco$Q2.2_1))
suspecteco$Q3.2_1<-as.numeric(as.character(suspecteco$Q3.2_1))
suspecteco$Q4.2_1<-as.numeric(as.character(suspecteco$Q4.2_1))
suspecteco$Q5.2_1<-as.numeric(as.character(suspecteco$Q5.2_1))
suspecteco$Q6.2_1<-as.numeric(as.character(suspecteco$Q6.2_1))
suspecteco$Q7.2_1<-as.numeric(as.character(suspecteco$Q7.2_1))
suspecteco$Q8.2_1<-as.numeric(as.character(suspecteco$Q8.2_1))
suspecteco$Q9.2_1<-as.numeric(as.character(suspecteco$Q9.2_1))
suspecteco$Q10.2_1<-as.numeric(as.character(suspecteco$Q10.2_1))
suspecteco$Q11.2_1<-as.numeric(as.character(suspecteco$Q11.2_1))
suspecteco<-suspecteco/100
suspectecoresults<-as.data.frame(sapply(suspecteco, function(x) CI(na.omit(x))))
suspectecoresultsa<-as.data.frame(t(suspectecoresults))
suspectecoresultsa$question <- c("QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")

#evolutionary biologists
suspectevo<-evbiologists1[c(9,14,19,24,29,34,39,44,49,54)]
suspectevo$Q2.2_1<-as.numeric(as.character(suspectevo$Q2.2_1))
suspectevo$Q3.2_1<-as.numeric(as.character(suspectevo$Q3.2_1))
suspectevo$Q4.2_1<-as.numeric(as.character(suspectevo$Q4.2_1))
suspectevo$Q5.2_1<-as.numeric(as.character(suspectevo$Q5.2_1))
suspectevo$Q6.2_1<-as.numeric(as.character(suspectevo$Q6.2_1))
suspectevo$Q7.2_1<-as.numeric(as.character(suspectevo$Q7.2_1))
suspectevo$Q8.2_1<-as.numeric(as.character(suspectevo$Q8.2_1))
suspectevo$Q9.2_1<-as.numeric(as.character(suspectevo$Q9.2_1))
suspectevo$Q10.2_1<-as.numeric(as.character(suspectevo$Q10.2_1))
suspectevo$Q11.2_1<-as.numeric(as.character(suspectevo$Q11.2_1))
suspectevo<-suspectevo/100
suspectevoresults<-as.data.frame(sapply(suspectevo, function(x) CI(na.omit(x))))
suspectevoresultsa<-as.data.frame(t(suspectevoresults))
suspectevoresultsa$question <- c("QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")


#Graphs
#Headline Graph

#bind eco and evo datasets together for prevalence
ecodat1<-ecodat
ecodat1$field<-rep("ecology",nrow(ecodat1))
evodat1<-evodat
evodat1$field<-rep("evolution",nrow(evodat1))
graphingdata<-rbind.data.frame(ecodat1,evodat1)
graphingdata$question <- c("QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")
graphingdata$qrp <- c("01 unreported variables","02 unreported covariates","03 HARKing","04 unreported models","05 rounding ps","06 excluding data","07 adding data","08 switching analyses","09 undisclosed problems","10 fabrifaction")
suspectecoresultsa1<-suspectecoresultsa
suspectecoresultsa1$field<-rep("ecology",nrow(suspectecoresultsa1))

suspectevoresultsa1<-suspectevoresultsa
suspectevoresultsa1$field<-rep("evolution",nrow(suspectevoresultsa1))
suspectgraphingdata<-rbind.data.frame(suspectecoresultsa1,suspectevoresultsa1)
suspectgraphingdata$question <- c("QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")
suspectgraphingdata$qrp <- c("01 unreported variables","02 unreported covariates","03 HARKing","04 unreported models","05 rounding ps","06 excluding data","07 adding data","08 switching analyses","09 undisclosed problems","10 fabrifaction")

headlinedots<-
  ggplot(graphingdata, aes(qrp, mean, group = field, fill = factor(field))) +
  geom_col(position = "dodge")+
  geom_errorbar(data=graphingdata,aes(x=qrp,ymin=lower, ymax=upper,group=field),width=.2,position=position_dodge(.9))+
  geom_point(data=suspectgraphingdata, aes(x=qrp, y=mean,group=field),position=position_dodge(width=0.9))+
    theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ylab("proportion prevalence")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
    theme(text=element_text(size=14))+theme(axis.text.x=element_text(angle=45,hjust=1))

headlineNOdots<-
  ggplot(graphingdata, aes(question, mean, group = field, fill = factor(field))) +
  geom_col(position = "dodge")+
  geom_errorbar(data=graphingdata,aes(x=question,ymin=lower, ymax=upper,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("prevalence")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="bottom")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=14))
headlinelegend <- get_legend(headlineNOdots)
plot_grid(headlinelegend,ncol=1)

###Data for all QRP plots##
#ecology
ecologists<-ecologists1[c(10,15,20,25,30,35,40,45,50,55)]
nec<-sapply(ecologists, function(x) length(na.omit(as.numeric(x))))#number answering each question
k1ec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))==1))#number saying they 'never' use this QRP
k2ec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))==2))#number saying they have used it 'once'
k3ec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))==3))#number saying they use it 'occasionally'
k4ec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))==4))#number saying they use it 'frequently'
k5ec<-sapply(ecologists, function(x) sum(na.omit(as.numeric(x))==5))#number saying they use it 'almost always'

ppn1ec<-as.vector(k1ec/nec)
lowerci1ec<-binom.confint(k1ec, nec, method=c("wilson"),type="central")[5]
upperci1ec<-binom.confint(k1ec, nec, method=c("wilson"),type="central")[6]

ppn2ec<-as.vector(k2ec/nec)
lowerci2ec<-binom.confint(k2ec, nec, method=c("wilson"),type="central")[5]
upperci2ec<-binom.confint(k2ec, nec, method=c("wilson"),type="central")[6]

ppn3ec<-as.vector(k3ec/nec)
lowerci3ec<-binom.confint(k3ec, nec, method=c("wilson"),type="central")[5]
upperci3ec<-binom.confint(k3ec, nec, method=c("wilson"),type="central")[6]

ppn4ec<-as.vector(k4ec/nec)
lowerci4ec<-binom.confint(k4ec, nec, method=c("wilson"),type="central")[5]
upperci4ec<-binom.confint(k4ec, nec, method=c("wilson"),type="central")[6]

ppn5ec<-as.vector(k5ec/nec)
lowerci5ec<-binom.confint(k5ec, nec, method=c("wilson"),type="central")[5]
upperci5ec<-binom.confint(k5ec, nec, method=c("wilson"),type="central")[6]


#evolutionary biology
evbiologists<-evbiologists1[c(10,15,20,25,30,35,40,45,50,55)]
nev<-sapply(evbiologists, function(x) length(na.omit(as.numeric(x)))) #number answering
k1ev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))==1))#never
k2ev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))==2))#once
k3ev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))==3))#occasionally
k4ev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))==4))#frequently
k5ev<-sapply(evbiologists, function(x) sum(na.omit(as.numeric(x))==5))#almost always

ppn1ev<-as.vector(k1ev/nev)
lowerci1ev<-binom.confint(k1ev, nev, method=c("wilson"),type="central")[5]
upperci1ev<-binom.confint(k1ev, nev, method=c("wilson"),type="central")[6]

ppn2ev<-as.vector(k2ev/nev)
lowerci2ev<-binom.confint(k2ev, nev, method=c("wilson"),type="central")[5]
upperci2ev<-binom.confint(k2ev, nev, method=c("wilson"),type="central")[6]

ppn3ev<-as.vector(k3ev/nev)
lowerci3ev<-binom.confint(k3ev, nev, method=c("wilson"),type="central")[5]
upperci3ev<-binom.confint(k3ev, nev, method=c("wilson"),type="central")[6]

ppn4ev<-as.vector(k4ev/nev)
lowerci4ev<-binom.confint(k4ev, nev, method=c("wilson"),type="central")[5]
upperci4ev<-binom.confint(k4ev, nev, method=c("wilson"),type="central")[6]

ppn5ev<-as.vector(k5ev/nev)
lowerci5ev<-binom.confint(k5ev, nev, method=c("wilson"),type="central")[5]
upperci5ev<-binom.confint(k5ev, nev, method=c("wilson"),type="central")[6]

##Acceptability##
#ecology
acceptec<-ecologists1
acceptec$QRP1<-with(acceptec,ifelse(Q2.4 %in% c(1, 2),1,ifelse(Q2.4 %in% "NA",NA,0))) #Add a variable for QRP1 that codes "it should be used almost always" and "it should be used often" as 1- acceptable. Other categories are coded as unnacceptable
acceptec$QRP2<-with(acceptec,ifelse(Q3.4 %in% c(1, 2),1,ifelse(Q3.4 %in% "NA",NA,0)))
acceptec$QRP3<-with(acceptec,ifelse(Q4.4 %in% c(1, 2),1,ifelse(Q4.4 %in% "NA",NA,0)))
acceptec$QRP4<-with(acceptec,ifelse(Q5.4 %in% c(1, 2),1,ifelse(Q5.4 %in% "NA",NA,0)))
acceptec$QRP5<-with(acceptec,ifelse(Q6.4 %in% c(1, 2),1,ifelse(Q6.4 %in% "NA",NA,0)))
acceptec$QRP6<-with(acceptec,ifelse(Q7.4 %in% c(1, 2),1,ifelse(Q7.4 %in% "NA",NA,0)))
acceptec$QRP7<-with(acceptec,ifelse(Q8.4 %in% c(1, 2),1,ifelse(Q8.4 %in% "NA",NA,0)))
acceptec$QRP8<-with(acceptec,ifelse(Q9.4 %in% c(1, 2),1,ifelse(Q9.4 %in% "NA",NA,0)))
acceptec$QRP9<-with(acceptec,ifelse(Q10.4 %in% c(1, 2),1,ifelse(Q10.4 %in% "NA",NA,0)))
acceptec$QRP10<-with(acceptec,ifelse(Q11.4 %in% c(1, 2),1,ifelse(Q11.4 %in% "NA",NA,0)))
accepteca <- acceptec[c(10,15,20,25,30,35,40,45,50,55,75,76,77,78,79,80,81,82,83,84)] #get rid of all the extraneous columns

#acceptability QRP1
QRP1neverec<-subset(accepteca,Q2.3==1)
QRP1neverppnec<-mean(QRP1neverec$QRP1,na.rm=TRUE)
QRP1onceec<-subset(accepteca,Q2.3==2)
QRP1onceppnec<-mean(QRP1onceec$QRP1,na.rm=TRUE)
QRP1occasionallyec<-subset(accepteca,Q2.3==3)
QRP1occasionallyppnec<-mean(QRP1occasionallyec$QRP1,na.rm=TRUE)
QRP1frequentlyec<-subset(accepteca,Q2.3==4)
QRP1frequentlyppnec<-mean(QRP1frequentlyec$QRP1,na.rm=TRUE)
QRP1alwaysec<-subset(accepteca,Q2.3==5)
QRP1alwaysppnec<-mean(QRP1alwaysec$QRP1,na.rm=TRUE)
QRP1acceptec<-rbind.data.frame(QRP1neverppnec,QRP1onceppnec,QRP1occasionallyppnec,QRP1frequentlyppnec,QRP1alwaysppnec)
QRP1acceptec$level<-c("never","once","occasionally","frequently","always")
QRP1acceptec$QRP<-rep("QRP1",nrow(QRP1acceptec))
colnames(QRP1acceptec)<-c("acceptability","level","QRP")

#acceptability QRP2
QRP2neverec<-subset(accepteca,Q3.3==1)
QRP2neverppnec<-mean(QRP2neverec$QRP1,na.rm=TRUE)
QRP2onceec<-subset(accepteca,Q3.3==2)
QRP2onceppnec<-mean(QRP2onceec$QRP2,na.rm=TRUE)
QRP2occasionallyec<-subset(accepteca,Q3.3==3)
QRP2occasionallyppnec<-mean(QRP2occasionallyec$QRP2,na.rm=TRUE)
QRP2frequentlyec<-subset(accepteca,Q3.3==4)
QRP2frequentlyppnec<-mean(QRP2frequentlyec$QRP2,na.rm=TRUE)
QRP2alwaysec<-subset(accepteca,Q3.3==5)
QRP2alwaysppnec<-mean(QRP2alwaysec$QRP2,na.rm=TRUE)
QRP2acceptec<-rbind.data.frame(QRP2neverppnec,QRP2onceppnec,QRP2occasionallyppnec,QRP2frequentlyppnec,QRP2alwaysppnec)
QRP2acceptec$level<-c("never","once","occasionally","frequently","always")
QRP2acceptec$QRP<-rep("QRP2",nrow(QRP2acceptec))
colnames(QRP2acceptec)<-c("acceptability","level","QRP")

#acceptability QRP3
QRP3neverec<-subset(accepteca,Q4.3==1)
QRP3neverppnec<-mean(QRP3neverec$QRP3,na.rm=TRUE)
QRP3onceec<-subset(accepteca,Q4.3==2)
QRP3onceppnec<-mean(QRP3onceec$QRP3,na.rm=TRUE)
QRP3occasionallyec<-subset(accepteca,Q4.3==3)
QRP3occasionallyppnec<-mean(QRP3occasionallyec$QRP3,na.rm=TRUE)
QRP3frequentlyec<-subset(accepteca,Q4.3==4)
QRP3frequentlyppnec<-mean(QRP3frequentlyec$QRP3,na.rm=TRUE)
QRP3alwaysec<-subset(accepteca,Q4.3==5)
QRP3alwaysppnec<-mean(QRP3alwaysec$QRP3,na.rm=TRUE)
QRP3acceptec<-rbind.data.frame(QRP3neverppnec,QRP3onceppnec,QRP3occasionallyppnec,QRP3frequentlyppnec,QRP3alwaysppnec)
QRP3acceptec$level<-c("never","once","occasionally","frequently","always")
QRP3acceptec$QRP<-rep("QRP3",nrow(QRP3acceptec))
colnames(QRP3acceptec)<-c("acceptability","level","QRP")

#acceptability QRP4
QRP4neverec<-subset(accepteca,Q5.3==1)
QRP4neverppnec<-mean(QRP4neverec$QRP4,na.rm=TRUE)
QRP4onceec<-subset(accepteca,Q5.3==2)
QRP4onceppnec<-mean(QRP4onceec$QRP4,na.rm=TRUE)
QRP4occasionallyec<-subset(accepteca,Q5.3==3)
QRP4occasionallyppnec<-mean(QRP4occasionallyec$QRP4,na.rm=TRUE)
QRP4frequentlyec<-subset(accepteca,Q5.3==4)
QRP4frequentlyppnec<-mean(QRP4frequentlyec$QRP4,na.rm=TRUE)
QRP4alwaysec<-subset(accepteca,Q5.3==5)
QRP4alwaysppnec<-mean(QRP4alwaysec$QRP4,na.rm=TRUE)
QRP4acceptec<-rbind.data.frame(QRP4neverppnec,QRP4onceppnec,QRP4occasionallyppnec,QRP4frequentlyppnec,QRP4alwaysppnec)
QRP4acceptec$level<-c("never","once","occasionally","frequently","always")
QRP4acceptec$QRP<-rep("QRP4",nrow(QRP4acceptec))
colnames(QRP4acceptec)<-c("acceptability","level","QRP")

#acceptability QRP5
QRP5neverec<-subset(accepteca,Q6.3==1)
QRP5neverppnec<-mean(QRP5neverec$QRP5,na.rm=TRUE)
QRP5onceec<-subset(accepteca,Q6.3==2)
QRP5onceppnec<-mean(QRP5onceec$QRP5,na.rm=TRUE)
QRP5occasionallyec<-subset(accepteca,Q6.3==3)
QRP5occasionallyppnec<-mean(QRP5occasionallyec$QRP5,na.rm=TRUE)
QRP5frequentlyec<-subset(accepteca,Q6.3==4)
QRP5frequentlyppnec<-mean(QRP5frequentlyec$QRP5,na.rm=TRUE)
QRP5alwaysec<-subset(accepteca,Q6.3==5)
QRP5alwaysppnec<-mean(QRP5alwaysec$QRP5,na.rm=TRUE)
QRP5acceptec<-rbind.data.frame(QRP5neverppnec,QRP5onceppnec,QRP5occasionallyppnec,QRP5frequentlyppnec,QRP5alwaysppnec)
QRP5acceptec$level<-c("never","once","occasionally","frequently","always")
QRP5acceptec$QRP<-rep("QRP5",nrow(QRP5acceptec))
colnames(QRP5acceptec)<-c("acceptability","level","QRP")

#acceptability QRP6
QRP6neverec<-subset(accepteca,Q7.3==1)
QRP6neverppnec<-mean(QRP6neverec$QRP6,na.rm=TRUE)
QRP6onceec<-subset(accepteca,Q7.3==2)
QRP6onceppnec<-mean(QRP6onceec$QRP6,na.rm=TRUE)
QRP6occasionallyec<-subset(accepteca,Q7.3==3)
QRP6occasionallyppnec<-mean(QRP6occasionallyec$QRP6,na.rm=TRUE)
QRP6frequentlyec<-subset(accepteca,Q7.3==4)
QRP6frequentlyppnec<-mean(QRP6frequentlyec$QRP6,na.rm=TRUE)
QRP6alwaysec<-subset(accepteca,Q7.3==5)
QRP6alwaysppnec<-mean(QRP6alwaysec$QRP6,na.rm=TRUE)
QRP6acceptec<-rbind.data.frame(QRP6neverppnec,QRP6onceppnec,QRP6occasionallyppnec,QRP6frequentlyppnec,QRP6alwaysppnec)
QRP6acceptec$level<-c("never","once","occasionally","frequently","always")
QRP6acceptec$QRP<-rep("QRP6",nrow(QRP6acceptec))
colnames(QRP6acceptec)<-c("acceptability","level","QRP")

#acceptability QRP7
QRP7neverec<-subset(accepteca,Q8.3==1)
QRP7neverppnec<-mean(QRP7neverec$QRP7,na.rm=TRUE)
QRP7onceec<-subset(accepteca,Q8.3==2)
QRP7onceppnec<-mean(QRP7onceec$QRP7,na.rm=TRUE)
QRP7occasionallyec<-subset(accepteca,Q8.3==3)
QRP7occasionallyppnec<-mean(QRP7occasionallyec$QRP7,na.rm=TRUE)
QRP7frequentlyec<-subset(accepteca,Q8.3==4)
QRP7frequentlyppnec<-mean(QRP7frequentlyec$QRP7,na.rm=TRUE)
QRP7alwaysec<-subset(accepteca,Q8.3==5)
QRP7alwaysppnec<-mean(QRP7alwaysec$QRP7,na.rm=TRUE)
QRP7acceptec<-rbind.data.frame(QRP7neverppnec,QRP7onceppnec,QRP7occasionallyppnec,QRP7frequentlyppnec,QRP7alwaysppnec)
QRP7acceptec$level<-c("never","once","occasionally","frequently","always")
QRP7acceptec$QRP<-rep("QRP7",nrow(QRP7acceptec))
colnames(QRP7acceptec)<-c("acceptability","level","QRP")

#acceptability QRP8
QRP8neverec<-subset(accepteca,Q9.3==1)
QRP8neverppnec<-mean(QRP8neverec$QRP8,na.rm=TRUE)
QRP8onceec<-subset(accepteca,Q9.3==2)
QRP8onceppnec<-mean(QRP8onceec$QRP8,na.rm=TRUE)
QRP8occasionallyec<-subset(accepteca,Q9.3==3)
QRP8occasionallyppnec<-mean(QRP8occasionallyec$QRP8,na.rm=TRUE)
QRP8frequentlyec<-subset(accepteca,Q9.3==4)
QRP8frequentlyppnec<-mean(QRP8frequentlyec$QRP8,na.rm=TRUE)
QRP8alwaysec<-subset(accepteca,Q9.3==5)
QRP8alwaysppnec<-mean(QRP8alwaysec$QRP8,na.rm=TRUE)
QRP8acceptec<-rbind.data.frame(QRP8neverppnec,QRP8onceppnec,QRP8occasionallyppnec,QRP8frequentlyppnec,QRP8alwaysppnec)
QRP8acceptec$level<-c("never","once","occasionally","frequently","always")
QRP8acceptec$QRP<-rep("QRP8",nrow(QRP8acceptec))
colnames(QRP8acceptec)<-c("acceptability","level","QRP")

#acceptability QRP9
QRP9neverec<-subset(accepteca,Q10.3==1)
QRP9neverppnec<-mean(QRP9neverec$QRP9,na.rm=TRUE)
QRP9onceec<-subset(accepteca,Q10.3==2)
QRP9onceppnec<-mean(QRP9onceec$QRP9,na.rm=TRUE)
QRP9occasionallyec<-subset(accepteca,Q10.3==3)
QRP9occasionallyppnec<-mean(QRP9occasionallyec$QRP9,na.rm=TRUE)
QRP9frequentlyec<-subset(accepteca,Q10.3==4)
QRP9frequentlyppnec<-mean(QRP9frequentlyec$QRP9,na.rm=TRUE)
QRP9alwaysec<-subset(accepteca,Q10.3==5)
QRP9alwaysppnec<-mean(QRP9alwaysec$QRP9,na.rm=TRUE)
QRP9acceptec<-rbind.data.frame(QRP9neverppnec,QRP9onceppnec,QRP9occasionallyppnec,QRP9frequentlyppnec,QRP9alwaysppnec)
QRP9acceptec$level<-c("never","once","occasionally","frequently","always")
QRP9acceptec$QRP<-rep("QRP9",nrow(QRP9acceptec))
colnames(QRP9acceptec)<-c("acceptability","level","QRP")

#acceptability QRP10
QRP10neverec<-subset(accepteca,Q11.3==1)
QRP10neverppnec<-mean(QRP10neverec$QRP10,na.rm=TRUE)
QRP10onceec<-subset(accepteca,Q11.3==2)
QRP10onceppnec<-mean(QRP10onceec$QRP10,na.rm=TRUE)
QRP10occasionallyec<-subset(accepteca,Q11.3==3)
QRP10occasionallyppnec<-mean(QRP10occasionallyec$QRP10,na.rm=TRUE)
QRP10frequentlyec<-subset(accepteca,Q11.3==4)
QRP10frequentlyppnec<-mean(QRP10frequentlyec$QRP10,na.rm=TRUE)
QRP10alwaysec<-subset(accepteca,Q11.3==5)
QRP10alwaysppnec<-mean(QRP10alwaysec$QRP10,na.rm=TRUE)
QRP10acceptec<-rbind.data.frame(QRP10neverppnec,QRP10onceppnec,QRP10occasionallyppnec,QRP10frequentlyppnec,QRP10alwaysppnec)
QRP10acceptec$level<-c("never","once","occasionally","frequently","always")
QRP10acceptec$QRP<-rep("QRP10",nrow(QRP10acceptec))
colnames(QRP10acceptec)<-c("acceptability","level","QRP")
QRP10acceptec[4,1]<-0
QRP10acceptec[5,1]<-0#there are no researchers who did this 'frequently' or 'always' so it came up with NAN but I want it to be a zero so that I can then multiply it by 0... and get 0

QRPneverec<-rbind.data.frame(QRP1neverppnec,QRP2neverppnec,QRP3neverppnec,QRP4neverppnec,QRP5neverppnec,QRP6neverppnec,QRP7neverppnec,QRP8neverppnec,QRP9neverppnec,QRP10neverppnec)
colnames(QRPneverec)<-"acceptable"
QRPonceec<-rbind.data.frame(QRP1onceppnec,QRP2onceppnec,QRP3onceppnec,QRP4onceppnec,QRP5onceppnec,QRP6onceppnec,QRP7onceppnec,QRP8onceppnec,QRP9onceppnec,QRP10onceppnec)
colnames(QRPonceec)<-"acceptable"
QRPoccasionallyec<-rbind.data.frame(QRP1occasionallyppnec,QRP2occasionallyppnec,QRP3occasionallyppnec,QRP4occasionallyppnec,QRP5occasionallyppnec,QRP6occasionallyppnec,QRP7occasionallyppnec,QRP8occasionallyppnec,QRP9occasionallyppnec,QRP10occasionallyppnec)
colnames(QRPoccasionallyec)<-"acceptable"
QRPfrequentlyec<-rbind.data.frame(QRP1frequentlyppnec,QRP2frequentlyppnec,QRP3frequentlyppnec,QRP4frequentlyppnec,QRP5frequentlyppnec,QRP6frequentlyppnec,QRP7frequentlyppnec,QRP8frequentlyppnec,QRP9frequentlyppnec,QRP10frequentlyppnec)
colnames(QRPfrequentlyec)<-"acceptable"
QRPfrequentlyec[10,1]<-0
QRPalmostalwaysec<-rbind.data.frame(QRP1alwaysppnec,QRP2alwaysppnec,QRP3alwaysppnec,QRP4alwaysppnec,QRP5alwaysppnec,QRP6alwaysppnec,QRP7alwaysppnec,QRP8alwaysppnec,QRP9alwaysppnec,QRP10alwaysppnec)
colnames(QRPalmostalwaysec)<-"acceptable"
QRPalmostalwaysec[10,1]<-0

#evolutionary biology
acceptev<-evbiologists1
acceptev$QRP1<-with(acceptev,ifelse(Q2.4 %in% c(1, 2),1,ifelse(Q2.4 %in% "NA",NA,0))) #Add a variable for QRP1 that codes "it should be used almost always" and "it should be used often" as 1- acceptable. Other categories are coded as unnacceptable
acceptev$QRP2<-with(acceptev,ifelse(Q3.4 %in% c(1, 2),1,ifelse(Q3.4 %in% "NA",NA,0)))
acceptev$QRP3<-with(acceptev,ifelse(Q4.4 %in% c(1, 2),1,ifelse(Q4.4 %in% "NA",NA,0)))
acceptev$QRP4<-with(acceptev,ifelse(Q5.4 %in% c(1, 2),1,ifelse(Q5.4 %in% "NA",NA,0)))
acceptev$QRP5<-with(acceptev,ifelse(Q6.4 %in% c(1, 2),1,ifelse(Q6.4 %in% "NA",NA,0)))
acceptev$QRP6<-with(acceptev,ifelse(Q7.4 %in% c(1, 2),1,ifelse(Q7.4 %in% "NA",NA,0)))
acceptev$QRP7<-with(acceptev,ifelse(Q8.4 %in% c(1, 2),1,ifelse(Q8.4 %in% "NA",NA,0)))
acceptev$QRP8<-with(acceptev,ifelse(Q9.4 %in% c(1, 2),1,ifelse(Q9.4 %in% "NA",NA,0)))
acceptev$QRP9<-with(acceptev,ifelse(Q10.4 %in% c(1, 2),1,ifelse(Q10.4 %in% "NA",NA,0)))
acceptev$QRP10<-with(acceptev,ifelse(Q11.4 %in% c(1, 2),1,ifelse(Q11.4 %in% "NA",NA,0)))
accepteva <- acceptev[c(10,15,20,25,30,35,40,45,50,55,75,76,77,78,79,80,81,82,83,84)] #get rid of all the extraneous columns

#acceptability QRP1
QRP1neverev<-subset(accepteva,Q2.3==1)
QRP1neverppnev<-mean(QRP1neverev$QRP1,na.rm=TRUE)
QRP1onceev<-subset(accepteva,Q2.3==2)
QRP1onceppnev<-mean(QRP1onceev$QRP1,na.rm=TRUE)
QRP1occasionallyev<-subset(accepteva,Q2.3==3)
QRP1occasionallyppnev<-mean(QRP1occasionallyev$QRP1,na.rm=TRUE)
QRP1frequentlyev<-subset(accepteva,Q2.3==4)
QRP1frequentlyppnev<-mean(QRP1frequentlyev$QRP1,na.rm=TRUE)
QRP1alwaysev<-subset(accepteva,Q2.3==5)
QRP1alwaysppnev<-mean(QRP1alwaysev$QRP1,na.rm=TRUE)
QRP1acceptev<-rbind.data.frame(QRP1neverppnev,QRP1onceppnev,QRP1occasionallyppnev,QRP1frequentlyppnev,QRP1alwaysppnev)
QRP1acceptev$level<-c("never","once","occasionally","frequently","always")
QRP1acceptev$QRP<-rep("QRP1",nrow(QRP1acceptev))
colnames(QRP1acceptev)<-c("acceptability","level","QRP")

#acceptability QRP2
QRP2neverev<-subset(accepteva,Q3.3==1)
QRP2neverppnev<-mean(QRP2neverev$QRP1,na.rm=TRUE)
QRP2onceev<-subset(accepteva,Q3.3==2)
QRP2onceppnev<-mean(QRP2onceev$QRP2,na.rm=TRUE)
QRP2occasionallyev<-subset(accepteva,Q3.3==3)
QRP2occasionallyppnev<-mean(QRP2occasionallyev$QRP2,na.rm=TRUE)
QRP2frequentlyev<-subset(accepteva,Q3.3==4)
QRP2frequentlyppnev<-mean(QRP2frequentlyev$QRP2,na.rm=TRUE)
QRP2alwaysev<-subset(accepteva,Q3.3==5)
QRP2alwaysppnev<-mean(QRP2alwaysev$QRP2,na.rm=TRUE)
QRP2acceptev<-rbind.data.frame(QRP2neverppnev,QRP2onceppnev,QRP2occasionallyppnev,QRP2frequentlyppnev,QRP2alwaysppnev)
QRP2acceptev$level<-c("never","once","occasionally","frequently","always")
QRP2acceptev$QRP<-rep("QRP2",nrow(QRP2acceptev))
colnames(QRP2acceptev)<-c("acceptability","level","QRP")

#acceptability QRP3
QRP3neverev<-subset(accepteva,Q4.3==1)
QRP3neverppnev<-mean(QRP3neverev$QRP3,na.rm=TRUE)
QRP3onceev<-subset(accepteva,Q4.3==2)
QRP3onceppnev<-mean(QRP3onceev$QRP3,na.rm=TRUE)
QRP3occasionallyev<-subset(accepteva,Q4.3==3)
QRP3occasionallyppnev<-mean(QRP3occasionallyev$QRP3,na.rm=TRUE)
QRP3frequentlyev<-subset(accepteva,Q4.3==4)
QRP3frequentlyppnev<-mean(QRP3frequentlyev$QRP3,na.rm=TRUE)
QRP3alwaysev<-subset(accepteva,Q4.3==5)
QRP3alwaysppnev<-mean(QRP3alwaysev$QRP3,na.rm=TRUE)
QRP3acceptev<-rbind.data.frame(QRP3neverppnev,QRP3onceppnev,QRP3occasionallyppnev,QRP3frequentlyppnev,QRP3alwaysppnev)
QRP3acceptev$level<-c("never","once","occasionally","frequently","always")
QRP3acceptev$QRP<-rep("QRP3",nrow(QRP3acceptev))
colnames(QRP3acceptev)<-c("acceptability","level","QRP")


#acceptability QRP4
QRP4neverev<-subset(accepteva,Q5.3==1)
QRP4neverppnev<-mean(QRP4neverev$QRP4,na.rm=TRUE)
QRP4onceev<-subset(accepteva,Q5.3==2)
QRP4onceppnev<-mean(QRP4onceev$QRP4,na.rm=TRUE)
QRP4occasionallyev<-subset(accepteva,Q5.3==3)
QRP4occasionallyppnev<-mean(QRP4occasionallyev$QRP4,na.rm=TRUE)
QRP4frequentlyev<-subset(accepteva,Q5.3==4)
QRP4frequentlyppnev<-mean(QRP4frequentlyev$QRP4,na.rm=TRUE)
QRP4alwaysev<-subset(accepteva,Q5.3==5)
QRP4alwaysppnev<-mean(QRP4alwaysev$QRP4,na.rm=TRUE)
QRP4acceptev<-rbind.data.frame(QRP4neverppnev,QRP4onceppnev,QRP4occasionallyppnev,QRP4frequentlyppnev,QRP4alwaysppnev)
QRP4acceptev$level<-c("never","once","occasionally","frequently","always")
QRP4acceptev$QRP<-rep("QRP4",nrow(QRP4acceptev))
colnames(QRP4acceptev)<-c("acceptability","level","QRP")

#acceptability QRP5
QRP5neverev<-subset(accepteva,Q6.3==1)
QRP5neverppnev<-mean(QRP5neverev$QRP5,na.rm=TRUE)
QRP5onceev<-subset(accepteva,Q6.3==2)
QRP5onceppnev<-mean(QRP5onceev$QRP5,na.rm=TRUE)
QRP5occasionallyev<-subset(accepteva,Q6.3==3)
QRP5occasionallyppnev<-mean(QRP5occasionallyev$QRP5,na.rm=TRUE)
QRP5frequentlyev<-subset(accepteva,Q6.3==4)
QRP5frequentlyppnev<-mean(QRP5frequentlyev$QRP5,na.rm=TRUE)
QRP5alwaysev<-subset(accepteva,Q6.3==5)
QRP5alwaysppnev<-mean(QRP5alwaysev$QRP5,na.rm=TRUE)
QRP5acceptev<-rbind.data.frame(QRP5neverppnev,QRP5onceppnev,QRP5occasionallyppnev,QRP5frequentlyppnev,QRP5alwaysppnev)
QRP5acceptev$level<-c("never","once","occasionally","frequently","always")
QRP5acceptev$QRP<-rep("QRP5",nrow(QRP5acceptev))
colnames(QRP5acceptev)<-c("acceptability","level","QRP")

#acceptability QRP6
QRP6neverev<-subset(accepteva,Q7.3==1)
QRP6neverppnev<-mean(QRP6neverev$QRP6,na.rm=TRUE)
QRP6onceev<-subset(accepteva,Q7.3==2)
QRP6onceppnev<-mean(QRP6onceev$QRP6,na.rm=TRUE)
QRP6occasionallyev<-subset(accepteva,Q7.3==3)
QRP6occasionallyppnev<-mean(QRP6occasionallyev$QRP6,na.rm=TRUE)
QRP6frequentlyev<-subset(accepteva,Q7.3==4)
QRP6frequentlyppnev<-mean(QRP6frequentlyev$QRP6,na.rm=TRUE)
QRP6alwaysev<-subset(accepteva,Q7.3==5)
QRP6alwaysppnev<-mean(QRP6alwaysev$QRP6,na.rm=TRUE)
QRP6acceptev<-rbind.data.frame(QRP6neverppnev,QRP6onceppnev,QRP6occasionallyppnev,QRP6frequentlyppnev,QRP6alwaysppnev)
QRP6acceptev$level<-c("never","once","occasionally","frequently","always")
QRP6acceptev$QRP<-rep("QRP6",nrow(QRP6acceptev))
colnames(QRP6acceptev)<-c("acceptability","level","QRP")
QRP6acceptev[5,1]<-0

#acceptability QRP7
QRP7neverev<-subset(accepteva,Q8.3==1)
QRP7neverppnev<-mean(QRP7neverev$QRP7,na.rm=TRUE)
QRP7onceev<-subset(accepteva,Q8.3==2)
QRP7onceppnev<-mean(QRP7onceev$QRP7,na.rm=TRUE)
QRP7occasionallyev<-subset(accepteva,Q8.3==3)
QRP7occasionallyppnev<-mean(QRP7occasionallyev$QRP7,na.rm=TRUE)
QRP7frequentlyev<-subset(accepteva,Q8.3==4)
QRP7frequentlyppnev<-mean(QRP7frequentlyev$QRP7,na.rm=TRUE)
QRP7alwaysev<-subset(accepteva,Q8.3==5)
QRP7alwaysppnev<-mean(QRP7alwaysev$QRP7,na.rm=TRUE)
QRP7acceptev<-rbind.data.frame(QRP7neverppnev,QRP7onceppnev,QRP7occasionallyppnev,QRP7frequentlyppnev,QRP7alwaysppnev)
QRP7acceptev$level<-c("never","once","occasionally","frequently","always")
QRP7acceptev$QRP<-rep("QRP7",nrow(QRP7acceptev))
colnames(QRP7acceptev)<-c("acceptability","level","QRP")

#acceptability QRP8
QRP8neverev<-subset(accepteva,Q9.3==1)
QRP8neverppnev<-mean(QRP8neverev$QRP8,na.rm=TRUE)
QRP8onceev<-subset(accepteva,Q9.3==2)
QRP8onceppnev<-mean(QRP8onceev$QRP8,na.rm=TRUE)
QRP8occasionallyev<-subset(accepteva,Q9.3==3)
QRP8occasionallyppnev<-mean(QRP8occasionallyev$QRP8,na.rm=TRUE)
QRP8frequentlyev<-subset(accepteva,Q9.3==4)
QRP8frequentlyppnev<-mean(QRP8frequentlyev$QRP8,na.rm=TRUE)
QRP8alwaysev<-subset(accepteva,Q9.3==5)
QRP8alwaysppnev<-mean(QRP8alwaysev$QRP8,na.rm=TRUE)
QRP8acceptev<-rbind.data.frame(QRP8neverppnev,QRP8onceppnev,QRP8occasionallyppnev,QRP8frequentlyppnev,QRP8alwaysppnev)
QRP8acceptev$level<-c("never","once","occasionally","frequently","always")
QRP8acceptev$QRP<-rep("QRP8",nrow(QRP8acceptev))
colnames(QRP8acceptev)<-c("acceptability","level","QRP")
QRP8acceptev[5,1]<-0

#acceptability QRP9
QRP9neverev<-subset(accepteva,Q10.3==1)
QRP9neverppnev<-mean(QRP9neverev$QRP9,na.rm=TRUE)
QRP9onceev<-subset(accepteva,Q10.3==2)
QRP9onceppnev<-mean(QRP9onceev$QRP9,na.rm=TRUE)
QRP9occasionallyev<-subset(accepteva,Q10.3==3)
QRP9occasionallyppnev<-mean(QRP9occasionallyev$QRP9,na.rm=TRUE)
QRP9frequentlyev<-subset(accepteva,Q10.3==4)
QRP9frequentlyppnev<-mean(QRP9frequentlyev$QRP9,na.rm=TRUE)
QRP9alwaysev<-subset(accepteva,Q10.3==5)
QRP9alwaysppnev<-mean(QRP9alwaysev$QRP9,na.rm=TRUE)
QRP9acceptev<-rbind.data.frame(QRP9neverppnev,QRP9onceppnev,QRP9occasionallyppnev,QRP9frequentlyppnev,QRP9alwaysppnev)
QRP9acceptev$level<-c("never","once","occasionally","frequently","always")
QRP9acceptev$QRP<-rep("QRP9",nrow(QRP9acceptev))
colnames(QRP9acceptev)<-c("acceptability","level","QRP")
QRP9acceptev[5,1]<-0

#acceptability QRP10
QRP10neverev<-subset(accepteva,Q11.3==1)
QRP10neverppnev<-mean(QRP10neverev$QRP10,na.rm=TRUE)
QRP10onceev<-subset(accepteva,Q11.3==2)
QRP10onceppnev<-mean(QRP10onceev$QRP10,na.rm=TRUE)
QRP10occasionallyev<-subset(accepteva,Q11.3==3)
QRP10occasionallyppnev<-mean(QRP10occasionallyev$QRP10,na.rm=TRUE)
QRP10frequentlyev<-subset(accepteva,Q11.3==4)
QRP10frequentlyppnev<-mean(QRP10frequentlyev$QRP10,na.rm=TRUE)
QRP10alwaysev<-subset(accepteva,Q11.3==5)
QRP10alwaysppnev<-mean(QRP10alwaysev$QRP10,na.rm=TRUE)
QRP10acceptev<-rbind.data.frame(QRP10neverppnev,QRP10onceppnev,QRP10occasionallyppnev,QRP10frequentlyppnev,QRP10alwaysppnev)
QRP10acceptev$level<-c("never","once","occasionally","frequently","always")
QRP10acceptev$QRP<-rep("QRP10",nrow(QRP10acceptev))
colnames(QRP10acceptev)<-c("acceptability","level","QRP")
QRP10acceptev[5,1]<-0

QRPneverev<-rbind.data.frame(QRP1neverppnev,QRP2neverppnev,QRP3neverppnev,QRP4neverppnev,QRP5neverppnev,QRP6neverppnev,QRP7neverppnev,QRP8neverppnev,QRP9neverppnev,QRP10neverppnev)
colnames(QRPneverev)<-"acceptable"
QRPonceev<-rbind.data.frame(QRP1onceppnev,QRP2onceppnev,QRP3onceppnev,QRP4onceppnev,QRP5onceppnev,QRP6onceppnev,QRP7onceppnev,QRP8onceppnev,QRP9onceppnev,QRP10onceppnev)
colnames(QRPonceev)<-"acceptable"
QRPoccasionallyev<-rbind.data.frame(QRP1occasionallyppnev,QRP2occasionallyppnev,QRP3occasionallyppnev,QRP4occasionallyppnev,QRP5occasionallyppnev,QRP6occasionallyppnev,QRP7occasionallyppnev,QRP8occasionallyppnev,QRP9occasionallyppnev,QRP10occasionallyppnev)
colnames(QRPoccasionallyev)<-"acceptable"
QRPfrequentlyev<-rbind.data.frame(QRP1frequentlyppnev,QRP2frequentlyppnev,QRP3frequentlyppnev,QRP4frequentlyppnev,QRP5frequentlyppnev,QRP6frequentlyppnev,QRP7frequentlyppnev,QRP8frequentlyppnev,QRP9frequentlyppnev,QRP10frequentlyppnev)
colnames(QRPfrequentlyev)<-"acceptable"
QRPalmostalwaysev<-rbind.data.frame(QRP1alwaysppnev,QRP2alwaysppnev,QRP3alwaysppnev,QRP4alwaysppnev,QRP5alwaysppnev,QRP6alwaysppnev,QRP7alwaysppnev,QRP8alwaysppnev,QRP9alwaysppnev,QRP10alwaysppnev)
colnames(QRPalmostalwaysev)<-"acceptable"
QRPalmostalwaysev[c(6,8,9,10),1]<-0 #sub 0s in for NaNs

#plotting data
question <- c("QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")

#ecology
neverdataec<-cbind(question, ppn1ec,lowerci1ec,upperci1ec)
neverdataec$Rank<-rep("never",nrow(neverdataec))
colnames(neverdataec)<-c("question","ppnused","lcippnused","hcippnused","rank")
neverdataec<-cbind.data.frame(neverdataec,QRPneverec)
neverdataec$ppnacceptable<-as.numeric(as.character(neverdataec$ppnused))*neverdataec$acceptable

oncedataec<-cbind(question, ppn2ec,lowerci2ec,upperci2ec)
oncedataec$Rank<-rep("once",nrow(oncedataec))
colnames(oncedataec)<-c("question","ppnused","lcippnused","hcippnused","rank")
oncedataec<-cbind.data.frame(oncedataec,QRPonceec)
oncedataec$ppnacceptable<-as.numeric(as.character(oncedataec$ppnused))*oncedataec$acceptable

occasionallydataec<-cbind(question, ppn3ec,lowerci3ec,upperci3ec)
occasionallydataec$Rank<-rep("occasionally",nrow(occasionallydataec))
colnames(occasionallydataec)<-c("question","ppnused","lcippnused","hcippnused","rank")
occasionallydataec<-cbind.data.frame(occasionallydataec,QRPoccasionallyec)
occasionallydataec$ppnacceptable<-as.numeric(as.character(occasionallydataec$ppnused*occasionallydataec$acceptable))

frequentlydataec<-cbind(question, ppn4ec,lowerci4ec,upperci4ec)
frequentlydataec$Rank<-rep("frequently",nrow(frequentlydataec))
colnames(frequentlydataec)<-c("question","ppnused","lcippnused","hcippnused","rank")
frequentlydataec<-cbind.data.frame(frequentlydataec,QRPfrequentlyec)
frequentlydataec[10,6]<-0 #there is still a NaN in there- change to 0
frequentlydataec$ppnacceptable<-as.numeric(as.character(frequentlydataec$ppnused))*frequentlydataec$acceptable


almostalwaysdataec<-cbind(question, ppn5ec,lowerci5ec,upperci5ec)
almostalwaysdataec$Rank<-rep("almost.always",nrow(almostalwaysdataec))
colnames(almostalwaysdataec)<-c("question","ppnused","lcippnused","hcippnused","rank")
almostalwaysdataec<-cbind.data.frame(almostalwaysdataec,QRPalmostalwaysec)
almostalwaysdataec$ppnacceptable<-as.numeric(as.character(almostalwaysdataec$ppnused))*almostalwaysdataec$acceptable

#evolutionary biology
neverdataev<-cbind(question, ppn1ev,lowerci1ev,upperci1ev)
neverdataev$Rank<-rep("never",nrow(neverdataev))
colnames(neverdataev)<-c("question","ppnused","lcippnused","hcippnused","rank")
neverdataev<-cbind.data.frame(neverdataev,QRPneverev)
neverdataev$ppnacceptable<-as.numeric(as.character(neverdataev$ppnused))*neverdataev$acceptable

oncedataev<-cbind(question, ppn2ev,lowerci2ev,upperci2ev)
oncedataev$Rank<-rep("once",nrow(oncedataev))
colnames(oncedataev)<-c("question","ppnused","lcippnused","hcippnused","rank")
oncedataev<-cbind.data.frame(oncedataev,QRPonceev)
oncedataev$ppnacceptable<-as.numeric(as.character(oncedataev$ppnused))*oncedataev$acceptable

occasionallydataev<-cbind(question, ppn3ev,lowerci3ev,upperci3ev)
occasionallydataev$Rank<-rep("occasionally",nrow(occasionallydataev))
colnames(occasionallydataev)<-c("question","ppnused","lcippnused","hcippnused","rank")
occasionallydataev<-cbind.data.frame(occasionallydataev,QRPoccasionallyev)
occasionallydataev$ppnacceptable<-as.numeric(as.character(occasionallydataev$ppnused*occasionallydataev$acceptable))

frequentlydataev<-cbind(question, ppn4ev,lowerci4ev,upperci4ev)
frequentlydataev$Rank<-rep("frequently",nrow(frequentlydataev))
colnames(frequentlydataev)<-c("question","ppnused","lcippnused","hcippnused","rank")
frequentlydataev<-cbind.data.frame(frequentlydataev,QRPfrequentlyev)
frequentlydataev$ppnacceptable<-as.numeric(as.character(frequentlydataev$ppnused))*frequentlydataev$acceptable


almostalwaysdataev<-cbind(question, ppn5ev,lowerci5ev,upperci5ev)
almostalwaysdataev$Rank<-rep("almost.always",nrow(almostalwaysdataev))
colnames(almostalwaysdataev)<-c("question","ppnused","lcippnused","hcippnused","rank")
almostalwaysdataev<-cbind.data.frame(almostalwaysdataev,QRPalmostalwaysev)
almostalwaysdataev$ppnacceptable<-as.numeric(as.character(almostalwaysdataev$ppnused))*almostalwaysdataev$acceptable


#ecology dataframe
QRPDATAec<-as.data.frame(rbind(neverdataec,oncedataec,occasionallydataec,frequentlydataec,almostalwaysdataec))
QRPDATAaec<-QRPDATAec
QRPDATAec$ppnused<-as.numeric(as.character(QRPDATAec$ppnused))
QRPDATAec$lcippnused<-as.numeric(as.character(QRPDATAec$lcippnused))
QRPDATAec$hcippnused<-as.numeric(as.character(QRPDATAec$hcippnused))
QRPDATAec$rank<-as.factor(QRPDATAec$rank)
QRPDATAec$rank<-factor(QRPDATAec$rank, c("never", "once", "occasionally", "frequently","almost.always"))      
QRPDATAec$field<-rep("ecology",nrow(QRPDATAec))

#evolutionary biology dataframe
QRPDATAev<-as.data.frame(rbind(neverdataev,oncedataev,occasionallydataev,frequentlydataev,almostalwaysdataev))
QRPDATAaev<-QRPDATAev
QRPDATAev$ppnused<-as.numeric(as.character(QRPDATAev$ppnused))
QRPDATAev$lcippnused<-as.numeric(as.character(QRPDATAev$lcippnused))
QRPDATAev$hcippnused<-as.numeric(as.character(QRPDATAev$hcippnused))
QRPDATAev$rank<-as.factor(QRPDATAev$rank)
QRPDATAev$rank<-factor(QRPDATAev$rank, c("never", "once", "occasionally", "frequently","almost.always"))  
QRPDATAev$field<-rep("evolution",nrow(QRPDATAev))

#combine datasets
QRPDATA<-rbind.data.frame(QRPDATAec,QRPDATAev)

#JOHN et al 2012 data
JohnQ1<-c(((100-63.4)/100),NA,NA,NA,NA)
JohnQ7<-c(((100-55.9)/100),NA,NA,NA,NA)
JohnQ5<-c(((100-22)/100),NA,NA,NA,NA)
JohnQ6<-c(((100-38.2)/100),NA,NA,NA,NA)
JohnQ3<-c(((100-27)/100),NA,NA,NA,NA)

#Agnoli data
AgnoliQ1<-c(((100-47.9)/100),NA,NA,NA,NA)
AgnoliQ7<-c(((100-53.2)/100),NA,NA,NA,NA)
AgnoliQ5<-c(((100-22)/100),NA,NA,NA,NA)
AgnoliQ6<-c(((100-39.7)/100),NA,NA,NA,NA)
AgnoliQ3<-c(((100-37.4)/100),NA,NA,NA,NA)


rank<-c("never","once","occasionally","frequently","almost.always")
JohnData<-cbind.data.frame(rank,JohnQ1,JohnQ3,JohnQ5,JohnQ6,JohnQ7)
AgnoliData<-cbind.data.frame(rank,AgnoliQ1,AgnoliQ3,AgnoliQ5,AgnoliQ6,AgnoliQ7)
#QRP plots

#QRP1
QRP1<-subset(QRPDATA, question == "QRP01")

QRP1graph<-ggplot() +
  geom_col(data=QRP1, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP1, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP1,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("1) unreported variables")#+
 # geom_point(data=JohnData, aes(x=rank, y=JohnQ1),size=6,colour="red")+
 # geom_point(data=AgnoliData, aes(x=rank, y=AgnoliQ1),size=6,shape=4)


#QRP2
QRP2<-subset(QRPDATA, question == "QRP02")

QRP2graph<-ggplot() +
  geom_col(data=QRP2, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP2, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP2,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("2) unreported covariates")


#QRP3
QRP3<-subset(QRPDATA, question == "QRP03")

QRP3graph<-ggplot() +
  geom_col(data=QRP3, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP3, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP3,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("3) HARKing")#+
#geom_point(data=JohnData, aes(x=rank, y=JohnQ3),size=6,colour="red")+
#  geom_point(data=AgnoliData, aes(x=rank, y=AgnoliQ3),size=6,shape=4)


#QRP4
QRP4<-subset(QRPDATA, question == "QRP04")

QRP4graph<-ggplot() +
  geom_col(data=QRP4, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP4, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP4,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("4) unreported models")

#QRP5
QRP5<-subset(QRPDATA, question == "QRP05")

QRP5graph<-ggplot() +
  geom_col(data=QRP5, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP5, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP5,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("5) rounding ps")#+
#  geom_point(data=JohnData, aes(x=rank, y=JohnQ5),size=6,colour="red")+
#  geom_point(data=AgnoliData, aes(x=rank, y=AgnoliQ5),size=6,shape=4)
#to get paper grapsh remove dot, change ylab and change legend position to none


#QRP6
QRP6<-subset(QRPDATA, question == "QRP06")

QRP6graph<-ggplot() +
  geom_col(data=QRP6, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP6, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP6,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("6) excluding data")#+
#  geom_point(data=JohnData, aes(x=rank, y=JohnQ6),size=6,colour="red")+
#  geom_point(data=AgnoliData, aes(x=rank, y=AgnoliQ6),size=6,shape=4)


#QRP7
QRP7<-subset(QRPDATA, question == "QRP07")

QRP7graph<-ggplot() +
  geom_col(data=QRP7, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP7, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP7,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("7) adding data")#+
#  geom_point(data=JohnData, aes(x=rank, y=JohnQ7),size=6,colour="red")+
#  geom_point(data=AgnoliData, aes(x=rank, y=AgnoliQ7),size=6,shape=4)



#QRP8
QRP8<-subset(QRPDATA, question == "QRP08")

QRP8graph<-ggplot() +
  geom_col(data=QRP8, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP8, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP8,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("8) switching analyses")


#QRP9
QRP9<-subset(QRPDATA, question == "QRP09")

QRP9graph<-ggplot() +
  geom_col(data=QRP9, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP9, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP9,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("9) undisclosed problems")

#QRP10
QRP10<-subset(QRPDATA, question == "QRP10")

QRP10graph<-ggplot() +
  geom_col(data=QRP10, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP10, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP10,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="none")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))+theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm"))+ ggtitle("10) fabrication")

legendgraph<-ggplot() +
  geom_col(data=QRP10, aes(rank, ppnused, group = field, fill = factor(field),colour="black"),position = "dodge")+
  geom_col(data=QRP10, aes(rank, ppnacceptable, group = field,fill="find acceptable",colour="black"),position = "dodge")+
  geom_errorbar(data=QRP10,aes(x=rank,ymin=lcippnused, ymax=hcippnused,group=field),width=.2,position=position_dodge(.9))+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("")+xlab("")+ylim(0,1)+scale_fill_manual(values = c("#21507a","#337AB8","#A1C9ED"))+scale_color_manual(values=c("black","black"))+ theme(legend.position="bottom")+ guides(fill=guide_legend(title=NULL))+ guides(colour=FALSE)+
  theme(text=element_text(size=12))
legend <- get_legend(legendgraph)

plot_grid(QRP1graph, QRP2graph,QRP3graph,QRP4graph,QRP5graph,QRP6graph,QRP7graph,QRP8graph,QRP9graph,QRP10graph,legend,ncol=2)



#Checking for Kendall Tau Correlations
#prevalence and acceptability - for all QRPs, both fields together
combinedprev <- combined[c(2,10,15,20,25,30,35,40,45,50,55)] 
colnames(combinedprev)<-c("ID","QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")
longprevalence <- gather(combinedprev, question, prevalence,-ID)

combinedaccept <- combined[c(2,11,16,21,26,31,36,41,46,51,56)] 
colnames(combinedaccept)<-c("ID","QRP01","QRP02","QRP03","QRP04","QRP05","QRP06","QRP07","QRP08","QRP09","QRP10")
longaccept <- gather(combinedaccept, question, acceptability,-ID)
longaccept$codedacceptability<-5-as.numeric(as.character(longaccept$acceptability)) 
#the acceptability variable is currently coded 1 "It should be used almost always", 2"It should be used often", 3 "It should only be used rarely", 4 "It should never be used".... in order to match up with prevalence (1 never, 2 once, 3 occassionally, 4 frequently, 5 almost always), the order needs to be reversed
prevaccept<-merge(longprevalence,longaccept) #join the datasets back together
prevaccept$prevalence<-as.numeric(as.character(prevaccept$prevalence)) 
cor(prevaccept$prevalence,prevaccept$codedacceptability, method="kendall", use="pairwise")  #test for Kendalls tau correlation between how often someone uses QRPs and how acceptable they find them
#result is 0.613

#prevalence and age and seniority
#for all questions together- this has the problem that each person with a specific age completed a number (often all) of the questions about QRPs
ageseniority<-combined[c(2,68,71)] 
colnames(ageseniority)<-c("ID","seniority","age")
tauprevagesen<-merge(prevaccept,ageseniority)
tauprevagesen$age<-as.numeric(as.character(tauprevagesen$age)) 
tauprevagesen$seniority<-as.numeric(as.character(tauprevagesen$seniority)) 

cor(tauprevagesen$prevalence,tauprevagesen$age, method="kendall", use="pairwise")  #test for Kendalls tau correlation between age and prevalence
#result is 0.046

cor(tauprevagesen$prevalence,tauprevagesen$seniority, method="kendall", use="pairwise")  #test for Kendalls tau correlation between seniority and prevalence
#result is 0.035

##Demographics
#number of ecologists 
nrow(ecologists) #answer 493

#number of evolutionary biologists
nrow(evbiologists) #answer 313

#Gender breakdown
summary(as.factor(combined[,69]))
#number of males = 387
#number of females = 164
#number non-binary/third gender and prefer not to say= 9
#NAs = 311


#seniority breakdown
summary(as.factor(combined[,68])) #1=graduate student 34, 2=post-doctoral researcher 182, 3=mid-career researcher 134, 4=senior research fellow 208, NAs 314

#age breakdown
summary(as.factor(combined[,71])) #1=under 20:1, 2=20-29:63, 3=30-39:261, 4=40-49:145, 5=50-59:55, 6=60-69:27, 7=70+:7, NAs 313


##Researcher Integrity: Have you even had doubts about the scientific integrity of research in ecology##
##QRPs

#never
neverquestionableotherin<-subset(combined,  combined[,58]== 1)
kn1<-nrow(neverquestionableotherin)
neverquestionableyourin<-subset(combined,  combined[,59]== 1)
kn2<-nrow(neverquestionableyourin)
neverquestionablegradstu<-subset(combined,  combined[,60]== 1)
kn3<-nrow(neverquestionablegradstu)
neverquestionablesenior<-subset(combined,  combined[,61]== 1)
kn4<-nrow(neverquestionablesenior)
neverquestionableown<-subset(combined,  combined[,62]== 1)
kn5<-nrow(neverquestionableown)


#once or twice
oncequestionableotherin<-subset(combined,  combined[,58]== 2)
kot1<-nrow(oncequestionableotherin)
oncequestionableyourin<-subset(combined,  combined[,59]== 2)
kot2<-nrow(oncequestionableyourin)
oncequestionablegradstu<-subset(combined,  combined[,60]== 2)
kot3<-nrow(oncequestionablegradstu)
oncequestionablesenior<-subset(combined,  combined[,61]== 2)
kot4<-nrow(oncequestionablesenior)
oncequestionableown<-subset(combined,  combined[,62]== 2)
kot5<-nrow(oncequestionableown)


#often
oftenquestionableotherin<-subset(combined,  combined[,58]== 3)
ko1<-nrow(oftenquestionableotherin)
oftenquestionableyourin<-subset(combined,  combined[,59]== 3)
ko2<-nrow(oftenquestionableyourin)
oftenquestionablegradstu<-subset(combined,  combined[,60]== 3)
ko3<-nrow(oftenquestionablegradstu)
oftenquestionablesenior<-subset(combined,  combined[,61]== 3)
ko4<-nrow(oftenquestionablesenior)
oftenquestionableown<-subset(combined,  combined[,62]== 3)
ko5<-nrow(oftenquestionableown)

n1<-kn1+kot1+ko1
n2<-kn2+kot2+ko2
n3<-kn3+kot3+ko3
n4<-kn4+kot4+ko4
n5<-kn5+kot5+ko5

bn1<-binom.confint(kn1,n1 , method=c("wilson"),type="central")
bn2<-binom.confint(kn2,n2 , method=c("wilson"),type="central")
bn3<-binom.confint(kn3,n3 , method=c("wilson"),type="central")
bn4<-binom.confint(kn4,n4 , method=c("wilson"),type="central")
bn5<-binom.confint(kn5,n5 , method=c("wilson"),type="central")
bot1<-binom.confint(kot1,n1 , method=c("wilson"),type="central")
bot2<-binom.confint(kot2,n2 , method=c("wilson"),type="central")
bot3<-binom.confint(kot3,n3 , method=c("wilson"),type="central")
bot4<-binom.confint(kot4,n4 , method=c("wilson"),type="central")
bot5<-binom.confint(kot5,n5 , method=c("wilson"),type="central")
bo1<-binom.confint(ko1,n1 , method=c("wilson"),type="central")
bo2<-binom.confint(ko2,n2 , method=c("wilson"),type="central")
bo3<-binom.confint(ko3,n3 , method=c("wilson"),type="central")
bo4<-binom.confint(ko4,n4 , method=c("wilson"),type="central")
bo5<-binom.confint(ko5,n5 , method=c("wilson"),type="central")

##Misconduct
nevermisconductotherin<-subset(combined,  combined[,63]== 1)
knm1<-nrow(nevermisconductotherin)
nevermisconductyourin<-subset(combined,  combined[,64]== 1)
knm2<-nrow(nevermisconductyourin)
nevermisconductgradstu<-subset(combined,  combined[,65]== 1)
knm3<-nrow(nevermisconductgradstu)
nevermisconductsenior<-subset(combined,  combined[,66]== 1)
knm4<-nrow(nevermisconductsenior)
nevermisconductown<-subset(combined,  combined[,67]== 1)
knm5<-nrow(nevermisconductown)

#once or twice
oncemisconductotherin<-subset(combined,  combined[,63]== 2)
kotm1<-nrow(oncemisconductotherin)
oncemisconductyourin<-subset(combined,  combined[,64]== 2)
kotm2<-nrow(oncemisconductyourin)
oncemisconductgradstu<-subset(combined,  combined[,65]== 2)
kotm3<-nrow(oncemisconductgradstu)
oncemisconductsenior<-subset(combined,  combined[,66]== 2)
kotm4<-nrow(oncemisconductsenior)
oncemisconductown<-subset(combined,  combined[,67]== 2)
kotm5<-nrow(oncemisconductown)

#often
oftenmisconductotherin<-subset(combined,  combined[,63]== 3)
kom1<-nrow(oftenmisconductotherin)
oftenmisconductyourin<-subset(combined,  combined[,64]== 3)
kom2<-nrow(oftenmisconductyourin)
oftenmisconductgradstu<-subset(combined,  combined[,65]== 3)
kom3<-nrow(oftenmisconductgradstu)
oftenmisconductsenior<-subset(combined,  combined[,66]== 3)
kom4<-nrow(oftenmisconductsenior)
oftenmisconductown<-subset(combined,  combined[,67]== 3)
kom5<-nrow(oftenmisconductown)



nm1<-knm1+kotm1+kom1
nm2<-knm2+kotm2+kom2
nm3<-knm3+kotm3+kom3
nm4<-knm4+kotm4+kom4
nm5<-knm5+kotm5+kom5

bnm1<-binom.confint(knm1,nm1 , method=c("wilson"),type="central")
bnm2<-binom.confint(knm2,nm2 , method=c("wilson"),type="central")
bnm3<-binom.confint(knm3,nm3 , method=c("wilson"),type="central")
bnm4<-binom.confint(knm4,nm4 , method=c("wilson"),type="central")
bnm5<-binom.confint(knm5,nm5 , method=c("wilson"),type="central")
bomt1<-binom.confint(kotm1,nm1 , method=c("wilson"),type="central")
bomt2<-binom.confint(kotm2,nm2 , method=c("wilson"),type="central")
bomt3<-binom.confint(kotm3,nm3 , method=c("wilson"),type="central")
bomt4<-binom.confint(kotm4,nm4 , method=c("wilson"),type="central")
bomt5<-binom.confint(kotm5,nm5 , method=c("wilson"),type="central")
bom1<-binom.confint(kom1,nm1 , method=c("wilson"),type="central")
bom2<-binom.confint(kom2,nm2 , method=c("wilson"),type="central")
bom3<-binom.confint(kom3,nm3 , method=c("wilson"),type="central")
bom4<-binom.confint(kom4,nm4 , method=c("wilson"),type="central")
bom5<-binom.confint(kom5,nm5 , method=c("wilson"),type="central")

