.libPaths("C:/R-3.3.2/library")
#This is the proceedure use to take the raw data files (that are stored privately on the OSF) and process them in a way that allows all of the data to be publically available without revealing any identifying information about who took part in which practices.
#We are unable to share the raw data except for verification and fraud detection purposes


Ecology<-read.csv("./EcologyResponses.csv",na.strings="",stringsAsFactors = FALSE) #load in the dataset gathered from the Ecology journals
EvBiol<-read.csv("./EvBiolResponses.csv",na.strings="",stringsAsFactors = FALSE) #load in the dataset gathered from the evolutionary biology journals
combined1<-rbind(Ecology,EvBiol) #join these two datasets together
combined<-combined1[-1, ] #remove the first row that has the full text of each question written in it
qualitative<-combined[c(11,16,21,26,31,36,41,46,51,56,73)] #take all of the columns with qualitative data
combined[c(11,16,21,26,31,36,41,46,51,56,73)]<-rep("Masked",nrow(combined)) #mask all of the qualitative response data for the publically available version of the data used for data analysis
write.csv(combined,"./combined.csv") #create this file


#reordering qualitative data to protect participants identities. 
#Reading through the data it seems unlikely that any participant could be identified from a single comment but it might be possible to identify someone based on comments to a series of questions. By reordering the qualitative responses and disconnecting them from the answers in the rest of the survey, we are able to protect people's anonymity and (even if it is possible to recognise someone from a response) it will be impossible to tell how they answered the questions in the rest of the survey. 

QRP1<-sort(qualitative[,1],decreasing=F,na.last=T)
QRP2<-sort(qualitative[,2],decreasing=F,na.last=T)
QRP3<-sort(qualitative[,3],decreasing=F,na.last=T)
QRP4<-sort(qualitative[,4],decreasing=F,na.last=T)
QRP5<-sort(qualitative[,5],decreasing=F,na.last=T)
QRP6<-sort(qualitative[,6],decreasing=F,na.last=T)
QRP7<-sort(qualitative[,7],decreasing=F,na.last=T)
QRP8<-sort(qualitative[,8],decreasing=F,na.last=T)
QRP9<-sort(qualitative[,9],decreasing=F,na.last=T)
QRP10<-sort(qualitative[,10],decreasing=F,na.last=T)
QRPgeneral<-sort(qualitative[,11],decreasing=F,na.last=T)

ReorderedQualtitativeData<-cbind.data.frame(QRP1,QRP2,QRP3,QRP4,QRP5,QRP6,QRP7,QRP8,QRP9,QRP10,QRPgeneral) #join reordered qualitative responses back together as a dataframe
names[ReorderedQualtitativeData]<-c("QRP1","QRP2","QRP3","QRP4","QRP5","QRP6","QRP7","QRP8","QRP9","QRP10","QRPgeneral") #rename the variables something more informative
write.csv(ReorderedQualtitativeData,"./ReorderedQualtitativeData.csv") #create this file
