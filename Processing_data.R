## Script for processing survey data##
library(dplyr)
library(readxl)
library(ggplot2)
library(rmarkdown)
data<-read_xlsx("/home/shervin/Documents/GitStuff/Uppsala_MTfacestudy/MTdata_workfile.xlsx") #read file
#filter out responses taking more than 5 minutes
data<-data %>% filter(600>data$`Duration (in seconds)`) 
#filter out people who have done the survey before
data<-data %>%filter(grepl('No',`Have you taken part of this survey before?`))
#count participants per condition 
sum(!is.na(data$Pic1_2))
sum(!is.na(data$Pic2_2))
sum(!is.na(data$Pic3_2))
sum(!is.na(data$Pic4_2))
sum(!is.na(data$Pic5_2))
sum(!is.na(data$Pic6_2))
sum(!is.na(data$Pic7_2))
sum(!is.na(data$Pic8_2))
sum(!is.na(data$Pic9_2))
sum(!is.na(data$Pic10_2))
sum(!is.na(data$Pic11_2))
sum(!is.na(data$Pic12_2)) 


## Organized original xlsx format to long format in a new sheet. 
dt<-read_xlsx("/home/shervin/Documents/GitStuff/Uppsala_MTfacestudy/MTdata_workfile.xlsx", sheet="Organized")

#Exclude responses taking more than 10 minutes (as per instructions)
dt<-dt %>% filter(600>dt$`Duration (in seconds)`) 
#Exclude responses that are not "No" to if they have done the study before
dt<-dt %>%filter(grepl('No',`survey_before`))
#Exclude responses with fraud score above threshold >=30
dt <- dt %>% filter(30>=dt$Q_RelevantIDFraudScore)
#Exclude responses with ID duplicate =true (likely duplicate)
dt<-dt %>% filter(!grepl('true',`Q_RelevantIDDuplicate`))
#Exclude >=75 duplicate scores (likely duplicate)
dt %>% filter(75>=dt$Q_RelevantIDDuplicateScore)
#

#Check that number of n's per condition matches with the raw file
sum(dt$PicOrder=="0")
sum(dt$PicOrder=="1")
sum(dt$PicOrder=="2")
sum(dt$PicOrder=="3")
sum(dt$PicOrder=="4")
sum(dt$PicOrder=="5")
sum(dt$PicOrder=="6")
sum(dt$PicOrder=="7")
sum(dt$PicOrder=="8")
sum(dt$PicOrder=="9")
sum(dt$PicOrder=="10")
sum(dt$PicOrder=="11")

#Remove unneccesary data columns
dt = subset(dt, select = -c(Q_RelevantIDuplicateScore,z) )
#plot distribution of age
dt$Age<-as.numeric(dt$Age)
dt$Age<- dt$Age %>% na.omit

plot(hist(x=dt$Age,xlim=range(1:100)))
plot(density(x=dt$Age))
mean(na.omit(dt$Age))



ggplot(dt, aes(x=Age)) + geom_histogram()
# Change the width of bins
ggplot(dt, aes(x=Age)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(dt, aes(x=Age)) + 
  geom_histogram(color="black", fill="white")
p
# Add mean line
p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", size=1)
# Histogram with density plot
ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

