rm(list=ls())
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
 


trial=read.csv("C:/Users/YUXC9N/Desktop/SCTmeg/Behavioral/longfile_ANT.csv")
names(trial1)
trial$sub=as.numeric(trial$sub)


#remove outliers
# if know the sub ID, then remove sub with accuracy < 66%
trial1<-subset(trial, sub!="1586" & sub!="1060" &sub!="1551" &sub!="1023" &sub!="1512"&sub!="1544" &sub!="1556" &sub!="1575"
               &sub!="1608" &sub!="1659" &sub!="1686" &sub!="1697" &sub!="1075" &sub!="1513" &sub!="1560" &sub!="1562" &sub!="1583" &sub!="1084"
               &sub!="1649"  & sub!= "1647"  & sub!="1621" & sub!="1547") #last row are outliers 


#remove RT > < 1sd+_mean
hist(trial2$mean.RT)
boxplot(outlier_remove$RT,ylab="mean.RT")
out=boxplot.stats(outlier_remove$RT)$out
out_ind<-which(aa1$RT %in% c(out))
out_ind #return row number
trial2[out_ind,]#return all variables for outliers

#remove RT with set values, here <100 & > mean+3SD
mean=mean(aa1$RT)
sd=sd(aa1$RT)
outlier=aa1[aa1$RT< 100 |  aa1$RT> mean+3*sd,]
outlier=aa1[aa1$RT< mean-3*sd |  aa1$RT> mean+3*sd,]#save outliers in a dataframe
outlier_remove=trial1[!row.names(trial1) %in% row.names(aa),]#remove aa from file


# get mean across blocks

df1 <- dat1 %>% dplyr::select(ERF,Group,Latency)

stats<- df1 %>%
  group_by(Group,Trialtype, Cue) %>%
  summarize(
    mean=mean (RT),
    sd = sd(RT),
    sem = sd(RT)/sqrt(n()),
    n = n())


# plot with 2 categorical variables
ggplot(stats,aes(x=Group,y=mean,fill=Group))+ geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),                            
                width = 0.2,
                position = position_dodge(0.9))+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=0, hjust=1)) +
  ggtitle("RT_trialtype")

# plot with 3 categorical variables distributed into panels
aa=stats[-grep("NeutralReponse",stats$Trialtype),] #remove rows with "NeutralResponse"
p=ggplot(stats,aes(x=Group,y=mean, fill=Group))+ geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),                            
                width = 0.2,
                position = position_dodge(0.9))+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=1)) +
  ggtitle("RT_Cue by Trialtype")+facet_grid(Trialtype ~ Cue) 


p+coord_cartesian(ylim =c(500,800) ) #change start value on y-axis

write_csv(stats,"C:/Users/YUXC9N/Desktop/SCTmeg/Behavioral/RT_stats_CueTrial.csv")

write.csv(stats,file="C:/Users/YUXC9N/Desktop/SCTmeg/MEG/CongruentMaxRMS.csv")
