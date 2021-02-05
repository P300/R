#plot each subcategory from data with multiple categories: groups, ERFs, and cue conditions, using split and lapply
rm(list=ls())

library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
 
df1=read.csv("allcues_merged_renamed.csv")# data with multiple groups, ERFs, and cue conditions

#get stat first then split the stats
stats1<- df1 %>%
  group_by(Group,ERF, cue) %>% #grouping variables
  summarize(mean=mean (Latency),
            sd = sd(Latency),
            sem = sd(Latency)/sqrt(n()),
            n = n())
splitlist = split(stats1,list(stats1$Group,stats1$ERF)) #split the stats by Group and ERF

lapply(splitlist,function(x) {
  ggplot(data=x, aes(cue,mean)) + 
    geom_bar(stat="identity",position="dodge")+
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), 
                  width = 0.2,
                  position = position_dodge(0.9))+
    theme(text = element_text(size=12),
          axis.text.x = element_text(angle=0, hjust=1))+
    ggtitle("Latency")
}) -> list_plots #use lapply to loop through the stats list and plot

lapply(names(list_plots),function(x) ggsave(filename=paste(x,".jpeg",sep=""),plot=list_plots[[x]]))#save plots individually

#get ANOVA from sub dataset (create a new list by splitting data)
newlist=split(df1,list(df1$Group,df1$ERF))#put all the factors you want to split by in a list
lapply(newlist,function(x) with(x,TukeyHSD(aov(Latency ~ Group))))#apply ANOVA to all lists