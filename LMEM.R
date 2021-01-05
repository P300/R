library(readxl)
library(tidyverse)
library(lme4)
library(afex)
library(emmeans)

dat1 <- readxl::read_xlsx("C:/Users/P3d_LMEM_76 71.xlsx")#readfile
dat2 <-subset(dat1, sub!="731"& sub!="737" &sub!="742"& sub!="745" &sub!="906"& sub!="5361" &sub!="5372") #remove outliers
df1<- dat2 %>% pivot_longer(-c(sub,Group), 
                            names_to="Electrode", 
                            values_to="P3d") #reshpae the data to long file if necessary. in this case, all columns of electrodes being turned into one column called electrode
dat3=na.omit(dat1)#remove na if there is any
#if intend to do log transform; otherwise skip
min(df1$P3d)#check for any negative values, need to be positive 
aa=df1$P3d+9 # add the smallest integer to make it positive value
logaa = log(aa)# log transform

fm1<-afex::lmer (P3d ~ Group + (1|sub),data=df1)#fix-Group, random-sub 
fm2 <- afex::lmer (P3d ~ Group + (1|Electrode)+(1|sub), data = df1)#fix-Group, random-sub and electrode
em <- emmeans::emmeans(fm1, ~ Group) # get estimated marginal means
anova(fm1) # get F test values
summary(fm2) # get coef, SE, df, t, p

