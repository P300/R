#The code does the following:
#rename column names 
#get mean RT group by Trialtype and save to one output file/individual files

library(data.table)
library(tidyverse)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(stringr)
#rename column names

#check column name
bb=read.csv("C:/Users/Desktop/SCTmeg/Behavioral/Control1/1055 ANT Behavioral Data_Flanker Block 2.xlsx")
names(bb)#display headers

#assign new column names
files <- list.files(path="C:/Users/Desktop/SCTmeg/Behavioral/SCThigh/", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
for (file in files ){
  data<-read_excel(file)
  names(data)<-c("TrlID"  ,   "Cue",  "Time2SyncTrig(ms)...3",  "Point2SyncTrig...4"   ,  "Repetition...5"    , "TrigName...6"   ,       
                 "Time2SyncTrig(ms)...7",  "Point2SyncTrig...8"    , "Repetition...9"   ,   "Trialtype"  ,   "Accuracy"   , 
                 "RT","Point2SyncTrig...13"  ,  "Repetition...14", "TotalTrig") #new name
  outfile<-file.path("C:/Users/YUXC9N/Desktop/SCTmeg/Behavioral/SCThighrename/", basename(file)) #new column names
  write.csv(data, outfile, sep = "|", row.names = FALSE, quote = FALSE)
}


#get mean RT group by Trialtype 
#save to one output file

files <- list.files(path="C:/Users/Desktop/SCTmeg/Behavioral/SCTlowrename/", pattern="*.xlsx", full.names=TRUE, recursive=FALSE)

nm1 <- tools::file_path_sans_ext(basename(files))
imap_dfr(setNames(files, nm1), ~ {
  dat <- read_csv(.x) 
  if(exists('RT', where = dat)) {
    dat %>%
      group_by(Trialtype) %>%
      drop_na() %>%
      summarise(Count = n(), 
                mean.RT= mean(RT, na.rm = TRUE), .groups = 'drop')  %>%
      mutate(filename = .y)
    
    
  } else tibble(Trialtype = first(dat$Trialtype),
                mean.RT = NA_real_, filename = .y)
}) %>%
  
  write_csv(path  =  file.path("C:/Users/Desktop/SCTmeg/Behavioral/", "SCTlow_trial.csv"))

#save to individual files


map(files, ~ {
  dat <- read_csv(.x) 
  if(exists('RT', where = dat)) {
    dat %>%
      group_by(Cue) %>%
      drop_na() %>%
      summarise(Count = n(), 
                mean.RT= mean(RT, na.rm = TRUE), .groups = 'drop') %>%
)

  }
})

write.csv(path  =  file.path("C:/Users/Desktop/SCTmeg/Behavioral/Control1", 
                             str_c(tools::file_path_sans_ext(basename(.x)), "RTcue.csv"))
          
          #Group data by sub and cuetype
          aa=read.csv("C:/Users/Desktop/SCTmeg/Behavioral/SCTlow_trial.csv")
          average<- aa %>%
            group_by(Trialtype,sub) %>%
            drop_na() %>%
            summarise(Count = n(), 
                      mean= mean(mean.RT, na.rm = TRUE), .groups = 'drop')
          write_csv(average,"C:/Users/Desktop/SCTmeg/Behavioral/SCTlowtrial.csv")
          
          #further group data by cue
          aa1=read.csv("C:/Users/Desktop/SCTmeg/Behavioral/Control_trial.csv")
          ave<-aa1 %>%
            group_by(Cue) %>%
            drop_na() %>%
            summarise(Count = n(), 
                      mean= mean(mean, na.rm = TRUE), .groups = 'drop')
          
          #compute trial type
          aa1=read.csv("C:/Users/YUXC9N/Desktop/SCTmeg/Behavioral/AllRTtrial.csv")
          test1<-aa1 %>% 
            pivot_wider(names_from=Trialtype,values_from=mean)
          write.csv(result1,"C:/Users/YUXC9N/Desktop/SCTmeg/Behavioral/exGaussian/exGaussianPrm_noOutlier.csv")
          