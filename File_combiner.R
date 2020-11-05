#This example shows how to combine multiple excel files into one, with and without keeping filenames
#Thanks to this post http://datacornering.com/how-to-combine-files-with-r-and-add-filename-column/

require(dplyr)
require(data.table)


setwd('C:/Users/Desktop/SCTmeg/Behavioral/SCThighrename/') #Sets the working directory. Be sure to check and change this before running!

#row combine files without adding filenames
all_paths<-list.files(getwd()) #read paths to files
files<-lapply(all_paths,read.csv,header=T) #read file content
all_files<-do.call(rbind,files) #combine all files by row


#row combine files by adding a column with filenames
all_paths<-list.files(getwd()) #read paths to files
files<-lapply(all_paths,read.csv,header=T) #read file content
all_filenames<-all_paths %>% #read file name list
  basename() %>%
  as.list()

all_lists<-mapply(c,all_filenames,files, SIMPLIFY = FALSE) # Combine file content list with filename list
all_result <- rbindlist(all_lists, fill = T) #Unlist result
names(all_result)[1] <- "Filename"# change column name

#or combined code
all_txt <- rbindlist(mapply(
  c,
  (
    list.files(
      path = "~/txt_files/",
      pattern = "*.txt",
      full.names = TRUE
    ) %>%
      lapply(
        read.table,
        header = TRUE,
        sep = "\t",
        encoding = "UTF-8"
      )
  ),
  (
    list.files(
      path = "~/txt_files/",
      pattern = "*.txt",
      full.names = TRUE
    ) %>%
      basename() %>%
      as.list()
  ),
  SIMPLIFY = FALSE
),
fill = T)

