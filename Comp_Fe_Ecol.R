library(dplyr)
library(ECoL)
setwd("C:/Users/Irfan khan/OneDrive/PHD/paper final experiments/MfeatExtractor-master")
sub.data = gsub(x = list.files(path = "data/datasets/"), pattern = ".arff", replacement = "")

compfe<- data.frame()

for (i in 1:length(sub.data)) {
  
  
  datafile<- sub.data[i]
  
  #data <- read.table(file = "clipboard",
   #                     sep = "\t", header=TRUE)
 
  
  data = RWeka::read.arff(file = paste0("data/datasets/", datafile, ".arff"))
  dir.create(path = paste0("mfeats/", datafile), showWarning = FALSE, recursive = TRUE)
  data<- data %>% select_if(~ length(unique(.)) > 1)  # preprocessing, romove the columns with one factor value
  
  
  freqclass<- as.data.frame(table(data$Class)) # get the frequency of the classes, 
  
  minority_class_value<- (1:nrow(freqclass))[freqclass[,2] == 1]
 
  
  for (k in 1:length(minority_class_value)) {
    
    j<- minority_class_value[k]
    data<- data[!data$Class == j,]
    xv<- data[data$Class == j,]
  }
  
  #pre.file = paste0("data/datasets/", datafile, ".arff")
    #RWeka::write.arff(x = data, file = pre.file)
 
  
  comp.file = paste0("mfeats/", datafile, "/ComplexityFeatures.RData")

  df<- data

  xxx<- data.frame(complexity(Class ~ ., df, type="class"))

  yyy<- data.frame(t(xxx))

  compfe<- rbind(compfe, yyy)

  save(yyy, file = comp.file)



  cat(paste0(" - Datafile: \t", datafile, "\n"))

}

