library(dplyr)
setwd("C:/Users/Irfan khan/OneDrive/PHD/paper final experiments/MfeatExtractor-master")
sub.data = gsub(x = list.files(path = "data/datasets/"), pattern = ".arff", replacement = "")


for (i in 1:length(sub.data)) {
  
  datafile<- sub.data[i]
  
  my_data = RWeka::read.arff(file = paste0("data/datasets/", datafile, ".arff"))
 

xxx<- as.data.frame(table(my_data$Class))
  
  minority_class_value<- (1:nrow(xxx))[xxx[,2] == 2]
  
  for (k in 1:length(minority_class_value)) {
   
    j<- minority_class_value[k]
    my_data<- my_data[!my_data$Class == j,]
   }
  
}

y<- my_data


if(is.data.frame(y)) {
  y <- y[, 1]
}

type <- match.arg(type, c("class", "regr"), TRUE)

#if(type == "class") {
  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

unique_class_col_val <- (unique(unlist(my_data[,ncol(my_data)])))  #**



