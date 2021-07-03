#####################
# change the no of columns in code nn[1,15]<- temp according to every dataset

####################

setwd("C:/Users/Irfan khan/OneDrive/PHD/paper final experiments")
library(tidyr)
library(exreport)
finalmatrix<- data.frame()
ran_selected_data<- data.frame()
#read.excel <- function(header=TRUE,...) {             ### This code is for reading data from clipboard, the Fvalue measures with dataset numbers 
 # read.table("clipboard",sep="\t",header=header,...)
#}

#my_data=read.excel()

my_data <- read.table(file = "clipboard",                            #### insert the data from excel in the form Datasetname, Algo, Fscore, Datasetname
                      sep = "\t", header=TRUE)
xx<- as.character(unique(unlist(my_data$datasetnames)))

my_data$Algo <- NULL

for (i in 1:length(xx)) {
      temp<- xx[i]

newdata <- subset(my_data, my_data$datasetnames == temp)
newdata$datasetnames<- NULL
x<- newdata %>% gather(Algo, Fscore, 2:18)

en<- "expself"
y<- expCreate(x, methods = "Algo", problems = "DatasetnameG", parameters = c(), respectOrder = FALSE, en, tol = 1e-09)

y <- expReduce(y, FUN = mean)

testAccuracy <- testMultipleControl(y, "Fscore", "max")
summary(testAccuracy)

z<- testAccuracy[["names"]]

pval<- testAccuracy[["pvalues"]]
n<- cbind(z,pval)

nn<- as.data.frame(t(n))
row.names(nn)<- NULL

tempDF <- nn
tempDF[] <- lapply(nn, as.character)
colnames(nn) <- tempDF[1, ]
nn <- nn[-1 ,]
tempDF <- NULL


nn[1,12]<- temp    ##### Change the column no according to the number of FSS algorithms for inserting the dataset name, i-e last column for dataset name

finalmatrix<- rbind(finalmatrix, nn)

}


save.image(file='Multiple_comarison_Review.RData')

write.csv(finalmatrix, file = "multiplecomparison.csv")

getwd()
