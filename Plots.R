setwd("C:/Users/Irfan khan/OneDrive/PHD/paper final experiments/results")

library(gcookbook) # For the data set
library(ggplot2)
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

data=read.excel()

# Grouped
bp <- ggplot(data, aes(fill=MF, y=Accuracy, x=Accuracy, group=stra)) + 
  geom_bar(position="dodge", stat="identity") + theme(text = element_text(size=15),
                                                      axis.text.x = element_text(angle=70, hjust=1))

bp
bp + facet_grid(Accuracy ~ .)
# Split in horizontal direction
bp + facet_grid(. ~ Accuracy)
bp

bp <- ggplot(data, aes(fill=Alpha, y=Accuracy, x=Meta_Features)) + 
  geom_bar(position="dodge", stat="identity") + theme(text = element_text(size=15),
                                                      axis.text.x = element_text(angle=70, hjust=1))

as.factor(data$Alpha)


bp <- ggplot(data, aes(fill=MF, y=AC, x=ST)) + 
  geom_bar(position="dodge", stat="identity")


##################################################
################################################

## Plot_Save_Rdata
library(tidyverse)

df.mean = data %>% 
  group_by(ST) %>% 
  mutate(ymean = mean(Accuracy))

bp <- ggplot(data, aes(x=MetaFeatures, y=Accuracy, group=MetaFeatures)) + theme(text = element_text(size=18))+
  geom_bar(aes(fill=MetaFeatures), stat="identity") + 
  geom_errorbar(data=df.mean, aes(MetaFeatures, ymax = ymean, ymin = ymean),
                size=0.5, linetype = "longdash", inherit.aes = F, width = 1)


bp + facet_grid(. ~ ST) + coord_cartesian(ylim=c(0.3,0.6)) 


df<- data

bp <- ggplot(df, aes(x=MF, y=Accuracy, group=MF)) + 
  geom_bar(aes(fill=MF))


save.image("Acc05.RData")
