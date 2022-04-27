
library(rvest) 
library(dplyr)
library(utils)

parents <- read.csv("https://raw.githubusercontent.com/Vasanth1975/DATASETS_2021/main/Untitled%20spreadsheet%20-%20Sheet1.csv")

parents1 <-data.frame(parents[1:301,])
View(parents1)


data <- cbind(data,parents1)

for(i in 1:ncol(data))
{
  if(is.character(data[,i])==T)
  {
    data[,i]<- as.factor(data[,i])
  }
}

parents1 <-data.frame(parents[1:301,])
View(parents1)

#------------------------------------------
data1 <- cbind(data1,parents1)

for(i in 1:ncol(data1))
{
  if(is.character(data1[,i])==T)
  {
    data1[,i]<- as.factor(data1[,i])
  }
}
