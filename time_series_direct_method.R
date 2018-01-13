#before going we need to prepare your data i.e finding missing value, outlire etc.
#Very first step of data analysis is data preparation after data is prepare for time series then you apply this one line code
#1 dataset=ts(........)
#2 HoltWinters(dataset)
#above two line of code do your cluster analysis

setwd("C:/R") #set working directory

#Reading the data
sales <- read.csv("2-12/timeseries/sales.csv") #read your dataset

#What kind of data we have
sales
head(sales)
tail(sales)

class(sales)
str(sales)
View(sales)

#Create time series from the input data
sales <- ts(sales[,1],start=2000,freq=12)

#Let's view, what is the output
sales
class(sales)
str(sales)

#Plot Time Series
plot(sales)

#Automatic Triple smoothening
hws6<-HoltWinters(sales)
#Look at alpha and beta values
hws6
#Creating File for prediction
p<-predict(hws6,12)
p

