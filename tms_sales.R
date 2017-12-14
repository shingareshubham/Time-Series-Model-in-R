setwd("C:/R")

#Reading the data
sales <- read.csv("2-12/timeseries/sales.csv")

#What kind of data we have
sales
head(sales)
tail(sales)

class(sales)
str(sales)


#Create time series from the input data

sales <- ts(sales[,1],start=1995,freq=12)

#Let's view, what is the output
sales
class(sales)
str(sales)

#Plot Time Series
plot(sales)

#Divides into Seasonal, Trend and Remainder. S.Window controls how rapidly the seasonal component can change
#STL means Seasonal Trend Decomposition using Loess

decom<-stl(sales,t.window=NULL, s.window="periodic")

summary(decom)
decom
plot(decom)

#Single Exponential Seasoning, coefficient tells you the level
hws1<-HoltWinters(sales, alpha=0.2, beta=FALSE, gamma=FALSE)

hws1

#Predict SES, Prediction interval gives me upper and lower bound of the confidence interval
sales.pred1<-predict(hws1,n.ahead=12,prediction.interval=TRUE)

#Predicted values
sales.pred1

#Plot the base level graph, giving limits to x
plot.ts(sales, xlim = c(1995,2014))
#Historical Fitted values, no trend so both columns are same
hws1$fitted
#Fit the historical fitted values
lines(hws1$fitted[,1],col="green")
#Fit the future predicted values
lines(sales.pred1[,1],col="blue")
#Fit the upper interval predicted values
lines(sales.pred1[,2],col="red")
#Can't see the upper bound, need to have y limit
plot.ts(sales, xlim = c(1995,2014),ylim=c(150000,400000))
#Fit the historical fitted values
lines(hws1$fitted[,1],col="green")
#Fit the future predicted values
lines(sales.pred1[,1],col="blue")
#Fit the upper interval predicted values
lines(sales.pred1[,2],col="red")
#Fit the upper interval predicted values
lines(sales.pred1[,3],col="red")

#Put Trend component, Double exponential smoothening
hws2<-HoltWinters(sales, alpha=0.2, beta=0.1,gamma=FALSE)
hws2

#Predict SES, Prediction interval gives me upper and lower bound of the confidence interval
sales.pred2<-predict(hws2,n.ahead=12,prediction.interval=TRUE)
#Predicted values
sales.pred2
#Plot the base level graph, giving limits to x and y
plot.ts(sales, xlim = c(1995,2014),ylim=c(150000,400000))
#Historical Fitted values, now third column is trend. Xhat is prediction
hws2$fitted
#Fit the historical fitted values
lines(hws2$fitted[,1],col="green")
#Fit the future predicted values
lines(sales.pred2[,1],col="blue")
#Fit the upper interval predicted values
lines(sales.pred2[,2],col="red")
#Fit the upper interval predicted values
lines(sales.pred2[,3],col="red")

#Triple Exponential Seasoning
hws3<-HoltWinters(sales, alpha = 0.2, beta=0.1,gamma=0.1)
#Look at the results
hws3
#Predict TES, Prediction interval gives me upper and lower bound of the confidence interval
sales.pred3<-predict(hws3,n.ahead=12,prediction.interval=TRUE)
#Predicted values
sales.pred3
#Plot the base level graph, giving limits to x and y
plot.ts(sales, xlim = c(1995,2014),ylim=c(150000,500000))
#Historical Fitted values, 3rd column is trend and 4th column is season
hws3$fitted
#Fit the historical fitted values
lines(hws3$fitted[,1],col="green")
#Fit the future predicted values
lines(sales.pred3[,1],col="blue")
#Fit the upper interval predicted values
lines(sales.pred3[,2],col="red")
#Fit the upper interval predicted values
lines(sales.pred3[,3],col="red")


#Automatic Triple smoothening
hws6<-HoltWinters(sales)
#Look at alpha and beta values
hws6
#Predict TES, Prediction interval gives me upper and lower bound of the confidence interval
sales.pred6<-predict(hws6,n.ahead=10,prediction.interval=TRUE)
#Predicted values
sales.pred6
#Plot the base level graph, giving limits to x
plot.ts(sales, xlim = c(1995,2014),ylim=c(150000,400000))
#Historical Fitted values, now third column is trend. Xhat is prediction
hws6$fitted
#Fit the historical fitted values
lines(hws6$fitted[,1],col="green")
#Fit the future predicted values
lines(sales.pred6[,1],col="blue")
#Fit the upper interval predicted values
lines(sales.pred6[,2],col="red")
#Fit the upper interval predicted values
lines(sales.pred6[,3],col="red")

#Creating File for prediction
p<-predict(hws6,12)
write.csv(p,"2-12/timeseries/p.csv")
p


#without any trens/sea/sim