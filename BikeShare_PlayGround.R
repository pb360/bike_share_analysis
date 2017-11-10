  # Clear Console
cat("\014")

    # Seting the working Directory 
      "set working directory. If you want you can download google drive for your 
      comuter and we can make two setwd lines in the code and comment out the one 
      based on whose doing work"

    #   Set wd for Carolyn
#setwd("/Users/CarolynsComputerUser/Google Drive/......../BikeShare")
    #   Set wd for Paul (Mac)
#setwd("/Users/Paul1/Google Drive/Penn State/Junior/IE 330 - Engineering Analytics/Projects/BikeShare")
    #   Set wd for Paul (Windows)
setwd("C:/Users/Paul Boehringer/Google Drive/Penn State/Junior/IE 330 - Engineering Analytics/Projects/BikeShare")

      #INSTALL Necessary Packages   
    
    #   Install corrgram package 
#install.packages('corrgram')
library(corrgram)
    #   Install corrplot package
#install.packages('corrplot')
library(corrplot)
#   Install ggplot2 package
#install.packages('ggplot2')
library(ggplot2)
    #   Install reshape2 package
#install.packages('reshape2')
library(reshape2)
  #  install.packages('flashClust')
#install.packages("flashClust")
library(flashClust)
  #  install.packages('MASS')
#install.packages("MASS")
library(MASS)
  #  install.packages('lattice')
#install.packages("lattice")
library(lattice)
  #  install.packages('forecast')
library(forecast)


    #  import data frame from csv
bike <- read.csv('BikeSharing.csv')


    #  initial inspection on bike
str(bike)

    #  Clean up import  -  delete variables X and X.1 from data frame (they were empty)
bike$X   <- NULL
bike$X.1 <- NULL


  #  Another Inspection of Dataframe 
head(bike)
View(bike)
str(bike)

  #  Consolidate Useage Columns for Further Analysis 
sm_bike <- bike[,c("Weekday","Casual", "Registered")]

  #  Define Day of Week Vector
weekday <-levels(bike$Weekday)
Weekday_binary <- data.frame(matrix(ncol=length(weekday),nrow=nrow(sm_bike)))
colnames(Weekday_binary) <- weekday

  #  Set up Distance Matrix for Clustering of Consolidated Bike Dataframe 
distance_matrix <- dist(sm_bike)
cluster_model <- flashClust(distance_matrix) 
plot(cluster_model)

  #  Summarize year column of bike 
summary(bike$Year)

  #  Cluster into 3 groups 
clusterCut <- cutree(cluster_model, 3)
clusterCut 

  #  Define and Plot Dataframe based off clusters of consolidated usage data
bike_clusters <- cbind(sm_bike, cluster=clusterCut) 
head(bike_clusters) 
qplot(data=bike_clusters, x=Weekday, y=Casual, colour=cluster)
qplot(data=bike_clusters, x=Weekday, y=Casual, colour=cluster, geom="jitter")

  #  Define Second consolidated dataframe and prepare for clustering on weather and user type 
sm_bike2 <- bike[,c("Weather","Casual", "Registered")]
weather <- levels(bike$Weather)
Weather_binary <- data.frame(matrix(ncol=length(weather),nrow=nrow(sm_bike2)))
colnames(Weather_binary) <- weather

  #  Define distance matrix for condition vs user type clustering 
distance_matrix2 <- dist(sm_bike2)
cluster_model2 <- flashClust(distance_matrix2) 
plot(cluster_model2)

  #  Cluster into 3 groups for second clustering method 
clusterCut2 <- cutree(cluster_model2, 3)
clusterCut2

  #  Cluster based off condition and user type 
bike_clusters2 <- cbind(sm_bike2, cluster=clusterCut2) 
head(bike_clusters2) 

  #   Various Plots of Variables with Logical Coorelations 
qplot(data=bike_clusters, x=Weekday, y=Casual, colour=cluster)
qplot(data=bike, x=Weekday, y=Count, geom="jitter", main = "Day of Week VS. Total Daily Usage")
qplot(data=bike, x=Count, y=Temp, geom="jitter", main = "Temperature VS. Total Daily Usage")
qplot(data=bike, x=Casual, y=Count, geom="jitter", main = "Casual Users VS. Total Users")
qplot(data=bike, x=Registered, y=Count, geom="jitter", main = "Registered Users VS. Total Users")
qplot(data=bike, x=Temp, fill=Registered, geom="bar", main = "Temperature VS. Registered Users")
qplot(data=bike, x=Temp, fill=Casual, geom="bar", main = "Temp vs. Casual Users")
qplot(data=bike, x=Holiday, fill=Registered, geom="bar",  main = "Holiday vs Registered Bike Usage")
qplot(data=bike, x=Holiday, fill=Casual, geom="bar",  main = "Relationship of Casual Usage with Holiday Classification")
qplot(data=bike, x=Weekday, fill=Registered, geom="bar", main = "Registered Bike Usage by Day of The Week")
qplot(data=bike, x=Weekday, fill=Casual, geom="bar", main = "Registered Bike Usage by Day of The Week")


    #  Make a copy of bike with Numeric Values ONLY
nums <- sapply(bike, is.numeric)
numericBike <- bike[ , nums]
View(numericBike)

    #   Coorelation plot of different factors
corrgram(numericBike, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Correlations of Numeric Variables")


    #  Coorelation plot of different factors  -  v2
corrBike <- cor(numericBike)
corrplot(corrBike, method = "ellipse")

  
#FORECASTING SECTION 

  #  Make time series sequence to represent Months in the data 
MonthsDated <- seq(as.Date('2011-01-01'), as.Date('2012-12-01'), by = 'month')

  #  Add and format YearMonth to numeric bike. This will list the different months in the data
numericBike$YearMonth <- as.numeric(paste(bike$Year, bike$Month, sep=''))

  #  Find unique values in YearMonth
MonthLevels <- (as.vector(unique(numericBike$YearMonth)))

  #   Define vector which will represent how many times bikes are used in each month
MonthUsage <- vector(length = length(MonthLevels))

  #  Populate MonthUsage with total usage 
for (level in 1:length(MonthLevels)){
 for (i in 1:nrow(numericBike)) {
   if (numericBike[i, 'YearMonth'] == MonthLevels[level]) {
     MonthUsage[level] <- MonthUsage[level] + numericBike[i, 'Count']
    }
  }
}


  #    1.) Forecasting: NAIVE SEASONAL METHOD

  #  Set Forecast to periods of last year 
Naive_Season_12 <-  MonthUsage[1:12]
MonthUsage
cast1 <- data.frame(MonthLevels, MonthUsage[13:24], Naive_Season_12)

  #  1.b) Plotting Forecast Vs Actual 
plot(MonthUsage[13:24] ~ MonthsDated[13:24], col='red', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage")
par(new=T)
plot(Naive_Season_12 ~ MonthsDated[13:24], col='green', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage", main = "(1) Naive Forecasting vs. Actual Monthly Usage ")
legend( x = "bottomright", c('Actual Usage', 'Naive Forecast'), col = c('red', 'green'), lty = c(1, 1))




    # 2.) Forecasting: NAIVE TREND SEASONAL METHOD   for 2012 

  #  Make vector for usage in months that includeds December of last year so trend can be used 
MonthUsage_w_Dec09 <- c(80000, MonthUsage)
MonthUsage_w_Dec09

  #  Populate forecast vector for part 2
Naive_Trend_w_Season <- vector( length = 12)
for (i in 1:12){
  Naive_Trend_w_Season[i] <- MonthUsage[i+1] + (MonthUsage[i+11] + MonthUsage[i])/12   
}

  #  2.b) Plotting Forecast Vs Actual 
plot(MonthUsage[13:24] ~ MonthsDated[13:24], col='red', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage")
par(new=T)
plot(Naive_Trend_w_Season ~ MonthsDated[13:24], col='green', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage", main = "(2)Naive Seasonal with Trend Forecasting vs. Actual Usage ")
legend( x = "bottomright", c('Actual Usage', 'Naive Season+Trend Forecast'), col = c('red', 'green'), lty = c(1, 1))

 


    # 3.) Error for  1 & 2 ... MSE, MAD, and TS

MSE1 <- mean((MonthUsage[13:24] - Naive_Season_12)^2)
MSE1 
MAD1 <- sum(abs(MonthUsage[13:24] - Naive_Season_12))/length(Naive_Season_12)
MAD1
TS1 <- sum(MonthUsage[13:24] - Naive_Season_12)/MAD1
TS1
MSE2 <- mean((MonthUsage[13:24] - Naive_Trend_w_Season)^2) 
MSE2 
MAD2 <- sum(abs(MonthUsage[13:24] - Naive_Trend_w_Season))/length(Naive_Trend_w_Season)
MAD2 
TS2 <- sum(MonthUsage[13:24] - Naive_Trend_w_Season)/MAD1
TS2



    # 4.)  3-Point SMA 

  #  Define and Populate vector which forecasts using the SMA will be used 
SMAcast <- vector( length = length(MonthLevels) )
for (i in 1:length(MonthLevels)) { 
  SMAcast[i+3] <- ((MonthUsage[i] + MonthUsage[i+1] + MonthUsage[i+2])/3)
}
SMAcast[is.na(SMAcast)] <- 0
SMAcast <- SMAcast[4:24]
length(SMAcast)
SMAcast

  #  4b.) Plotting Forecast Vs Actual 
plot(MonthsDated[4:24], MonthUsage[4:24], col='red', ylim=range(c(0, 280000)), xlab='Month (2012)',
     ylab="Total Monthly Usage")
par(new=T)
plot(SMAcast ~ MonthsDated[4:24], col='green', ylim=range(c(0, 280000)), xlab='Month (2012)',
     ylab="Total Monthly Usage", main = "(4) Simple Moving Average Forecasting vs. Actual Usage ")
legend( x = "bottomright", c('Actual', 'SMA Forecast'), col = c('red', 'green'), lty = c(1, 1))



  #  5.)   Exponential Smoothing Forecasting 
ExpCast <- vector( length = length(MonthUsage))
ExpCast[1] <- MonthUsage[1]
alpha <- 0.3
for (i in 1:(length(MonthUsage)-1)) { 
  print(i)
  ExpCast[i+1] <- alpha*MonthUsage[i] + (1-alpha)*ExpCast[i] 
}

  #  5b.) Plotting The Exponential Smoothing Forecast
plot(MonthUsage ~ MonthsDated, col='red', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage")
par(new=T)
plot(ExpCast ~ MonthsDated, col='green', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage", main = "(5) Exponential Smooth Forecasting vs. Actual Monthly Usage ")
legend( x = "bottomright", c('Actual Usage', 'Exp Smooth Forecast'), col = c('red', 'green'), lty = c(1, 1))



  # 6.) Error for  4 & 5 ... MSE, MAD, and TS

MSE4 <- mean((MonthUsage[3:24] - SMAcast)^2)
MSE4 
MAD4 <- sum(abs(MonthUsage[3:24] - SMAcast))/length(SMAcast[3:24])
MAD4
TS4 <- sum(MonthUsage[3:24] - SMAcast)/MAD4
TS4
MSE5 <- mean((MonthUsage - ExpCast)^2) 
MSE5 
MAD5 <- sum(abs(MonthUsage - ExpCast))/length(ExpCast)
MAD5 
TS5 <- sum(MonthUsage - ExpCast)/MAD1
TS5


  #  7.)   Fit data from 2011 only with linear regression line 
fit <- lm(MonthUsage[1:12] ~ time(MonthsDated[1:12]))
summary(fit)
slope     <- as.numeric(coef(fit)["time(MonthsDated[1:12])"])
intercept <- as.numeric(coef(fit)["(Intercept)"])
rSquared  <- summary(fit)$r.squared
slope
intercept
rSquared


  #  8.)  Use Fitting from 7 to forecast monthly counts for 2012
timeCast <- vector(length = 12)
for (i in 1:12) {
  print(i)
  timeCast[i] <- intercept + slope * (i + 12)
}
length(timeCast)

  #  8b.) Plotting The Exponential Smoothing Forecast
plot(MonthUsage[13:24] ~ MonthsDated[13:24], col='red', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage")
par(new=T)
plot(timeCast ~ MonthsDated[13:24], col='green', ylim=range(c(0, 250000)), xlab='Month (2012)',
     ylab="Total Monthly Usage", main = "(8) Linear Fit Forecasting vs. Actual Monthly Usage ")
legend( x = "bottomright", c('Actual Usage', 'Lineaer Fit Forecast'), col = c('red', 'green'), lty = c(1, 1))



  #  9.) Error for 8 ... MSE, MAD, and TS

MSE8 <- mean((MonthUsage[13:24] - timeCast)^2) 
MSE8 
MAD8 <- sum(abs(MonthUsage[13:24] - timeCast))/length(timeCast)
MAD8 
TS8 <- sum(MonthUsage[13:24] - timeCast)/MAD1
TS8





