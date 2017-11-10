  #  install.packages('reshape2')
#   Install reshape2 package
library(reshape2)
  #  install.packages('corrplot')
#   Install ggplot2 package
library(ggplot2)
  #  install.packages('flashClust')
#install.packages("flashClust")
library(flashClust)


  #  import data frame from csv
bike <- read.csv('BikeSharing.csv')

  #  Initial Inspection
head(bike)
View(bike)
str(bike)

  #  Consolidate Use Columns for Further Analysis 
sm_bike <- bike[,c("Weekday","Casual", "Registered")]

  #  Define Day of Week Vector
weekday <-levels(bike$Weekday)
weekday
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

  #  
bike_clusters2 <- cbind(sm_bike2, cluster=clusterCut2) 
head(bike_clusters2) 

  # Various Plots of Variables with Logical Coorelations 
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

