x<-10
x

getwd()

setwd("C:/Users/lenovo/Desktop/STAT 462 Data Mining/Assignment_1/datasets (1)")

brakingdata<-read.csv("braking.csv") #read the csv file from directory

View(brakingdata) #View the braking data
sorted_brakingdata<- brakingdata[order(brakingdata$dist),]
sort(brakingdata,)

View(sorted_brakingdata)
sorted_brakingdata$speedinkmhr<-sorted_brakingdata$speed * 1.60934 #Converting miles/hr to km/hr

View()

sorted_brakingdata$distanceinmeter<-sorted_brakingdata$dist * 0.3048 #Converting miles to meters

sorted_brakingdata$speedinkmhrplus5<-sorted_brakingdata$speed * 1.60934 + 5 #Converting miles/hr to km/hr

View(sorted_brakingdata)
#split the dataset randomly into training set(80%) and Testing set(20%)

nrowsinbrakindata <-nrow(sorted_brakingdata)
set.seed(1)
sorted_brakingdata_indices <- sample(1:nrowsinbrakindata,size = 0.8*nrowsinbrakindata) 

sorted_brakingdata_training <-sorted_brakingdata[sorted_brakingdata_indices,]

sorted_brakingdata_testing <-sorted_brakingdata[-sorted_brakingdata_indices,]

#b)

slope_b1 <-sum((sorted_brakingdata_training$distanceinmeter-mean(sorted_brakingdata_training$distanceinmeter))
 * (sorted_brakingdata_training$speedinkmhr-mean(sorted_brakingdata_training$speedinkmhr)))/ sum((sorted_brakingdata_training$speedinkmhr-mean(sorted_brakingdata_training$speedinkmhr))^2)

#Value of slope is 0.722643
slope_intercept_b0 <- mean(sorted_brakingdata_training$distanceinmeter) - slope_b1 * mean(sorted_brakingdata_training$speedinkmhr)

#Value of B0 intercept is -5.14755

# 2) a) Braking Distance is
 increasespeed<-5
 BD<-slope_intercept_b0 + slope_b1 *increasespeed

#if speed stays Zero, increase speed <-0
 
 BDatz<-slope_intercept_b0
 
Changeindis <-abs(BDatz - BD) 
 # if speed increases by 5 km/h, then change in breaking distance is 3.613215 metres
  
library(ggplot2)

p <- ggplot(data = sorted_brakingdata_training, aes(x = speedinkmhr, y = distanceinmeter)) +
  geom_point(color = "black", alpha = 0.3) + geom_abline(aes(slope = slope_b1, intercept = slope_intercept_b0, col = "SLR" ), show.legend = FALSE)
  ylim(0, 100)

 +


#Estimate Explained Sum of Squares:
    pred_slr <- slope_intercept_b0 + slope_b1 * sorted_brakingdata_training$speedinkmhr
  
  
  ESS_slr <- sum((pred_slr - mean(sorted_brakingdata_training$distanceinmeter))^2)
  
  TSS <- sum((sorted_brakingdata_training$distanceinmeter - mean(sorted_brakingdata_training$distanceinmeter))^2)
  
  R_Sqaure <-ESS_slr/TSS
  
  #For 95% Confidence Interval, alpha =0.05
  
  alpha <- 0.05
  n <- length(sorted_brakingdata_training$speedinkmhr)
#  nrow(sorted_brakingdata_training)
  t <- qt(1 - alpha/2, n - 1)
  #Value of t is 2.022691
  
  lm.fit <- lm(formula = distanceinmeter ~ speedinkmhr, data = sorted_brakingdata_training)
  summary(lm.fit)$r.squared
  
  lm.fit <- lm(formula = distanceinmeter ~ speedinkmhr, data = sorted_brakingdata)
 lm.fit$coefficients
 
 
 summary(lm.fit)

 
 model <- lm(dist ~ speedinkmhr, data = sorted_brakingdata_training)
 summary(model)
 # speedinkmhr is 2.249 and Intercept is -13.864
 
 se_b1 <- sqrt(1/(n - 2) * (TSS - ESS_slr) / sum((sorted_brakingdata_training$speedinkmhr - mean(sorted_brakingdata_training$speedinkmhr))^2))
 
 
 # 2 d) for speed = 30km/hr
 #we already have
 BD<-slope_intercept_b0 + slope_b1 *increasespeed
predictedBDat30<- slope_intercept_b0 + slope_b1 *30
 #Predicted Braking distance at 30km/hr is 16.33 Metres


# to calculate RSE
RSE <- sqrt(sum(residuals^2) / (n - 2))
residuals<-
  
  residuals <- sorted_brakingdata_training$distanceinmeter - (slope_intercept_b0 + slope_b1 * 30)

RSE <- sqrt(sum(residuals^2) / (n - 2))

SE_pred <- RSE * sqrt(1 + (1 / n) + (mean(sorted_brakingdata_testing$speedinkmhr) - 30)^2 / slope_b1)

# for 80% prediction model, alpha =0.20
newalpha<-0.20
newtvalue <-qt(1 - newalpha / 2,df= n - 2)


#Now calcualte the prediction interval

MOE <-newtvalue * SE_pred
Lower_bound <-predictedBDat30 - MOE

Upper_bound <-predictedBDat30 + MOE

#3 fit a K-NN model
View(sorted_brakingdata_testing)
library(dbplyr)
library(dplyr)
kNN40 <- function(x){
  neighborhood_vals <- sorted_brakingdata_training %>% 
    # find the speed difference between new speed and each existing speed
    mutate(nearspeeddiff = abs(speedinkmhr-x)) %>% 
    # sort the speed difference order 
    arrange(nearspeeddiff) %>% 
    # subset the first k rows
    slice(1:40) %>% 
    # distance in meters is our target response
    select(distanceinmeter) 
  
  # take the average response as the estimate
  return (sum(neighborhood_vals)/40)
}

kNN40(30)

#Using K-NN Model, the braking distance for a car going at 30 km/h is 13.31214

# 4)

# p is the plot with the scatter layer
p + 
  # add a layer for kNN21 model
  geom_function(fun = function(x) {sapply(x, kNN40)}, aes(col = "k-NN")) +
  # add a layer for SLR model
  geom_abline(aes(slope = slope_b1, intercept = slope_intercept_b0, col = "SLR" ), show.legend = TRUE) +
  # add a legend for description
  scale_colour_manual(name = "Model fit", values = c("red", "green"))


# TSS
pred_knn <- sapply(X = sorted_brakingdata_training$speedinkmhr, FUN = kNN40) 


ESS_knn <- sum((pred_knn - mean(sorted_brakingdata_training$speedinkmhr))^2)

RSS_Knn <-ESS_knn/TSS # RSS for KNN is 2.965833

R_Sqaure  # RSS for SLR is 0.6402279

# Now calculate MSE on Training set


pred_test_knn <- sapply(X = sorted_brakingdata_testing$speedinkmhr, FUN = kNN40)

MSE_knn = mean((pred_test_knn - sorted_brakingdata_testing$distanceinmeter)^2)


pred_test_slr <- slope_intercept_b0 + slope_b1 * sorted_brakingdata_testing$speedinkmhr

MSE_slr = mean((sorted_brakingdata_testing$distanceinmeter - pred_test_slr)^2)

