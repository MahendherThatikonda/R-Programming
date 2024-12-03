x<-5
x

getwd()

Phillipincome<-read.csv("income.csv")

View(Phillipincome$Total.Household.Income,Phillipincome$Members.with.age.5...17.years.old)

Phillipincomeupd<-Phillipincome[,c("Total.Household.Income","Members.with.age.5...17.years.old")]

View(Phillipincomeupd)
# Fillipina Household Income 
# 1)Remove unnecessary columns and rename the features name
colnames(Phillipincomeupd)<-c("income","children")

#2)

set.seed(1)
n<-nrow(Phillipincomeupd)
Phillipincomeupd_indices<-sample(1:n,size=0.8*n)
Phillipincomeupd_training<-Phillipincomeupd[Phillipincomeupd_indices,]
nrow(Phillipincomeupd_training)
# no. of rows in training dataset is 33235
Phillipincomeupd_testing<-Phillipincomeupd[-Phillipincomeupd_indices,]
nrow(Phillipincomeupd_testing)
# no. of rows in testing dataset is 8309

# 3) Perform Linear Regression of type income = b0 + b1 * children

#a) what are b0 and b1

#we have slope b1 =
b1 <- sum((Phillipincomeupd_training$children - mean(Phillipincomeupd_training$children)) * (Phillipincomeupd_training$income - mean(Phillipincomeupd_training$income)))/
  sum((Phillipincomeupd_training$children - mean(Phillipincomeupd_training$children))^2)

# The value of slope is 101518.66

b0 <- mean(Phillipincomeupd_training$income) - b1*mean(Phillipincomeupd_training$children)

# The value of b0 is 261520.2

# b)

# Fit the linear model
Linear_modl <- lm(income ~children, data = Phillipincomeupd_training)


# Create a new data frame with n from 0 to 8
no_data_ <- data.frame(children = 0:8)

# Predict mean income and 90% prediction intervals
Prediction_1 <- predict(Linear_modl, newdata = no_data_, interval = "prediction", level = 0.90)

# Combine the predictions with the new_data
result <- cbind(no_data_, Prediction_1)

print(result)

# within_interval <- with(result, income >= lwr & income <= upr)

predictions_test <- predict(Linear_modl, newdata = Phillipincomeupd_testing, interval = "prediction", level = 0.90)


test_results <- cbind(Phillipincomeupd_testing, predictions_test)


within_interval <- with(test_results, income >= lwr & income <= upr)


percent_within_interval <- mean(within_interval) * 100

# c) 95.23 percent of datapoints lie within 90% interval

# d) Do all steps of part 3. again, but this time you will be predicting log_income = log(income) instead of income.

Phillipincomeupd_training$logincome<-log(Phillipincomeupd_training$income)
Phillipincomeupd_testing$logincome<-log(Phillipincomeupd_testing$income)


View(Phillipincomeupd_training)
# Fit the linear model
log_Linear_modl <- lm(logincome ~children, data = Phillipincomeupd_training)


# Create a new data frame with n from 0 to 8
no_data_ <- data.frame(children = 0:8)

# Predict mean income and 90% prediction intervals
Prediction_log <- predict(log_Linear_modl, newdata = no_data_, interval = "prediction", level = 0.90)

# Combine the predictions with the new_data
result_log <- cbind(no_data_, Prediction_log)

print(result_log)

# within_interval <- with(result, income >= lwr & income <= upr)

predictions_test_log <- predict(log_Linear_modl, newdata = Phillipincomeupd_testing, interval = "prediction", level = 0.90)

View(Phillipincomeupd_testing)
test_results_log <- cbind(Phillipincomeupd_testing, predictions_test_log)


within_interval_log <- with(test_results_log, income >= lwr & income <= upr)


percent_within_interval_log <- mean(within_interval_log) * 100

# d) 0 percent of datapoints lie within 90% interval
