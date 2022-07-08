options(scipen=999) # To remove scientific notation
data <- read.csv('/Users/namithamariacherian/Projects_R/regression/housing.csv')
View(data)
head(data)

#Data Partition
P1 <- runif(nrow(data))
P1
P2 <- order(P1)
P2

training_data <- data[P2[1:300],]
test_data <- data[P2[301:nrow(data)],]

View(training_data)
View(test_data)

#Training the model
model <- lm(MEDV~RM+LSTAT+PTRATIO, data=training_data)
summary(model)
model_1 <- lm(MEDV~LSTAT, data=training_data)
summary(model_1)

#Visualization
plot(training_data$LSTAT,training_data$MEDV, 
     xlab='% of lower status in population', 
     ylab='Value of owner-occupied homes', 
     main='House Value to %of lower status in Population', size=1)
abline(model_1, col='red')
hist(model$residuals, 
     xlab='Model Residual Values', 
     ylab='Density')


#Prediction
new_data <- predict(model, newdata = test_data)
new_data
test_data$Predicted_Home_Value= new_data
head(test_data)

#Plotting test_data and Predicted Data
plot(test_data$Predicted_Home_Value, test_data$MEDV,
     xlab='Predicted Home Value', ylab='Observed Home Value',
     main='Predicted v/s Actual Home Value')
