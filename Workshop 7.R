library(forecast)
library(tseries)


walmart <- read.csv("walmart.csv", header = TRUE)

#Convert to quarterly time series starting from Q1 in 1995
walmart <- ts(walmart, frequency = 4, start = c(1995,1))

plot(walmart)


#Scatterplot of Sales and GDP 
plot(as.data.frame(walmart))

#Fit regression with GDP 
fit1 <- lm(Sales ~ L1_GDP, data = walmart)
summary(fit1)

lm1_resid <- residuals(fit1)

lm1_fitted <- fitted(fit1)

tsdisplay(lm1_resid)

#Create seasonal dummies 
Q1 <- rep(c(1,0,0,0),16)
Q2 <- rep(c(0,1,0,0), 16)
Q3 <- rep(c(0,0,1,0), 16)
Q4 <- rep(c(0,0,0,1), 16)

walmart <- cbind(walmart, Q1, Q2, Q3, Q4)

colnames(walmart) <- c("Sales", "L1_GDP","Q1","Q2","Q3","Q4")

#Fit regression with seasonal dummies and GDP
fit2 <- lm(Sales ~. , data = walmart)
summary(fit2)

#Removing columns
#walmart <- walmart[,-7]

#Lag 1 of Sales
L1_Sales <- lag(walmart[,"Sales"],k = -1)

walmart_colnames <- colnames(walmart)
walmart <- cbind(walmart, L1_Sales)
colnames(walmart) <- c(walmart_colnames, "L1_Sales")

colnames(walmart) <- c("Sales", "L1_GDP","Q1","Q2","Q3","Q4", "L1_Sales")

fit3 <- lm(Sales ~. , data = walmart)
summary(fit3)


#Lag 4 of Sales
L2_Sales <- lag(walmart[,"Sales"],-2)
L3_Sales <- lag(walmart[,"Sales"],-3)
L4_Sales <- lag(walmart[,"Sales"],-4)

walmart_colnames <- colnames(walmart)
walmart <- cbind(walmart, L2_Sales, L3_Sales, L4_Sales)
colnames(walmart) <- c(walmart_colnames, "L2_Sales", "L3_Sales", "L4_Sales")

fit4 <- lm(Sales ~ L1_GDP + L1_Sales + L2_Sales + L3_Sales + L4_Sales  , data = walmart)
summary(fit4)


#Correlation matrix for walmart data
cor(walmart, use = "c")
#Pearson Correaltion only measures for continuous variables, so can't beleive correlation 
#between dummy variables and sales 


library(car)

#VIF for fit1
#If less than 30 observations, VIF value exceeding 5 is a signal of high multicollinearity
#If more than 30 observations, VIF value exceeding 10 is a signal of high multicollinearity

vif(fit4)


#Fit regression with seasonality dummies and GDP 
fit6 <- lm(Sales~ Q1 + Q2 + Q3 + Q4 + L1_GDP, data = walmart)
summary(fit6)
tsdisplay(residuals(fit6))


#Fit regression with Seasonally dummies , GDP , and Lag 4 sales
fit7 <- lm(Sales ~ Q1 + Q2 + Q3 + Q4 + L1_GDP + L4_Sales, data = walmart)
summary(fit7)
tsdisplay(residuals(fit7))

arima1 <- arima(x = walmart[,1], order = c(4,0,0),xreg = c(walmart[,2], walmart[,3],walmart[,4],walmart[,5]))


#Create trend 
walmart_trend <- c(1:70)
walmart_colnames <- colnames(walmart)
walmart <- cbind(walmart, walmart_trend)
colnames(walmart) <- c(walmart_colnames, "Trend")



fit8 <- lm(Sales~., data = walmart)
summary(fit8)
tsdisplay(residuals(fit8))


#Create Trend
L1_GDP_Log <- log(walmart[,"L1_GDP"])

walmart_colnames <- colnames(walmart)
walmart <- cbind(walmart, L1_GDP_Log)
colnames(walmart) <- c(walmart_colnames, "L1_GDP_Log")


fit9 <- lm(Sales~., data = walmart)
summary(fit9)
tsdisplay(residuals(fit9))


###Forecasting with Linear Regression

GasData <- read.csv("GasData.csv")

#Convert to a time series
GasData <- ts(GasData, frequency = 12, start = c(1996,1))

#Split the data into train and test sets
GasDataTrain <- window(GasData, start(GasData), (2007+11/12))

GasDataTest <- window(GasData, (2008), end(GasData))

plot(GasData[,"Unleaded"])
plot(GasDataTrain[,"Unleaded"])
plot(GasDataTest[,"Unleaded"])



#Regression with no variables
GasDataModel0 <- lm(Unleaded ~ 1, data = GasDataTrain)

#Regression with all variables
GasDataModel1 <- lm(Unleaded ~ . , data = GasDataTrain)

#Summary of regressions
summary(GasDataModel0)
summary(GasDataModel1)

#AIC Forward Selectoin
GasDataModel2 <- step(GasDataModel0, formula(GasDataModel1), direction = "forward")
summary(GasDataModel2)

#AIC Backwards Selection 
GasDataModel3 <- step(GasDataModel1, formula(GasDataModel0), direction = "backward")
summary(GasDataModel3)

#AIC forward and backwards selction 
GasDataModel4 <- step(GasDataModel0, formula(GasDataModel1), direction= "both")
summary(GasDataModel4)

#Forecast from Model1 
GasDataModel1Forecast <- predict(GasDataModel1, GasDataTest)

Model1_errors <- GasDataTest - GasDataModel1Forecast

Model1_ME <- mean(Model1_errors)
Model1_MSE <- mean(Model1_errors^2)
Model1_MAE <- mean(abs(Model1_errors))
Model1_MAPE <- 100 * mean(abs(Model1_errors)/GasDataTest)

#Forecast 2
GasDataModel2Forecast <- predict(GasDataModel2, GasDataTest)

Model2_errors <- GasDataTest - GasDataModel2Forecast

Model2_ME <- mean(Model2_errors)
Model2_MSE <- mean(Model2_errors^2)
Model2_MAE <- mean(abs(Model2_errors))
Model2_MAPE <- 100 * mean(abs(Model2_errors)/GasDataTest)

#Forecast 3
GasDataModel3Forecast <- predict(GasDataModel3, GasDataTest)

Model3_errors <- GasDataTest - GasDataModel3Forecast

Model3_ME <- mean(Model2_errors)
Model3_MSE <- mean(Model2_errors^2)
Model3_MAE <- mean(abs(Model2_errors))
Model3_MAPE <- 100 * mean(abs(Model2_errors)/GasDataTest)


#Prediction Intervals
#Model 1
GasDataModel1Forecast <- predict(GasDataModel1, GasDataTest, interval = "prediction", level = 0.95)

Model1_errors <- GasDataTest - GasDataModel1Forecast

Model1_ME <- mean(Model1_errors)
Model1_MSE <- mean(Model1_errors^2)
Model1_MAE <- mean(abs(Model1_errors))
Model1_MAPE <- 100 * mean(abs(Model1_errors)/GasDataTest)



#Basic Bivariate Model 
GasDataBivariateModel <- lm(Unleaded ~ Crude_Price, data = GasDataTrain)

#Forecast of Crude Prices
CrudeForecast <- forecast(ets(GasDataTrain[,"Crude_Price"]), h =18)$mean

GasDataTestWithCrude <- as.data.frame(CrudeForecast)
colnames(GasDataTestWithCrude) <- "Crude_Price"

#Forecast from Model 1
GasDataBivariateModelForecast <- predict(GasDataBivariateModel, GasDataTestWithCrude, 
                                         interval = "prediction", level = 0.95)

Model4_errors <- GasDataTest - GasDataBivariateModelForecast[,1]

Model4_ME <- mean(Model4_errors)
Model4_MSE <- mean(Model4_errors^2)
Model4_MAE <- mean(abs(Model4_errors))
Model4_MAPE <- 100 * mean(abs(Model4_errors)/GasDataTest)

#Outlier and Leverage Points

gas.data <- read.csv("GasData.csv", header = TRUE)
#Extract the Y-variable
Unleaded <- gas.data$Unleaded
#Extract the X-variables
L1_Crude <- gas.data$L1_Crude
L1_Unemp <- gas.data$L1_Unemp
L1_S.P <- gas.data$L1_S.P
L1_PDI <- gas.data$L1_PDI
#Fit a regression
fit1 <- lm(Unleaded ~ L1_Crude + L1_Unemp + L1_S.P + L1_PDI)

#Studentisted residuals 
s <- rstandard(fit1)

#Plot studentised residuals 
plot(s)
#add title to plot
title(main = "Studentised Residuals")

#2 Standard Deviation Border
abline(h = -2, col = "red")
abline(h = 2, col = "red")


#3 Standard Deviatoin Border
abline(h = -3, col = "deepskyblue")
abline(h = 3, col = "deepskyblue")

#Studentised Deleted Residuals 
sd <- rstudent(fit1)

#Plot Studentised Deleted Residuals
plot(sd)

#Add title to plot
title(main = "Studentised Deleted Residuals")

#Standard deviation Border
abline(h= -2, col = "blue")
abline(h= 2, col = "blue")

#Standard deviation Border
abline(h= -3, col = "orange")
abline(h= 3, col = "orange")

#Cook's Distance
cook <- cooks.distance(fit1)

#Number of Observations
n <- length(Unleaded)
#Cut-Off Value for Cook's Distance
cook.cut.off <- 4/n

#Plot Cook's Distance
plot(cook, pch = 23, bg = "yellow")
#Add title to plot
title(main = "Cook's Distance")
#Plot Cutt-Off Value
abline(h = cook.cut.off, col = "purple")


#What are the values above the Cut-Off
cook[cook > cook.cut.off]

#Which points are above the cut - off
which(cook>cook.cut.off)


#DFFITS
dff <- dffits(fit1)

#Number of Parameter 
p <- length(names(fit1$coef)) - 1
#DFFITS cutoff value
dff.cut.off <- 2*sqrt(p/n)

#Plot DFFITS
plot(dff, pch = 21, bg = "mediumorchid1")
#Add title to plot
title(main = "DFFITS")
#Plot Cut-Off Value
abline(h = dff.cut.off, col = "darkcyan", lty = "twodash", lwd = 2)

