########################################
##########ECON 494 Project II###########
######Predictive Analytics Project######
########################################

library(tseries)# Loads the tseries package for the jarque-bera test.

setwd('C:/Users/Bengtson/ECON-494-Project-I-Submission-F20')# Need to set 
# the working directory to where the TIDY dataset from Project I is located. 
getwd()# Used to ensure R is accessing files from the correct location. 

TIDYdata <- read.csv('C:/Users/Bengtson/ECON-494-Project-I-Submission-F20\\TIDY Data.csv')
# Loads TIDY dataset from Project I into R and stores it into variable TIDYdata.
View(TIDYdata)# Can view the dataset loaded into R. The observation row was
# included in the dataset when loaded into R so lets remove it.
TIDYdata <- subset(TIDYdata, select = -c(X)) # Removes the column X from the 
# dataset.
colnames(TIDYdata)<-c('Quarter', 'Year', 'AA Adj Close', 'AAPL Adj Close',
                       'MO Adj Close', 'F Adj Close', 'GVA Adj Close', 'JPM Adj Close', 'WMT Adj Close', 
                       'MSFT Adj Close', 'S&P 500 Adj Close', 'AA % Change', 'AAPL % Change', 
                       'MO % Change', 'F % Change', 'GVA % Change', 'JPM % Change', 'WMT % Change',
                       'MSFT % Change', 'S&P 500 % Change', 'CPI', 'Interest Rate', 'Unemployment Rate', 
                       'Short Term NROU', 'Long Term NROU', 'GDP (billions)')
View(TIDYdata)# View the dataset to ensure it is ready to be manipulated.
dim(TIDYdata)

# I will pick four of the most important relationships analyzed in Project I
# and build regression models on these relationships. Then, I will pick two of
# the models with the highest significance and r-squared values to then train
# and test the data. Ultimately, there will be two separate relationships that I 
# will build training and testing models for prediction. 

# Regressions for each stock percent change against percent change of the S&P
# 500. 
# Alcoa (AA):
MODEL1<-lm(`AA % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL1) # Multiple R-squared is 46.43% and Adjusted R-squared is 45.98%.
# Altria (MO):
MODEL2<-lm(`MO % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL2) # Multiple R-squared is 9.34% and Adjusted R-squared is 8.57%.
# Ford (F):
MODEL3<-lm(`F % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL3) # Multiple R-squared is 34.08% and Adjusted R-squared is 33.52%.
# Granite Construction (GVA):
MODEL4<-lm(`GVA % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL4) # Multiple R-squared is 8.23% and Adjusted R-squared is 7.45%.
# Walmart (WMT):
MODEL5<-lm(`WMT % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL5) # Multiple R-squared is 19.58% and Adjusted R-squared is 18.90%.
# JP Morgan (JPM):
MODEL6<-lm(`JPM % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL6) # Multiple R-squared is 37.34% and Adjusted R-squared is 36.81%.
# Apple (AAPL):
MODEL7<-lm(`AAPL % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL7) # Multiple R-squared is 18.33% and Adjusted R-squared is 17.63%.
# Microsoft (MSFT):
MODEL8<-lm(`MSFT % Change` ~ `S&P 500 % Change`, TIDYdata)
summary(MODEL8) # Multiple R-squared is 42.11% and Adjusted R-squared is 41.62%.

# Let's take the models above a little bit further with Alcoa, Ford, JP Morgan,
# and Microsoft. 
# Alcoa (AA):
MODEL1.1<-lm(`AA % Change` ~ `S&P 500 % Change` + `Unemployment Rate`, TIDYdata)
summary(MODEL1.1)# Multiple R-squared is 46.58% and Adjusted R-squared is 45.67%.
# Adding unemployment rate did not change much and it is not a significant 
# variable. The adjusted r-squared actually decreased. From exploration in 
# Project I, other than S&P 500 percent change, Alcoa's strongest relationship 
# was with the unemployment rate. 
# Ford (F):
MODEL3.1<-lm(`F % Change` ~ `S&P 500 % Change` + `GDP (billions)`, TIDYdata)
summary(MODEL3.1)# Multiple R-squared is 38.47% and Adjusted R-squared is 37.42%.
# Adding the GDP increased the multiple r-squared and adjusted r-squared values.
# GDP is statistically significant at the 1% level. Other than with the S&P 500
# percent change, Ford's strongest relationship was with GDP. 
# JP Morgan (JPM): 
MODEL6.1<-lm(`JPM % Change` ~ `S&P 500 % Change` + `GDP (billions)`, TIDYdata)
summary(MODEL6.1)# Multiple R-squared is 38.15% and Adjusted R-squared is 37.09%.
# In Project I, there appeared to be a strong relationship between JP Morgan and
# GDP, but adding GDP to the regression barely increased the multiple r-squared
# and adjusted r-squared values. Additionally, GDP was not statistically 
# significant in the equation.
MODEL6.2<-lm(`JPM % Change` ~ `S&P 500 % Change` + `Interest Rate`, TIDYdata)
summary(MODEL6.2)# Multiple R-squared is 37.42% and Adjusted R-squared is 36.35%.
# In Project I, there appeared to be a moderate relationship between JP Morgan and
# interest rates, but adding them to the regression barely increased the multiple r-squared
# and adjusted r-squared values. Additionally, interest rate was not statistically 
# significant in the equation. GDP increase the multiple r-squared and adjusted r-squared
# values more than interest rate.
# Microsoft (MSFT):
MODEL8.1<-lm(`MSFT % Change` ~ `S&P 500 % Change` + `GDP (billions)`, TIDYdata)
summary(MODEL8.1) # Multiple R-squared is 42.51% and Adjusted R-squared is 41.52%.
# In Project I, there appeared to be a strong relationship between Microsoft and
# GDP, but adding GDP to the regression barely increased the multiple r-squared
# and decreased the adjusted r-squared value. Additionally, GDP was not statistically 
# significant in the equation.

# Moving forward, I will build five models to train and then test the percent 
# change of Alcoa, using percent change of the S&P 500 and unemployment rate. I
# will then build five models to train and test the percent change of Microsoft,
# using percent change of the S&P 500 and GDP.

# Incorporating nonlinear transformations of S&P 500 % Change: 
TIDYdata$`S&P 500 % Change_2` <- TIDYdata$`S&P 500 % Change`^2 
# 2nd order quadratic transformation. 
TIDYdata$`S&P 500 % Change_3` <- TIDYdata$`S&P 500 % Change`^3 
# 3rd order cubic transformation.
TIDYdata$`S&P 500 % Change_4` <- TIDYdata$`S&P 500 % Change`^4
# 4th order quartic transformation.
View(TIDYdata)
dim(TIDYdata)

# Separation of data into training and testing partitions to build models for
# testing:
dp <- .7# 70% of sample will be used for training. 

obscount <- dim(TIDYdata)[1]# Storing the number of rows (observations) into 
# variable obscount. 

trainsize <- floor(dp * obscount)# Takes the number of observations to be 
# selected for the training partition, using floor() which rounds down to the 
# nearest integer. 
trainsize

set.seed(123)# Using set.seed() to make the training partition reproducible.

trainvec <- sample(obscount, size = trainsize) # Creating a vector with the
# shuffled row numbers of the original dataset.
View(trainvec)

Training <- TIDYdata[trainvec, ] # Pulls random rows for training.
Testing <- TIDYdata[-trainvec, ] # Pulls random rows for testing.

# Checking the dimensions of the partitioned data.
dim(Training)
dim(Testing)
# Everything checks out: training data has 84 observations (70% of the original
# 120 observations) and 29 columns; and testing data has 36 rows (30% of the 
# original 120 observations) and 29 columns.

# Regression models with the percent change of the stock as the y-variable:
# Alcoa (AA):
# Plotting the training and testing partitions of the data:
plot(`AA % Change` ~ `S&P 500 % Change`, TIDYdata, xlim=c(-.008,.008), ylim=c(-.005, .008))
# Plotting the entire dataset. 
plot(`AA % Change` ~ `S&P 500 % Change`, Training, xlim=c(-.008,.008), ylim=c(-.005, .008), col ='blue')
# Plots the in-sample training partition.
plot(`AA % Change` ~ `S&P 500 % Change`, Testing, xlim=c(-.008,.008), ylim=c(-.005, .008),  col ='red', pch=2) 
# Plots the out-of-sample testing partition.
points(Training$`AA % Change`, Training$`S&P 500 % Change`, col='blue') 
# Plots the in sample building partition.
points(Testing$`AA % Change`, Testing$`S&P 500 % Change`, col='red', pch=2) 
# Plots the out-of-sample testing partition.

# Building a model from the in-sample training data:
M1AA <- lm(`AA % Change` ~ `S&P 500 % Change`, Training)
# M1AA: AA % Change = B0 + B1*S&P 500 % Change + u
summary(M1AA)# Multiple R-squared is 52.84% and Adjusted R-squared is 52.26%.

PRED1INAA <- predict(M1AA, Training)# Generates predictions on the (in-sample) 
# training data from M1AA. 
View(PRED1INAA)
View(M1AA$fitted.values)# These are the same as the fitted values run in the line before.  

M1AA$coefficients# Returns the beta estimates.
M1AA$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M1AA$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M1AA$residuals)
# The residuals are not normally distributed because the test for normality
# returns a p-value less than 5% so reject the null hypothesis that the 
# residuals are distributed normally at the 95% level of significance.

# Are the residuals correlated with other regressors?
plot(M1AA$residuals ~ M1AA$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model. 

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED1OUTAA <- predict(M1AA, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE1INAA<-sqrt(sum((PRED1INAA-Training$`AA % Change`)^2)/length(PRED1INAA))# Computes in-sample error.
RMSE1OUTAA<-sqrt(sum((PRED1OUTAA-Testing$`AA % Change`)^2)/length(PRED1OUTAA))# Computes out-of-sample error. 

RMSE1INAA# In-sample error.
RMSE1OUTAA# Out-of-sample error.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
predictions <- predict(M1AA, list(`S&P 500 % Change`=xgrid))
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a quadratic model from the in-sample training data:
M2AA <- lm(`AA % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2`, Training)
# M2AA: AA % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + u
summary(M2AA)# Multiple R-squared is 53.40% and Adjusted R-squared is 52.25%.

PRED2INAA <- predict(M2AA, Training)# Generates predictions on the (in-sample) 
# training data from M2AA. 
View(PRED2INAA)
View(M2AA$fitted.values)# These are the same as the fitted values run in the line before. 

M2AA$coefficients# Returns the beta estimates.
M2AA$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M2AA$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M2AA$residuals)
# The residuals are normally distributed because the test for normality
# returns a p-value more than 5% so fail to reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M2AA$residuals ~ M2AA$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model. 

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED2OUTAA <- predict(M2AA, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE2INAA<-sqrt(sum((PRED2INAA-Training$`AA % Change`)^2)/length(PRED2INAA))# Computes in-sample error.
RMSE2OUTAA<-sqrt(sum((PRED2OUTAA-Testing$`AA % Change`)^2)/length(PRED2OUTAA))# Computes out-of-sample error. 

RMSE2INAA# In-sample error.
RMSE2OUTAA# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INAA, RMSE2INAA)
c(RMSE1OUTAA,RMSE2OUTAA)
# M2AA is better with in-sample prediction, and M1AA is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
predictions <- predict(M2AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2))
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a cubic model from the in-sample training data:
M3AA <- lm(`AA % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3`, Training)
# M3AA: AA % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + 
# + B3*S&P 500 % Change^3 + u
summary(M3AA)# Multiple R-squared is 57.13% and Adjusted R-squared is 55.52%.

PRED3INAA <- predict(M3AA, Training)# Generates predictions on the (in-sample) 
# training data from M3AA. 
View(PRED3INAA)
View(M3AA$fitted.values)# These are the same as the fitted values run in the line before. 

M3AA$coefficients# Returns the beta estimates.
M3AA$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M3AA$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M3AA$residuals)
# The residuals are not normally distributed because the test for normality
# returns a p-value less than 5% so reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M3AA$residuals ~ M3AA$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED3OUTAA <- predict(M3AA, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE3INAA<-sqrt(sum((PRED3INAA-Training$`AA % Change`)^2)/length(PRED3INAA))# Computes in-sample error.
RMSE3OUTAA<-sqrt(sum((PRED3OUTAA-Testing$`AA % Change`)^2)/length(PRED3OUTAA))# Computes out-of-sample error. 

RMSE3INAA# In-sample error.
RMSE3OUTAA# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INAA, RMSE2INAA, RMSE3INAA)
c(RMSE1OUTAA,RMSE2OUTAA, RMSE3OUTAA)
# M3AA is better with in-sample prediction, and M3AA is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
predictions <- predict(M3AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3))
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a quartic model from the in-sample training data:
M4AA <- lm(`AA % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3` + `S&P 500 % Change_4`, Training)
# M4AA: AA % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + 
# + B3*S&P 500 % Change^3 + B4*S&P 500 % Change^4 + u
summary(M4AA)# Multiple R-squared is 57.31% and Adjusted R-squared is 55.15%.

PRED4INAA <- predict(M4AA, Training)# Generates predictions on the (in-sample) 
# training data from M4AA. 
View(PRED4INAA)
View(M4AA$fitted.values)# These are the same as the fitted values run in the line before. 

M4AA$coefficients# Returns the beta estimates.
M4AA$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M4AA$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M4AA$residuals)
# The residuals are normally distributed because the test for normality
# returns a p-value more than 5% so fail to reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M4AA$residuals ~ M4AA$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED4OUTAA <- predict(M4AA, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE4INAA<-sqrt(sum((PRED4INAA-Training$`AA % Change`)^2)/length(PRED4INAA))# Computes in-sample error.
RMSE4OUTAA<-sqrt(sum((PRED4OUTAA-Testing$`AA % Change`)^2)/length(PRED4OUTAA))# Computes out-of-sample error. 

RMSE4INAA# In-sample error.
RMSE4OUTAA# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INAA, RMSE2INAA, RMSE3INAA, RMSE4INAA)
c(RMSE1OUTAA,RMSE2OUTAA, RMSE3OUTAA, RMSE4OUTAA)
# M4AA is better with in-sample prediction, and M4AA is better with out-of-sample
# prediction. 

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
predictions <- predict(M4AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3, `S&P 500 % Change_4`=xgrid^4))
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a model from the in-sample training data using unemployment rate:
M5AA <- lm(`AA % Change` ~ `S&P 500 % Change` + `Unemployment Rate`, Training)
# M5AA: AA % Change = B0 + B1*S&P 500 % Change + B2*Unemployment Rate + u
summary(M5AA)# Multiple R-squared is 52.87% and Adjusted R-squared is 51.71%.

PRED5INAA <- predict(M5AA, Training)# Generates predictions on the (in-sample) 
# training data from M5AA. 
View(PRED5INAA)
View(M5AA$fitted.values)# These are the same as the fitted values run in the line before. 

M5AA$coefficients# Returns the beta estimates.
M5AA$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M5AA$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M5AA$residuals)
# The residuals are not normally distributed because the test for normality
# returns a p-value less than 5% so reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M5AA$residuals ~ M5AA$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED5OUTAA <- predict(M5AA, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE5INAA<-sqrt(sum((PRED5INAA-Training$`AA % Change`)^2)/length(PRED5INAA))# Computes in-sample error.
RMSE5OUTAA<-sqrt(sum((PRED5OUTAA-Testing$`AA % Change`)^2)/length(PRED5OUTAA))# Computes out-of-sample error. 

RMSE5INAA# In-sample error.
RMSE5OUTAA# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INAA, RMSE2INAA, RMSE3INAA, RMSE4INAA, RMSE5INAA)
c(RMSE1OUTAA, RMSE2OUTAA, RMSE3OUTAA, RMSE4OUTAA, RMSE5OUTAA)
# M4AA is better with in-sample prediction, and M4AA is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
predictions <- predict(M5AA, list(`S&P 500 % Change`=xgrid, `Unemployment Rate`=xgrid))
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Model Comparison:
# Comparison of in-sample model performance by RMSE:
RMSE1INAA# Model with only the linear term.
RMSE2INAA# Model with linear and quadratic term.
RMSE3INAA# Model with linear, quadratic, and cubic term.
RMSE4INAA# Model with the linear, quadratic, cubic, and quartic term.
RMSE5INAA# Model with two linear terms. 
# Model four is best for fitting the data. 

# Comparison of out-of-sample model performance by RMSE:
RMSE1OUTAA# Model with only the linear term.
RMSE2OUTAA# Model with linear and quadratic term.
RMSE3OUTAA# Model with linear, quadratic, and cubic term.
RMSE4OUTAA# Model with the linear, quadratic, cubic, and quartic term.
RMSE5OUTAA# Model with two linear terms. 
# Model four is best for predicting the data.

# Plotting the regression models against one another: 
xgrid <- seq(-.03,.03,.001)# Creating a grid of x-values.
plot(Training$`AA % Change` ~ Training$`S&P 500 % Change`, col='blue')
predictions1AA <- predict(M1AA, list(`S&P 500 % Change`=xgrid))
predictions2AA <- predict(M2AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2))
predictions3AA <- predict(M3AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3))
predictions4AA <- predict(M4AA, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3, `S&P 500 % Change_4`=xgrid^4))
predictions5AA <- predict(M5AA, list(`S&P 500 % Change`=xgrid, `Unemployment Rate`=xgrid))
lines(xgrid, predictions1AA, col='darkgreen', lwd=3)# Plots M1AA
lines(xgrid, predictions2AA, col='green', lwd=3)# Plots M2AA
lines(xgrid, predictions3AA, col='lightgreen', lwd=3)# Plots M3AA
lines(xgrid, predictions4AA, col='orange', lwd=3)# Plots M4AA
lines(xgrid, predictions5AA, col='darkorange', lwd=3)# Plots M5AA
points(Testing$`AA % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# A similar process of model building and testing will be done for Microsoft 
# percent change as the constant y-variable.

# Regression models with the percent change of the stock as the y-variable:
# Microsoft (MSFT):
# Plotting the training and testing partitions of the data:
plot(`MSFT % Change` ~ `S&P 500 % Change`, TIDYdata, xlim=c(-.008,.01), ylim=c(-.005, .008))
# Plotting the entire dataset. 
plot(`MSFT % Change` ~ `S&P 500 % Change`, Training, xlim=c(-.008,.01), ylim=c(-.005, .008), col ='blue')
# Plots the in-sample training partition.
plot(`MSFT % Change` ~ `S&P 500 % Change`, Testing, xlim=c(-.008,.01), ylim=c(-.005, .008),  col ='red', pch=2) 
# Plots the out-of-sample testing partition.
points(Training$`MSFT % Change`, Training$`S&P 500 % Change`, col='blue') 
# Plots the in sample building partition.
points(Testing$`MSFT % Change`, Testing$`S&P 500 % Change`, col='red', pch=2) 
# Plots the out-of-sample testing partition.

# Building a model from the in-sample training data: 
M1MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change`, Training)
# M1MSFT: MSFT % Change = B0 + B1*S&P 500 % Change + u
summary(M1MSFT)# Multiple R-squared is 44.91% and Adjusted R-squared is 44.24%.

PRED1INMSFT <- predict(M1MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M1MSFT. 
View(PRED1INMSFT)
View(M1MSFT$fitted.values)# These are the same as the fitted values run in the line before.  

M1MSFT$coefficients# Returns the beta estimates.
M1MSFT$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M1MSFT$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M1MSFT$residuals)
# The residuals are normally distributed because the test for normality
# returns a p-value more than 5% so fail to reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M1MSFT$residuals ~ M1MSFT$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED1OUTMSFT <- predict(M1MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE1INMSFT <- sqrt(sum((PRED1INMSFT-Training$`MSFT % Change`)^2)/length(PRED1INMSFT))# Computes in-sample error.
RMSE1OUTMSFT <- sqrt(sum((PRED1OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED1OUTMSFT))# Computes out-of-sample error. 

RMSE1INMSFT# In-sample error.
RMSE1OUTMSFT# Out-of-sample error.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
predictions <- predict(M1MSFT, list(`S&P 500 % Change`=xgrid))
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a quadratic model from the in-sample training data:
M2MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2`, Training)
# M2MSFT: MSFT % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + u
summary(M2MSFT)# Multiple R-squared is 44.92% and Adjusted R-squared is 43.56%.

PRED2INMSFT <- predict(M2MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M2MSFT. 
View(PRED2INMSFT)
View(M2MSFT$fitted.values)# These are the same as the fitted values run in the line before. 

M2MSFT$coefficients# Returns the beta estimates.
M2MSFT$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M2MSFT$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M2MSFT$residuals)
# The residuals are not normally distributed because the test for normality
# returns a p-value less than 5% so reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M2MSFT$residuals ~ M2MSFT$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED2OUTMSFT <- predict(M2MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE2INMSFT <- sqrt(sum((PRED2INMSFT-Training$`MSFT % Change`)^2)/length(PRED2INMSFT))# Computes in-sample error.
RMSE2OUTMSFT <- sqrt(sum((PRED2OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED2OUTMSFT))# Computes out-of-sample error. 

RMSE2INMSFT# In-sample error.
RMSE2OUTMSFT# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INMSFT, RMSE2INMSFT)
c(RMSE1OUTMSFT, RMSE2OUTMSFT)
# M1MSFT is better with in-sample prediction, and M2MSFT is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
predictions <- predict(M2MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2))
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a cubic model from the in-sample training data:
M3MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3`, Training)
# M3MSFT: MSFT % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + 
# + B3*S&P 500 % Change^3 + u
summary(M3MSFT)# Multiple R-squared is 45.38% and Adjusted R-squared is 43.33%.

PRED3INMSFT <- predict(M3MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M3MSFT. 
View(PRED3INMSFT)
View(M3MSFT$fitted.values)# These are the same as the fitted values run in the line before. 

M3MSFT$coefficients# Returns the beta estimates.
M3MSFT$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M3MSFT$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M3MSFT$residuals)
# The residuals are not normally distributed because the test for normality
# returns a p-value less than 5% so reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M3MSFT$residuals ~ M3MSFT$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED3OUTMSFT <- predict(M3MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE3INMSFT <- sqrt(sum((PRED3INMSFT-Training$`MSFT % Change`)^2)/length(PRED3INMSFT))# Computes in-sample error.
RMSE3OUTMSFT <- sqrt(sum((PRED3OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED3OUTMSFT))# Computes out-of-sample error. 

RMSE3INMSFT# In-sample error.
RMSE3OUTMSFT# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INMSFT, RMSE2INMSFT, RMSE3INMSFT)
c(RMSE1OUTMSFT,RMSE2OUTMSFT, RMSE3OUTMSFT)
# M3MSFT is better with in-sample prediction, and M3MSFT is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
predictions <- predict(M3MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3))
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a quartic model from the in-sample training data:
M4MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3` + 
               `S&P 500 % Change_4`, Training)
# M4MSFT: MSFT % Change = B0 + B1*S&P 500 % Change + B2*S&P 500 % Change^2 + 
# + B3*S&P 500 % Change^3 + B4*S&P 500 % Change^4 + u
summary(M4MSFT)# Multiple R-squared is 46% and Adjusted R-squared is 43.26%.

PRED4INMSFT <- predict(M4MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M4MSFT. 
View(PRED4INMSFT)
View(M4MSFT$fitted.values)# These are the same as the fitted values run in the line before. 

M4MSFT$coefficients# Returns the beta estimates.
M4MSFT$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M4MSFT$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M4MSFT$residuals)
# The residuals are normally distributed because the test for normality
# returns a p-value more than 5% so fail to reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M4MSFT$residuals ~ M4MSFT$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED4OUTMSFT <- predict(M4MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE4INMSFT <- sqrt(sum((PRED4INMSFT-Training$`MSFT % Change`)^2)/length(PRED4INMSFT))# Computes in-sample error.
RMSE4OUTMSFT <- sqrt(sum((PRED4OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED4OUTMSFT))# Computes out-of-sample error. 

RMSE4INMSFT# In-sample error.
RMSE4OUTMSFT# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INMSFT, RMSE2INMSFT, RMSE3INMSFT, RMSE4INMSFT)
c(RMSE1OUTMSFT,RMSE2OUTMSFT, RMSE3OUTMSFT, RMSE4OUTMSFT)
# M4MSFT is better with in-sample prediction, and M3MSFT is better with out-of-sample
# prediction. 

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
predictions <- predict(M4MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3, `S&P 500 % Change_4`=xgrid^4))
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Building a model from the in-sample training data using interest rates:
M5MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `GDP (billions)`, Training)
# M5MSFT: MSFT % Change = B0 + B1*S&P 500 % Change + B2*GDP + u 
summary(M5MSFT)# Multiple R-squared is 45.55% and Adjusted R-squared is 44.20%.

PRED5INMSFT <- predict(M5MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M5MSFT. 
View(PRED5INMSFT)
View(M5MSFT$fitted.values)# These are the same as the fitted values run in the line before. 

M5MSFT$coefficients# Returns the beta estimates.
M5MSFT$residuals# Returns the residual values.

# Are the residuals for this model normal?
hist(M5MSFT$residuals)# Use a histogram to plot the residuals for the purpose of
# visualization. 
jarque.bera.test(M5MSFT$residuals)
# The residuals are normally distributed because the test for normality
# returns a p-value more than 5% so fail to reject the null hypothesis that the 
# residuals are distributed normally at the 95%  level of significance.

# Are the residuals correlated with other regressors?
plot(M5MSFT$residuals ~ M5MSFT$fitted.values) + abline(0,0, col='blue')
# In the plot, it can be seen that the residuals are centered on zero throughout
# the range of the fitted values. The residuals do not possess any explanatory 
# or predictive power, therefore, they are not correlated with any of the other
# regressors in the model.

# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED5OUTMSFT <- predict(M5MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.

# Computing the in-sample and out-of-sample root mean squared error.
RMSE5INMSFT<-sqrt(sum((PRED5INMSFT-Training$`MSFT % Change`)^2)/length(PRED5INMSFT))# Computes in-sample error.
RMSE5OUTMSFT<-sqrt(sum((PRED5OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED5OUTMSFT))# Computes out-of-sample error. 

RMSE5INMSFT# In-sample error.
RMSE5OUTMSFT# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INMSFT, RMSE2INMSFT, RMSE3INMSFT, RMSE4INMSFT, RMSE5INMSFT)
c(RMSE1OUTMSFT, RMSE2OUTMSFT, RMSE3OUTMSFT, RMSE4OUTMSFT, RMSE5OUTMSFT)
# M4MSFT is better with in-sample prediction, and M3MSFT is better with out-of-sample
# prediction.

# Plotting the model in 2D against both in-sample and out-of-sample data partitions.
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
predictions <- predict(M5MSFT, list(`S&P 500 % Change`=xgrid, `GDP (billions)`=xgrid))
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
lines(xgrid, predictions, col='green', lwd=2)
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# Model Comparison:
# Comparison of in-sample model performance by RMSE:
RMSE1INMSFT# Model with only the linear term.
RMSE2INMSFT# Model with linear and quadratic term.
RMSE3INMSFT# Model with linear, quadratic, and cubic term.
RMSE4INMSFT# Model with the linear, quadratic, cubic, and quartic term.
RMSE5INMSFT# Model with two linear terms.
# Model four is best for fitting the data. 

# Comparison of out-of-sample model performance by RMSE:
RMSE1OUTMSFT# Model with only the linear term.
RMSE2OUTMSFT# Model with linear and quadratic term.
RMSE3OUTMSFT# Model with linear, quadratic, and cubic term.
RMSE4OUTMSFT# Model with the linear, quadratic, cubic, and quartic term.
RMSE5OUTMSFT# Model with two linear terms.
# Model three is best for predicting the data.

# Plotting the regression models against one another: 
xgrid <- seq(-.008,.01,.001)# Creating a grid of x-values.
plot(Training$`MSFT % Change` ~ Training$`S&P 500 % Change`, col='blue')
predictions1MSFT <- predict(M1MSFT, list(`S&P 500 % Change`=xgrid))
predictions2MSFT <- predict(M2MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2))
predictions3MSFT <- predict(M3MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3))
predictions4MSFT <- predict(M4MSFT, list(`S&P 500 % Change`=xgrid, `S&P 500 % Change_2`=xgrid^2, `S&P 500 % Change_3`=xgrid^3, `S&P 500 % Change_4`=xgrid^4))
predictions5MSFT <- predict(M5MSFT, list(`S&P 500 % Change`=xgrid, `GDP (billions)`=xgrid))
lines(xgrid, predictions1MSFT, col='darkgreen', lwd=3)# Plots M1MSFT
lines(xgrid, predictions2MSFT, col='green', lwd=3)# Plots M2MSFT
lines(xgrid, predictions3MSFT, col='lightgreen', lwd=3)# Plots M3MSFT
lines(xgrid, predictions4MSFT, col='orange', lwd=3)# Plots M4MSFT
lines(xgrid, predictions5MSFT, col='darkorange', lwd=3)# Plots M5MSFT
points(Testing$`MSFT % Change` ~ Testing$`S&P 500 % Change`, col='red', pch=2)

# What happens to Microsoft's models when the set.seed is changed when 
# partitioning the data?

# Incorporating nonlinear transformations of S&P 500 % Change: 
TIDYdata$`S&P 500 % Change_2` <- TIDYdata$`S&P 500 % Change`^2 
# 2nd order quadratic transformation. 
TIDYdata$`S&P 500 % Change_3` <- TIDYdata$`S&P 500 % Change`^3 
# 3rd order cubic transformation.
TIDYdata$`S&P 500 % Change_4` <- TIDYdata$`S&P 500 % Change`^4
# 4th order quartic transformation.
View(TIDYdata)
dim(TIDYdata)

# Separation of data into training and testing partitions to build models for
# testing:
dp <- .7# 70% of sample will be used for training. 

obscount <- dim(TIDYdata)[1]# Storing the number of rows (observations) into 
# variable obscount. 

trainsize <- floor(dp * obscount)# Takes the number of observations to be 
# selected for the training partition, using floor() which rounds down to the 
# nearest integer. 
trainsize

set.seed(1234567)# Using set.seed() to make the training partition reproducible.

trainvec <- sample(obscount, size = trainsize) # Creating a vector with the
# shuffled row numbers of the original dataset.
View(trainvec)

Training <- TIDYdata[trainvec, ] # Pulls random rows for training.
Testing <- TIDYdata[-trainvec, ] # Pulls random rows for testing.

# Checking the dimensions of the partitioned data.
dim(Training)
dim(Testing)
# Everything checks out: training data has 84 observations (70% of the original
# 120 observations) and 29 columns; and testing data has 36 rows (30% of the 
# original 120 observations) and 29 columns.

# Building a model from the in-sample training data: 
M1MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change`, Training)
PRED1INMSFT <- predict(M1MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M1MSFT.
# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED1OUTMSFT <- predict(M1MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.
# Computing the in-sample and out-of-sample root mean squared error.
RMSE1INMSFT <- sqrt(sum((PRED1INMSFT-Training$`MSFT % Change`)^2)/length(PRED1INMSFT))# Computes in-sample error.
RMSE1OUTMSFT <- sqrt(sum((PRED1OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED1OUTMSFT))# Computes out-of-sample error. 
RMSE1INMSFT# In-sample error.
RMSE1OUTMSFT# Out-of-sample error.

# Building a quadratic model from the in-sample training data:
M2MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2`, Training)
PRED2INMSFT <- predict(M2MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M2MSFT. 
# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED2OUTMSFT <- predict(M2MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.
# Computing the in-sample and out-of-sample root mean squared error.
RMSE2INMSFT <- sqrt(sum((PRED2INMSFT-Training$`MSFT % Change`)^2)/length(PRED2INMSFT))# Computes in-sample error.
RMSE2OUTMSFT <- sqrt(sum((PRED2OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED2OUTMSFT))# Computes out-of-sample error. 
RMSE2INMSFT# In-sample error.
RMSE2OUTMSFT# Out-of-sample error.

# Building a cubic model from the in-sample training data:
M3MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3`, Training)
PRED3INMSFT <- predict(M3MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M3MSFT. 
# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED3OUTMSFT <- predict(M3MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.
# Computing the in-sample and out-of-sample root mean squared error.
RMSE3INMSFT <- sqrt(sum((PRED3INMSFT-Training$`MSFT % Change`)^2)/length(PRED3INMSFT))# Computes in-sample error.
RMSE3OUTMSFT <- sqrt(sum((PRED3OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED3OUTMSFT))# Computes out-of-sample error. 
RMSE3INMSFT# In-sample error.
RMSE3OUTMSFT# Out-of-sample error.

# Building a quartic model from the in-sample training data:
M4MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `S&P 500 % Change_2` + `S&P 500 % Change_3` + 
               `S&P 500 % Change_4`, Training)
PRED4INMSFT <- predict(M4MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M4MSFT. 
# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED4OUTMSFT <- predict(M4MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.
# Computing the in-sample and out-of-sample root mean squared error.
RMSE4INMSFT <- sqrt(sum((PRED4INMSFT-Training$`MSFT % Change`)^2)/length(PRED4INMSFT))# Computes in-sample error.
RMSE4OUTMSFT <- sqrt(sum((PRED4OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED4OUTMSFT))# Computes out-of-sample error. 
RMSE4INMSFT# In-sample error.
RMSE4OUTMSFT# Out-of-sample error.

# Building a model from the in-sample training data using interest rates:
M5MSFT <- lm(`MSFT % Change` ~ `S&P 500 % Change` + `GDP (billions)`, Training)
PRED5INMSFT <- predict(M5MSFT, Training)# Generates predictions on the (in-sample) 
# training data from M5MSFT. 
# For the purpose of benchmarking, need to generate predictions on the testing data.
PRED5OUTMSFT <- predict(M5MSFT, Testing)# Generates predictions on the (out-of-sample) testing data.
# Computing the in-sample and out-of-sample root mean squared error.
RMSE5INMSFT<-sqrt(sum((PRED5INMSFT-Training$`MSFT % Change`)^2)/length(PRED5INMSFT))# Computes in-sample error.
RMSE5OUTMSFT<-sqrt(sum((PRED5OUTMSFT-Testing$`MSFT % Change`)^2)/length(PRED5OUTMSFT))# Computes out-of-sample error. 
RMSE5INMSFT# In-sample error.
RMSE5OUTMSFT# Out-of-sample error.

# Creating a vector to see difference in error:
c(RMSE1INMSFT, RMSE2INMSFT, RMSE3INMSFT, RMSE4INMSFT, RMSE5INMSFT)
c(RMSE1OUTMSFT, RMSE2OUTMSFT, RMSE3OUTMSFT, RMSE4OUTMSFT, RMSE5OUTMSFT)
