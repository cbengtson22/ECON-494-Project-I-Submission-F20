#########################
######Data Cleaning######
#########################

# Data cleaning steps taken in excel:
# Compiled CSV data files of 8 stocks, and the S&P 500 into one excel file.
# Compiled CSV data files of 6 economic health indicators into one excel file. 
# I deleted all data entries in my stock data except for data and adjusted close. 
# I deleted all data before Q3 1990, because one of my stock's data sets only
# went back to mid-Q2 1990 (Q3 1990 was the first full quarter of data). 
# I added categorical variable quarter to the economic health data file.
# The economic health data goes until Q2 2020, so I deleted stock data from 
# April 2020 to October 2020.
# At this point, the data in both files ranges from Q3 1990 to Q2 2020, but the
# stock data has observations for each day, while the economic health data has
# observations by quarter. 
# I added variable quarter to stock data using an excel formula, so that the
# data would be easier to aggregate by quarter once in R. 
# Using an equation in excel, I computed the percent change of each stock and
# the S&P 500. 
# In excel, I cleaned up the variables that had data in the percent format and
# made each one go to the hundredths place.  

install.packages('readxl')# Install package in R to read excel files.
library('readxl')# Load the package into the library to run the excel data files.

getwd()# Used to ensure the files are in the proper place on my desktop to allow
# for access in R. 

DCstocks <- read_excel('C:/Users/Bengtson/OneDrive/Desktop\\Data Cleaning 1.xlsx')
View(DCstocks)
# Loads the excel file with my stock data into R. 

DCecon <- read_excel('C:/Users/Bengtson/OneDrive/Desktop\\Data Cleaning 2.xlsx')
View(DCecon)
# Loads the excel file with my economic health data for the U.S. into R. 

# I need to aggregate the stock data by quarter.  
d <- 3 
# Number of months in each period is 3, since there are four quarters a year. 
Year <- with(as.POSIXlt(DCstocks$Date), sprintf("%d", year + 1900)) 
# Changes date variable to display just the year. 
aaAVGByQtr <- aggregate(DCstocks$AA ~ Quarter + Year, data = DCstocks, FUN = mean)
aaplAVGByQtr <- aggregate(DCstocks$AAPL ~ Quarter + Year, data = DCstocks, FUN = mean)
moAVGByQtr <- aggregate(DCstocks$MO ~ Quarter + Year, data = DCstocks, FUN = mean)
fAVGByQtr <- aggregate(DCstocks$F ~ Quarter + Year, data = DCstocks, FUN = mean)
gvaAVGByQtr <- aggregate(DCstocks$GVA ~ Quarter + Year, data = DCstocks, FUN = mean)
jpmAVGByQtr <- aggregate(DCstocks$JPM ~ Quarter + Year, data = DCstocks, FUN = mean)
wmtAVGByQtr <- aggregate(DCstocks$WMT ~ Quarter + Year, data = DCstocks, FUN = mean)
msftAVGByQtr <- aggregate(DCstocks$MSFT ~ Quarter + Year, data = DCstocks, FUN = mean)
sp500AVGByQtr <- aggregate(DCstocks$SP ~ Quarter + Year, data = DCstocks, FUN = mean)
aapchAVGByQtr <- aggregate(DCstocks$AAPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
aaplpchAVGByQtr <- aggregate(DCstocks$AAPLPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
mopchAVGByQtr <- aggregate(DCstocks$MOPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
fpchAVGByQtr <- aggregate(DCstocks$FPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
gvapchAVGByQtr <- aggregate(DCstocks$GVAPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
jpmpchAVGByQtr <- aggregate(DCstocks$JPMPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
wmtpchAVGByQtr <- aggregate(DCstocks$WMTPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
msftpchAVGByQtr <- aggregate(DCstocks$MSFTPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
sp500pchAVGByQtr <- aggregate(DCstocks$SPPCH ~ Quarter + Year, data = DCstocks, FUN = mean)
# Lines 43-60 aggregate each stock by quarter, which yields a data set with the 
# quarter, year, average of the adjusted close price by quarter, and average of 
# the percent change by quarter.
View(aaAVGByQtr) # Can be used to check each aggregation.  

# Now I need to combine the aggregations into one data set. 
a<-cbind(aaAVGByQtr, aaplAVGByQtr[,3])
b<-cbind(a,moAVGByQtr[,3])
c<-cbind(b,fAVGByQtr[,3])
d<-cbind(c,gvaAVGByQtr[,3])
e<-cbind(d,jpmAVGByQtr[,3])
f<-cbind(e,wmtAVGByQtr[,3])
g<-cbind(f,msftAVGByQtr[,3])
h<-cbind(g,sp500AVGByQtr[,3])
i<-cbind(h,aapchAVGByQtr[,3])
j<-cbind(i,aaplpchAVGByQtr[,3])
k<-cbind(j,mopchAVGByQtr[,3])
l<-cbind(k,fpchAVGByQtr[,3])
m<-cbind(l,gvapchAVGByQtr[,3])
n<-cbind(m,jpmpchAVGByQtr[,3])
o<-cbind(n,wmtpchAVGByQtr[,3])
p<-cbind(o,msftpchAVGByQtr[,3])
StockAgg<-cbind(p,sp500pchAVGByQtr[,3])
head(StockAgg) # Check the format of the new data set. 
# Now all of the aggregated data is in one data set StockAgg. 

# I now need to add the economic health columns.
View(DCecon) 
# Economic health data has date in different format than data in StockAgg.
View(StockAgg) 
# BUT observations match up (in terms of date) between DCecon and StockAgg.

# I just need to column bind the economic health data to the stock data. 
TotalData<-cbind(StockAgg,DCecon[,3:8])
# Combining the economic health data to the aggregated stock data.
head(TotalData) #Check the structure of TotalData. 
dim(TotalData) # Check the dimensions of TotalData. 

# I now need to name the columns appropriately.  
colnames(TotalData)<-c('Quarter', 'Year', 'AA Adj Close', 'AAPL Adj Close',
'MO Adj Close', 'F Adj Close', 'GVA Adj Close', 'JPM Adj Close', 'WMT Adj Close', 
'MSFT Adj Close', 'S&P 500 Adj Close', 'AA % Change', 'AAPL % Change', 
'MO % Change', 'F % Change', 'GVA % Change', 'JPM % Change', 'WMT % Change',
'MSFT % Change', 'S&P 500 % Change', 'CPI', 'Interest Rate', 'Unemployment Rate', 
'Short Term NROU', 'Long Term NROU', 'GDP (billions)')
View(TotalData)
# I now have a final data that is TIDY.
# Each variable has its own column and each observation has its own row.

# Change quarter and year variable from character to factor:
TotalData$Quarter <- as.factor(TotalData$Quarter)
TotalData$Year <- as.factor(TotalData$Year)

write.csv(TotalData, 'C:/Users/Bengtson/OneDrive/Desktop\\TIDY Data.csv')
# Export final TIDY data set into a csv file in order to be submitted to
# GitHub data repository.

############################
######Data Exploration######
############################
View(TotalData)# Can see the structure of the TIDY data set that will be used
# to run analysis and exploration.
dim(TotalData)# Computes number of rows (observations) and number of columns
# (variables) in the data set.

head(TotalData)
tail(TotalData)
# Head shows the first 6 observations in the data set and tail shows the last
# 6 observations in the data set.

summary(TotalData)
# Obtains summary statistics for all variables in the data set.

sd(TotalData$`AA Adj Close`)
sd(TotalData$`AAPL Adj Close`)
sd(TotalData$`MO Adj Close`)
sd(TotalData$`F Adj Close`)
sd(TotalData$`GVA Adj Close`)
sd(TotalData$`JPM Adj Close`)
sd(TotalData$`WMT Adj Close`)
sd(TotalData$`MSFT Adj Close`)
sd(TotalData$`S&P 500 Adj Close`)
# The standard deviations of the stock's prices can shed some light on their 
# volatility.

var(TotalData$`AA Adj Close`)
var(TotalData$`AAPL Adj Close`)
var(TotalData$`MO Adj Close`)
var(TotalData$`F Adj Close`)
var(TotalData$`GVA Adj Close`)
var(TotalData$`JPM Adj Close`)
var(TotalData$`WMT Adj Close`)
var(TotalData$`MSFT Adj Close`)
var(TotalData$`S&P 500 Adj Close`)
# The variance of the stock's prices can shed some light on their volatility.

# For each stock, I have created a data set with the stock's adjusted close, the
# stock's percentage change, the S&P 500 adjusted close price, and the economic 
# health data so that I can use the pairs function to see potential relationships 
# between the stock's price, the stock's percentage change, the S&P 500, and the 
# economic health data.
AAECON <- cbind(TotalData$`AA Adj Close`, TotalData$`AA % Change`,
                TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                TotalData[,21:26])
colnames(AAECON) <- c('AA Adj Close', 'AA % Change', 'S&P 500 Adj Close',
                      'S&P 500 % Change', 'CPI', 'Interest Rate',
                      'Unemployment Rate', 'Short Term NROU', 
                      'Long Term NROU', 'GDP (billions)')
head(AAECON)# Used to check the structure of the data after reorganization. 

hist(AAECON$`AA Adj Close`)# Does not appear to be normally distributed.  
hist(AAECON$`AA % Change`)# Potentially normally distributed. 

library(tseries) # Loads the tseries library.
# Conducts a hypothesis test for normality.
jarque.bera.test(AAECON$`AA % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the data is 
# distributed normally.  

pairs(AAECON)# Relationships: Potential positive relationship between AA and
# S&P 500 adjusted close prices, but hard to interpret from the graph. Potential
# negative relationship between AA and unemployment rate (as well as natural
# rate of unemployment in the long and short term), but hard to see from
# the graph. Lastly, potential positive relationship between AA and GDP.  

AAPLECON <- cbind(TotalData$`AAPL Adj Close`, TotalData$`AAPL % Change`, 
                  TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`, 
                  TotalData[,21:26])
colnames(AAPLECON) <- c('AAPL Adj Close', 'AAPL % Change', 'S&P 500 Adj Close', 
                        'S&P 500 % Change','CPI', 'Interest Rate', 'Unemployment Rate', 
                        'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(AAPLECON)# Used to check the structure of the data after reorganization.

hist(AAPLECON$`AAPL Adj Close`)# Does not appear to be normally distributed.  
hist(AAPLECON$`AAPL % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(AAPLECON$`AAPL % Change`)
# Returns p-value more than 5% so fail to reject the null hypothesis that the 
# data is distributed normally.  

pairs(AAPLECON)# Relationships: Apparent positive relationship between AAPL and 
# S&P 500; and also AAPL and GDP. Apparent negative relationships between AAPL 
# and unemployment rate (including NROU short and long term).

MOECON <- cbind(TotalData$`MO Adj Close`, TotalData$`MO % Change`,
                TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                TotalData[,21:26])
colnames(MOECON) <- c('MO Adj Close', 'MO % Change', 'S&P 500 Adj Close',
                      'S&P 500 % Change','CPI','Interest Rate', 'Unemployment Rate', 
                      'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(MOECON)# Used to check the structure of the data after reorganization.

hist(MOECON$`MO Adj Close`)# Does not appear to be normally distributed.  
hist(MOECON$`MO % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(MOECON$`MO % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the 
# data is distributed normally.

pairs(MOECON)# Relationships: Somewhat positive relationship between MO and S&P
# 500. Apparent positive relationship between MO and GDP. Negative relationships
# between MO and unemployment (including NROU short term and long term). 

FECON <- cbind(TotalData$`F Adj Close`, TotalData$`F % Change`,
               TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
               TotalData[,21:26])
colnames(FECON) <- c('F Adj Close', 'F % Change', 'S&P 500 Adj Close',
                     'S&P 500 % Change','CPI', 'Interest Rate','Unemployment Rate', 
                     'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(FECON)# Used to check the structure of the data after reorganization.

hist(FECON$`F Adj Close`)# Does not appear to be normally distributed.  
hist(FECON$`F % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(FECON$`F % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the 
# data is distributed normally.

pairs(FECON)# Relationships: Initial positive relationship between F and S&P
# 500, but seems to turns more negative. Relationship between F and other economic
# health variables is less apparent.

GVAECON <- cbind(TotalData$`GVA Adj Close`, TotalData$`GVA % Change`,
                 TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                 TotalData[,21:26])
colnames(GVAECON) <- c('GVA Adj Close', 'GVA % Change', 'S&P 500 Adj Close',
                       'S&P 500 % Change','CPI', 'Interest Rate', 'Unemployment Rate',
                       'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(GVAECON)# Used to check the structure of the data after reorganization.

hist(GVAECON$`GVA Adj Close`)# Does not appear to be normally distributed.  
hist(GVAECON$`GVA % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(GVAECON$`GVA % Change`)
# Returns p-value more than 5% so fail to reject the null hypothesis that the 
# data is distributed normally.

pairs(GVAECON)# Relationships: Somewhat positive relationship between GVA and S&P
# 500. Somewhat positive relationship between GVA and GDP. Somewhat negative relationships
# between GVA and unemployment (including NROU short term and long term).

JPMECON <- cbind(TotalData$`JPM Adj Close`, TotalData$`JPM % Change`,
                 TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                 TotalData[,21:26])
colnames(JPMECON) <- c('JPM Adj Close', 'JPM % Change', 'S&P 500 Adj Close', 
                       'S&P 500 % Change','CPI', 'Interest Rate', 'Unemployment Rate',
                       'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(JPMECON)# Used to check the structure of the data after reorganization.

hist(JPMECON$`JPM Adj Close`)# Does not appear to be normally distributed.  
hist(JPMECON$`JPM % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(JPMECON$`JPM % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the 
# data is distributed normally.

pairs(JPMECON)# Relationships: Apparent positive relationship between JPM and S&P
# 500. Apparent positive relationship between JPM and GDP. Negative relationships
# between JPM and unemployment (including NROU short term and long term).

WMTECON <- cbind(TotalData$`WMT Adj Close`, TotalData$`WMT % Change`,
                 TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                 TotalData[,21:26])
colnames(WMTECON) <- c('WMT Adj Close', 'WMT % Change', 'S&P 500 Adj Close',
                       'S&P 500 % Change','CPI', 'Interest Rate', 'Unemployment Rate', 
                       'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(WMTECON)# Used to check the structure of the data after reorganization.

hist(WMTECON$`WMT Adj Close`)# Does not appear to be normally distributed.  
hist(WMTECON$`WMT % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(WMTECON$`WMT % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the 
# data is distributed normally.

pairs(WMTECON)# Relationships: Apparent positive relationship between WMT and S&P
# 500. Apparent positive relationship between WMT and GDP. Negative relationships
# between WMT and unemployment (including NROU short term and long term).

MSFTECON <- cbind(TotalData$`MSFT Adj Close`, TotalData$`MSFT % Change`,
                  TotalData$`S&P 500 Adj Close`, TotalData$`S&P 500 % Change`,
                  TotalData[,21:26])
colnames(MSFTECON) <- c('MSFT Adj Close', 'MSFT % Change', 'S&P 500 Adj Close', 
                        'S&P 500 % Change','CPI', 'Interest Rate', 'Unemployment Rate', 
                        'Short Term NROU','Long Term NROU', 'GDP (billions)')
head(MSFTECON)# Used to check the structure of the data after reorganization.

hist(MSFTECON$`MSFT Adj Close`)# Does not appear to be normally distributed.  
hist(MSFTECON$`MSFT % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(MSFTECON$`MSFT % Change`)
# Returns p-value more than 5% so fail to reject the null hypothesis that the 
# data is distributed normally.

pairs(MSFTECON)# Relationships: Apparent positive relationship between MSFT and S&P
# 500. Apparent positive relationship between MSFT and GDP. Negative relationships
# between MSFT and unemployment (including NROU short term and long term).

# I do not follow the same process for the S&P 500 data because you can already
# see the relationship between the S&P 500 data and the economic health data in
# all of the above pair functions.
# Relationships for S&P 500 adjusted close prices with economic health data:
# Negative relationship between S&P and unemployment statistics and positive
# relationship between S&P 500 and GDP
# More generally, the stock prices seem to have positive relationships with GDP
# and negative relationships with unemployment statistics, which theoretically
# makes sense.

# Distribution of the S&P 500:
hist(TotalData$`S&P 500 Adj Close`)# Does not appear to be normally distributed.  
hist(TotalData$`S&P 500 % Change`)# Potentially normally distributed. 

# Conducts a hypothesis test for normality.
jarque.bera.test(TotalData$`S&P 500 % Change`)
# Returns p-value less than 5% so reject the null hypothesis that the 
# data is distributed normally.

# Compute the covariance to see a picture of the pairs plots above in numerical 
# values. Difficult to understand the strength of the relationships noted above,
# but this process adds to the visualization of the data.
cov(AAECON)
cov(AAPLECON)
cov(MOECON)
cov(FECON)
cov(GVAECON)
cov(JPMECON)
cov(WMTECON)
cov(MSFTECON)
# Once again, I do not follow the same process for the S&P 500 data because you 
# can already see the relationship between the S&P 500 data and the economic 
# health data in all of the above cov functions.  

# Will use a correlation matrix to look further into the relationships observed
# above.
cor(AAECON)# Alcoa's relationships with all of the variables are relatively weak. 
# Most significant relationship is the moderate negative relationship with 
# unemployment rate. Moderate to strong positive relationship between Alcoa percent
# change and S&P 500 percent change. 
cor(AAPLECON)# Strong positive relationship between Apple and S&P 500 and GDP.
# Moderate negative relationship with interest rates. Interesting: strong 
# negative relationships with NROU, but weak with unemployment rate. Moderate 
# positive relationship between Apple percent change and S&P 500 percent change. 
cor(MOECON)# Strong positive relationship between Altria and S&P 500 and GDP.
# Moderate negative relationship with interest rates. Interesting: strong 
# negative relationships with NROU, but weak with unemployment rate. Moderate 
# positive relationship between Altria percent change and S&P 500 percent change. 
cor(FECON)# Moderate positive relationship between Ford and S&P 500 and weak with 
# GDP. Weak negative relationships with NROU unemployment rate. Weak positive
# relationship between Ford percent change and S&P 500 percent change.
cor(GVAECON)# Strong positive relationship between Granite Construction and S&P 
# 500 and GDP. Moderate negative relationship with interest rates. Interesting: 
# strong negative relationships with NROU, but weak with unemployment rate.
# Weak positive relationship between Granite Construction percent change and S&P 
# 500 percent change.
cor(JPMECON) # Very strong positive relationship between JP Morgan and S&P 500 
# Less strong, but still strong positive relationship with GDP.
# Moderate negative relationship with interest rates. Interesting: strong 
# negative relationships with NROU, but weak with unemployment rate.
# Moderate positive relationship between JP Morgan percent change and S&P 500 
# percent change.
cor(WMTECON)# Strong positive relationship between Walmart and S&P 500 and GDP.
# Moderate negative relationship with interest rates. Interesting: strong 
# negative relationships with NROU, but almost nonexistent with unemployment rate.
# Moderate positive relationship between Walmart percent change and S&P 500 
# percent change.
cor(MSFTECON)# Strong positive relationship between Microsoft and S&P 500 and GDP.
# Moderate negative relationship with interest rates. Interesting: strong 
# negative relationships with NROU, but weak with unemployment rate.
# Strong positive relationship between Microsoft percent change and S&P 500 
# percent change.

# In each of the cor() functions, the relationships of the S&P 500 and the 
# economic health data is compared as well: Moderate negative relationship with
# interest rates, weak negative with unemployment rate, but strong negative 
# with NROU, and then strong positive relationship with GDP. 

plot(TotalData$`AA Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`AA Adj Close`~TotalData$UNRATE)
plot(TotalData$`AA Adj Close`~TotalData$GDP)
# Plots of the interesting relationships between Alcoa's stock price and key
# economic health indicators. Can see in the plots the weak to nonexistent 
# relationships that exist, except for a moderate negative relationship with
# unemployment. Makes sense because Alcoa is a primary sector company. 

plot(TotalData$`MO Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`MO Adj Close`~TotalData$GDP)
# In both plots can witness stronger negative relationships between Altria and
# S&P 500 and Altria and GDP, but this relationship starts to wane. Could be 
# related to the increasing awareness of the health effects of smoking (Altria
# is a tobacco company).

plot(TotalData$`F Adj Close`~TotalData$UNRATE)
plot(TotalData$`F Adj Close`~TotalData$GDP)
# In both plots can see the relatively week relationships between Ford and
# unemployment rate and Ford and GDP. As a manufacturing company, interesting
# to see the limited effect economic health indicators have on stock price.

# The larger service (JP Morgan and Walmart) and technology (Apple and
# Microsoft) companies have strong relationships with GDP and S&P 500.
plot(TotalData$`AAPL Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`MSFT Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`JPM Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`WMT Adj Close`~TotalData$`S&P 500 Adj Close`)
plot(TotalData$`AAPL Adj Close`~TotalData$GDP)
plot(TotalData$`MSFT Adj Close`~TotalData$GDP)
plot(TotalData$`JPM Adj Close`~TotalData$GDP)
plot(TotalData$`WMT Adj Close`~TotalData$GDP)
# Interesting when compared with relationships between primary and secondary
# sector companies with the backdrop of the United State's increasingly service
# oriented economy.

###############################
######Data Visualizations######
###############################

library(ggplot2) # Load the ggplot library into R. 

# Primary Sector Companies:
# Visuals for Alcoa:
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `AA Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `AA % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")

# Visuals for Altria:
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `MO Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `MO Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `MO % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `MO Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `MO Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")

# Secondary Sector Companies:
# Visuals for Ford:
ggplot(TotalData, aes(x = `GDP (billions)`, y = `F Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `F % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `F Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")

# Visuals for Granite Construction:
ggplot(TotalData, aes(x = `Interest Rate`, y = `GVA Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `GVA Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `GVA % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `GVA Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `GVA Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")

# Tertiary Sector Companies:
# Visuals for Walmart: 
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `WMT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `WMT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `WMT % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `WMT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `WMT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `WMT % Change`)) +
  geom_point() + 
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `WMT Adj Close`)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)

# Visuals for JP Morgan
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `JPM Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `JPM Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `JPM % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `JPM Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `JPM Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `JPM % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `JPM Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)

# Quaternary Sector Companies: 
# Visuals for Apple:
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `AAPL Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `AAPL Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `AAPL % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `AAPL Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `AAPL Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `AAPL % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `AAPL Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)

# Visuals for Microsoft:
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `MSFT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `GDP (billions)`, y = `MSFT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `MSFT % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Unemployment Rate`, y = `MSFT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `Long Term NROU`, y = `MSFT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm")
ggplot(TotalData, aes(x = `S&P 500 % Change`, y = `MSFT % Change`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)
ggplot(TotalData, aes(x = `S&P 500 Adj Close`, y = `MSFT Adj Close`)) + 
  geom_point() +
  geom_smooth(method = "rlm") +
  facet_wrap(~Quarter)

# After running the cor() function and doing preliminary plots, it can be seen
# through the ggplot2 visuals that there are groups of outliers, especially in
# stocks such as Apple and Microsoft. These outliers can best be seen when
# the unemployment rate is graphed with the adjusted close prices of the stocks.
# The graphs seem to display a negative relationship; however, there is a group
# of outlying data where the price of the stock is very low, but the unemployment
# rate is also low. This outlying data is most likely the reason for weak
# relationships between the stocks adjusted close price and the unemployment rate.
# I believe there is a connection between this outlying data and the fact that 
# Apple and Microsoft have participated in a multitude of stock splits, 
# artificially making the price of the stock when it first became public 
# extremely low. 