install.packages("vars")
library(vars)
install.packages("bootUR")
library(bootUR)
install.packages("urca")
library(urca)
data <- read.csv("data/2020-02.csv")

set.seed(123)

print(head(data))

# Remove the metadata row that says "Transform:"
data <- data[-1, ]

# Convert date
data$sasdate <- as.Date(data$sasdate, format = "%m/%d/%Y")
range(data$sasdate)

# Check whether our three variables are numeric
str(data[, c("sasdate", "INDPRO", "CPIAUCSL", "FEDFUNDS")])

# Count missing observations in each variable
colSums(is.na(data[, c("INDPRO", "CPIAUCSL", "FEDFUNDS")]))

# Check for repeated dates
anyDuplicated(data$sasdate)
tail(data[, c("sasdate", "INDPRO", "CPIAUCSL", "FEDFUNDS")])

#We've verified that there are no missing observations, correct data types (all three economic variables are numeric) and that we have no duplicates.
#checking for missing months:

expected_dates <- seq.Date(
  from = min(data$sasdate),
  to = max(data$sasdate),
  by = "month"
)

setdiff(expected_dates, data$sasdate)
#numeric(0) -> no missing months (nice).

print(head(data))


# Case 2 variables and transformation codes according to FRED-MD
#INDPRO    = Industrial Production
#transformation code = 5 (first difference of log)
#CPIAUCSL  = CPI: All Items
#transformation code = 6 (second difference of log)
#FEDFUNDS  = Effective Federal Funds Rate
#transformation code = 2 (first difference)

#These transformation codes give us a hint
#of which transformation could make the data stationary
#We verify this using a unit root test. See unit root series. 

industrial_production <- data$INDPRO
consumer_price_index <- data$CPIAUCSL
federal_funds_rate <- data$FEDFUNDS

# Plot raw series

plot(data$sasdate, industrial_production,
     type = "l",
     main = "Industrial Production",
     xlab = "Date",
     ylab = "INDPRO")

plot(data$sasdate, consumer_price_index,
     type = "l",
     main = "Consumer Price Index",
     xlab = "Date",
     ylab = "CPIAUCSL")

plot(data$sasdate, federal_funds_rate,
     type = "l",
     main = "Federal Funds Rate",
     xlab = "Date",
     ylab = "FEDFUNDS")


#Proposed Data Transformation
#We take the log of industrial production and CPI so
#that we can evaluate the percentage change in IP and CPI.
#Taking the log changes the range of the graph
#Federal Funds Rate we do not transform
#since it is already in percentage form

log_industrial_production <- log(industrial_production)
log_consumer_price_index <- log(consumer_price_index)
length(log_consumer_price_index)
length(log_industrial_production)


plot(data$sasdate,log_industrial_production,
     type = "l",
     main = "log(Industrial Production)",
     xlab = "Date",
     ylab = "log(IP)")

plot(data$sasdate, log_consumer_price_index,
     type = "l",
     main = "log(Consumer Price Index)",
     xlab = "Date",
     ylab = "log(CPI)")

#Unit Root Test - Industrial Production
#Industrial Production. Set d=2

IPd2<-boot_adf(diff(diff(log_industrial_production)), deterministics = "intercept")
IPd2

#p-value is 0<0.05 so we reject the null
#Set d=1 and test again.
IPd1<-boot_adf(diff(log_industrial_production), deterministics = "intercept")
IPd1

#p-value is 0.001<0.05 so we reject the null
#Set d=0 and test again.

IPd0<-boot_adf(log_industrial_production, deterministics = "trend")
IPd0
#p-value is 0.69 > 0.05 so we fail to reject the null. The log(IP) series has a unit root at I(1)


#Unit Root Test - Consumer Price Index
#Set d=2
CPId2 <-boot_adf(diff(diff(log_consumer_price_index)), deterministics = "intercept")
CPId2

#p-value is 0<0.05 so we reject the null.
#set d=1 and test again

CPId1 <-boot_adf(diff(log_consumer_price_index), deterministics = "intercept")
CPId1
#p-value is 0.04<0.05 so reject the null. 
#set d=0 and test again

CPId0 <-boot_adf(log_consumer_price_index,deterministics = "trend")
CPId0
#p-value is 0.860 >0.05 so we fail to reject the null -> series has a unit root at I(1)

#Unit Root Test - Fed Funds Rate 
#Set d=2

FEDd2<-boot_adf(diff(diff(federal_funds_rate)), deterministics = "intercept")
FEDd2
#p-value 0< 0.05 to reject the null 
#Set d=1 and test again.

FEDd1<-boot_adf(diff(federal_funds_rate), deterministics = "intercept")
FEDd1

#p-value 0 < 0.05 to reject the null 
#Set d=0 and test again.
FEDd0 <-boot_adf(federal_funds_rate, deterministics = "trend")
FEDd0

#p-value 0.12> 0.05 so we fail to reject the null. The series CPI has a unit root at I(1) 



#Constructing the VAR model 
#The new transformed variables that result in stationary series 
IP_dlog <- diff(log_industrial_production)
CPI_dlog <-diff(log_consumer_price_index)
FED_d <-diff(federal_funds_rate)


#We take data$sasdate[-1] in order to make the dates equal the series length. 
#By differencing by order 1 we have lost one observation. 
length(data$sasdate)
length(log_industrial_production)
length(IP_dlog)
length(CPI_dlog)
length(FED_d)



#Plotting the series to see the transformation. 

plot(data$sasdate[-1],IP_dlog,
     type = "l",
     main = "First Differenced Log of Industrial Production",
     xlab = "Date",
     ylab = "1st Diff Log Industrial Production")

plot(data$sasdate[-1],CPI_dlog,
     type = "l",
     main = "First Differenced Log of CPI",
     xlab = "Date",
     ylab = "1st Diff Log(CPI)")

plot(data$sasdate[-1], FED_d,
     type = "l",
     main = "First Differenced Federal Funds Rate",
     xlab = "Date",
     ylab = "1st Diff FEDFUNDS")

#VAR model selection using criterion 
#Setup a dataframe run the VAR model select on 
var_data <- data.frame(
  IP = IP_dlog,
  CPI = CPI_dlog,
  FED = FED_d
)



#Lag selection
VARselect(var_data, lag.max = 36, type = "const")

#Results:
#AIC = 13 lags
#HQ  = 4 lags
#SC  = 2 lags
#FPE = 13 lags


#Estimate candidate VAR models
var18<-VAR(var_data,p=18,type="const")
var17<-VAR(var_data,p=17,type="const")
var16<-VAR(var_data,p=16,type="const")
var15<-VAR(var_data,p=15,type="const")
var14<-VAR(var_data,p=14,type="const")

var24<-VAR(var_data,p=24,type="const")

var13 <- VAR(var_data, p = 13, type = "const")
var7<-VAR(var_data,p=7,type="const")
var6<-VAR(var_data,p=6,type="const")
var4  <- VAR(var_data, p = 4, type = "const")
var2  <- VAR(var_data, p = 2, type = "const")

#summary(var13)
#summary(var4)
summary(var2)

#Plot residuals
#plot(residuals(var13))
#plot(residuals(var4))
plot(residuals(var2))


#Test for serial correlation
pt_var24 <-serial.test(var24,lags.pt=36,type="PT.adjusted")
bg_var24 <- serial.test(var24, lags.bg = 12, type = "BG")


pt_var18 <- serial.test(var18, lags.pt = 24, type = "PT.adjusted")
bg_var18 <- serial.test(var18, lags.bg = 12, type = "BG")

pt_var17 <- serial.test(var17, lags.pt = 24, type = "PT.adjusted")
bg_var17 <- serial.test(var17, lags.bg = 12, type = "BG")

pt_var16 <- serial.test(var16, lags.pt = 24, type = "PT.adjusted")
bg_var16 <- serial.test(var16, lags.bg = 12, type = "BG")

pt_var15 <- serial.test(var15, lags.pt = 24, type = "PT.adjusted")
bg_var15 <- serial.test(var15, lags.bg = 12, type = "BG")

pt_var14 <- serial.test(var14, lags.pt = 24, type = "PT.adjusted")
bg_var14 <- serial.test(var14, lags.bg = 12, type = "BG")

pt_var7 <- serial.test(var7, lags.pt = 24, type = "PT.adjusted")
bg_var7 <- serial.test(var7, lags.bg = 12, type = "BG")

pt_var6 <- serial.test(var6, lags.pt = 24, type = "PT.adjusted")
bg_var6 <- serial.test(var6, lags.bg = 12, type = "BG")

pt_var13 <- serial.test(var13, lags.pt = 16, type = "PT.adjusted")
bg_var13 <- serial.test(var13, lags.bg = 12, type = "BG")

pt_var4 <- serial.test(var4, lags.pt = 16, type = "PT.adjusted")
bg_var4 <- serial.test(var4, lags.bg = 12, type = "BG")

pt_var2 <- serial.test(var2, lags.pt = 16, type = "PT.adjusted")
bg_var2 <- serial.test(var2, lags.bg = 12, type = "BG")

print(pt_var24)
print(bg_var24)

print(pt_var18)
print(bg_var18)

print(pt_var17)
print(bg_var17)

print(pt_var16)
print(bg_var16)
#
print(pt_var15) 
print(bg_var15)

print(pt_var14)
print(bg_var14)

print(pt_var13)
print(bg_var13)

print(pt_var7)
print(bg_var7)

print(pt_var4)
print(bg_var4)

print(pt_var2)
print(bg_var2)



#Results:
#VAR(13):Portmanteau p= 0.000574, BG p= 0.00163
#VAR(4):Portmanteau p< 0.001, BG p< 0.001
#VAR(2):Portmanteau p< 0.001, BG p< 0.001
#
#All three models reject the null of no serial correlation.
#VAR(13) has the weakest evidence of serial correlation,
#but the residuals are still significantly autocorrelated.


#Check stability
roots(var16)
roots(var15)
roots(var13)
roots(var4)
roots(var2)
roots(var7)

plot(stability(var13))
plot(stability(var4))
plot(stability(var2))
plot(stability(var7))

#Results:
#VAR(13): largest inverse root = 0.960
#VAR(4):  largest inverse root = 0.863
#VAR(2):  largest inverse root = 0.684
#
#All inverse roots are below 1, so all three models are stable.


#Test for normality
normality.test(var13)
normality.test(var4)
normality.test(var2)
normality.test(var7)

#Results:
#All three models reject multivariate normality (p < 2.2e-16).
#This is mainly driven by significant skewness and kurtosis.


#Test for ARCH effects
arch.test(var13, lags.multi = 12)
arch.test(var4, lags.multi = 12)
arch.test(var2, lags.multi = 12)
arch.test(var7, lags.multi = 12)
arch.test(var15, lags.multi = 12)
arch.test(var16, lags.multi = 12)

#Results:
# All models tested reject the null of no ARCH effects (p < 2.2e-16).

#Summary 
#None of the candidate VAR models passes all diagnostics. 
#However, all are stable, while VAR(13) has the weakest evidence of residual serial correlation. 
#The information criteria disagree on the appropriate lag length.


#CHECKING ACF for potential lags that were missed

acf(residuals(var15)[, "IP"],lag.max = 50)
pacf(residuals(var15)[, "IP"],lag.max = 50)

acf(residuals(var15)[, "CPI"],lag.max = 50)
pacf(residuals(var15)[, "CPI"],lag.max = 50)

acf(residuals(var15)[, "FED"],lag.max = 50)
pacf(residuals(var15)[, "FED"],lag.max = 50)




acf(residuals(var13)[, "IP"],lag.max = 50)
pacf(residuals(var13)[, "IP"],lag.max = 50)

acf(residuals(var13)[, "CPI"],lag.max = 50)
pacf(residuals(var13)[, "CPI"],lag.max = 50)

acf(residuals(var13)[, "FED"],lag.max = 50)
pacf(residuals(var13)[, "FED"],lag.max = 50)

acf(residuals(var7)[, "FED"],lag.max = 50)
pacf(residuals(var7)[, "FED"],lag.max = 50)

acf(residuals(var7)[, "CPI"],lag.max = 50)
pacf(residuals(var7)[, "CPI"],lag.max = 50)

acf(residuals(var7)[, "IP"],lag.max = 50)
pacf(residuals(var7)[, "IP"],lag.max = 50)



#Granger Tests
# Does FED cause IP?
grangertest(IP ~ FED, order = 15, data = var_data)

#no FED does not granger cause IP 
# Does IP cause FED?
grangertest(FED ~ IP, order = 15, data = var_data)
# IP does granger cause FED

# Does IP cause CPI?
grangertest(CPI ~ IP, order = 15, data = var_data)
#yes, IP does granger cause CPI
# Does CPI cause FED?
grangertest(FED ~ CPI, order = 15, data = var_data)
#No, CPI does not granger cause FED
# Does CPI cause IP?
grangertest(IP ~ CPI, order = 15, data = var_data)
#Yes CPI granger causes IP
