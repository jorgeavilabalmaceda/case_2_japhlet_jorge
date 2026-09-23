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
var13 <- VAR(var_data, p = 13, type = "const")
var4  <- VAR(var_data, p = 4, type = "const")
var2  <- VAR(var_data, p = 2, type = "const")


#Plot residuals
plot(residuals(var13))
plot(residuals(var4))
plot(residuals(var2))


#Test for serial correlation
pt_var13 <- serial.test(var13, lags.pt = 16, type = "PT.asymptotic")
bg_var13 <- serial.test(var13, lags.bg = 4, type = "BG")

pt_var4 <- serial.test(var4, lags.pt = 16, type = "PT.asymptotic")
bg_var4 <- serial.test(var4, lags.bg = 4, type = "BG")

pt_var2 <- serial.test(var2, lags.pt = 16, type = "PT.asymptotic")
bg_var2 <- serial.test(var2, lags.bg = 4, type = "BG")

print(pt_var13)
print(bg_var13)
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
roots(var13)
roots(var4)
roots(var2)

plot(stability(var13))
plot(stability(var4))
plot(stability(var2))

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

#Results:
#All three models reject multivariate normality (p < 2.2e-16).
#This is mainly driven by significant skewness and kurtosis.


#Test for ARCH effects
arch.test(var13, lags.multi = 12)
arch.test(var4, lags.multi = 12)
arch.test(var2, lags.multi = 12)

#Results:
# All three models reject the null of no ARCH effects (p < 2.2e-16).

#Summary 
#None of the candidate VAR models passes all diagnostics. 
#However, all are stable, while VAR(13) has the weakest evidence of residual serial correlation. 
#The information criteria disagree on the appropriate lag length.


#CHECKING ACF for potential lags that were missed 
acf(residuals(var13)[, "IP"],lag.max = 50)
pacf(residuals(var13)[, "IP"],lag.max = 50)

acf(residuals(var13)[, "CPI"],lag.max = 50)
pacf(residuals(var13)[, "CPI"],lag.max = 50)

acf(residuals(var13)[, "FED"],lag.max = 50)
pacf(residuals(var13)[, "FED"],lag.max = 50)


#Johansen cointegration test on the level variables to check if a VECM model is more effective


data_levels <- data.frame(
  industrial_production,
  consumer_price_index,
  federal_funds_rate
)

jo_test <- ca.jo(
  data_levels,
  type = "trace",
  ecdet = "const",
  K = 12,
  spec = "transitory"
)

summary(jo_test)

#Results:
#r=0:  reject at 5%
#r<= 1: do not reject at 5%
#r<=2: do not reject at 5%
#
#Cointegration rank = 1.
# If all three level variables are I(1), this supports using a VECM.

#working with a subset of the data
# Restrict the sample to 1983–2019
data_1983_2019 <- subset(
  data,
  sasdate >= as.Date("1983-01-01") &
    sasdate <= as.Date("2020-01-01")
)

# Check the restricted sample
range(data_1983_2019$sasdate)
nrow(data_1983_2019)


# Variables for the restricted sample

industrial_production_1983_2019 <- data_1983_2019$INDPRO
consumer_price_index_1983_2019 <- data_1983_2019$CPIAUCSL
federal_funds_rate_1983_2019 <- data_1983_2019$FEDFUNDS


# Log transformations

log_industrial_production_1983_2019 <- log(industrial_production_1983_2019)
log_consumer_price_index_1983_2019 <- log(consumer_price_index_1983_2019)


# Unit Root Test - Industrial Production
# Set d=2

IPd2_1983_2019 <- boot_adf(
  diff(diff(log_industrial_production_1983_2019)),
  deterministics = "intercept"
)
IPd2_1983_2019
#p=0<0.05 we reject the null
# Set d=1 and test again

IPd1_1983_2019 <- boot_adf(
  diff(log_industrial_production_1983_2019),
  deterministics = "intercept"
)
IPd1_1983_2019
#p=0<0.04 we reject the null
# Set d=0 and test again

IPd0_1983_2019 <- boot_adf(
  log_industrial_production_1983_2019,
  deterministics = "trend"
)
IPd0_1983_2019
#p=0.78>0.05 we fail to reject the null logIP has a unit root at I(1)

# Unit Root Test - Consumer Price Index
# Set d=2

CPId2_1983_2019 <- boot_adf(
  diff(diff(log_consumer_price_index_1983_2019)),
  deterministics = "intercept"
)
CPId2_1983_2019
#p=0<0.05
# Set d=1 and test again

CPId1_1983_2019 <- boot_adf(
  diff(log_consumer_price_index_1983_2019),
  deterministics = "intercept"
)
CPId1_1983_2019
#p=0<0.05
# Set d=0 and test again

CPId0_1983_2019 <- boot_adf(
  log_consumer_price_index_1983_2019,
  deterministics = "trend"
)
CPId0_1983_2019
#p=0.94>0.05 we fail to reject the null. LogCPI

# Unit Root Test - Federal Funds Rate
# Set d=2

FEDd2_1983_2019 <- boot_adf(
  diff(diff(federal_funds_rate_1983_2019)),
  deterministics = "intercept"
)
FEDd2_1983_2019

# Set d=1 and test again

FEDd1_1983_2019 <- boot_adf(
  diff(federal_funds_rate_1983_2019),
  deterministics = "intercept"
)
FEDd1_1983_2019

# Set d=0 and test again

FEDd0_1983_2019 <- boot_adf(
  federal_funds_rate_1983_2019,
  deterministics = "trend"
)
FEDd0_1983_2019

