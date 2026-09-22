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
#p-value is 0.05753>0.05 so we fail to reject the null. -> Delta^2 log(CPI) series has a unit root at I(2)


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

#p-value 0.14> 0.05 so we fail to reject the null. The series CPI has a unit root at I(1) 



#Constructing the VAR model 
#The new transformed variables that result in stationary series 
IP_dlog <- diff(log_industrial_production)
CPI_dlog <-diff(diff(log_consumer_price_index))
FED_d <-diff(federal_funds_rate)


#We take data$sasdate[-1] in order to make the dates equal the series length. 
#By differencing by order 1 we have lost one observation. 
length(data$sasdate)
length(log_industrial_production)
length(IP_dlog)
length(CPI_dlog)
length(FED_d)


length(IP_dlog[-1])

#Plotting the series to see the transformation. 

plot(data$sasdate[-c(1,2)],IP_dlog[-1],
     type = "l",
     main = "First Differenced Log of Industrial Production",
     xlab = "Date",
     ylab = "1st Diff Log Industrial Production")

plot(data$sasdate[-c(1,2)],CPI_dlog,
     type = "l",
     main = "Second Differenced Log of CPI",
     xlab = "Date",
     ylab = "1st Diff Log(CPI)")

plot(data$sasdate[-c(1,2)], FED_d[-1],
     type = "l",
     main = "First Differenced Federal Funds Rate",
     xlab = "Date",
     ylab = "1st Diff FEDFUNDS")

#VAR model selection using criterion 
#Setup a dataframe run the VAR model select on 
var_data <- data.frame(
  IP = IP_dlog[-1],
  CPI = CPI_dlog,
  FED = FED_d[-1]
)

#type constant because the series are stationary 
VARselect(var_data,lag.max = 12, type="const")



