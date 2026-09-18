data <- read.csv("data/2020-02.csv")

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
#of which Model type could make the data stationary
#We can verify this using a unit root test

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

IPd2<-boot_adf(diff(log_industrial_production), deterministics = "intercept")
IPd2

#p-value is <0.05 so we reject the null

IPd1<-boot_adf(industrial_production, deterministics = "trend")
IPd1
#p-value is > 0.05 so we fail to reject the null. The series IP has a unit root is I(1)


#Unit Root Test - Consumer Price Index
CPId2<-boot_adf(diff(log_consumer_price_index), deterministics = "intercept")
CPId2

#p-value is >0.05 so we fail to reject the null. The series CPI has a unit root at I(2)

#Unit Root Test - Fed Funds Rate 
#test I(2)
FEDd2<-boot_adf(diff(federal_funds_rate), deterministics = "intercept")
FEDd2

#p-value < 0.05 to reject the null 

FEDd1<-boot_adf(federal_funds_rate, deterministics = "trend")
FEDd1

#p-value > 0.05 so we fail to reject the null. The series CPI has a unit root at I(1) 

#We see that the differences correspond with the log transformations suggested in FRED-MD. 
#We see that the series is indeed stationary after these series. 


#Constructing the VAR model 
#The new transformed variables 
IP_dlog <- diff(log_industrial_production)
CPI_d2log <-diff(diff(log_consumer_price_index))
FED_d <-diff(federal_funds_rate)


plot(tail(data$sasdate, length(IP_dlog)),IP_dlog,
     type = "l",
     main = "First Differenced Log of Industrial Production",
     xlab = "Date",
     ylab = "1st Diff Log Industrial Production")

plot(tail(data$sasdate, length(CPI_d2log)),CPI_d2log,
     type = "l",
     main = "Second differenced Log of CPI",
     xlab = "Date",
     ylab = "2nd Diff Log(CPI)")

plot(tail(data$sasdate, length(FED_d)), FED_d,
     type = "l",
     main = "First Differenced Federal Funds Rate",
     xlab = "Date",
     ylab = "1st Diff FEDFUNDS")
