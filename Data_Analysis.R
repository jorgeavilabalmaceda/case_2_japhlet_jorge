data <- read.csv("data/2019-12.csv")

print(head(data))

# Remove the metadata row that says "Transform:"
data <- data[-1, ]

# Convert date
data$sasdate <- as.Date(data$sasdate, format = "%m/%d/%Y")

print(head(data))


# Case 2 variables and transformation codes according to FRED-MD
#INDPRO    = Industrial Production
#transformation code = 5 (first difference of log)
#CPIAUCSL  = CPI: All Items
#transformation code = 6 (second difference of log)
#FEDFUNDS  = Effective Federal Funds Rate
#transformation code = 2 (first difference)

#I am not sure yet whether to transform the data
#according to the codes
#in the FRED-MD paper.

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