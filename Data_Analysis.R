data <- read.csv("data/2019-12.csv")

print(head(data))

# Remove the metadata row that says "Transform:"
data <- data[-1, ]

# Convert date
data$sasdate <- as.Date(data$sasdate, format = "%m/%d/%Y")

print(head(data))


#Case 2 variables
# INDPRO    = Industrial Production        tcode = 5
# CPIAUCSL  = CPI: All Items               tcode = 6
# FEDFUNDS  = Effective Federal Funds Rate tcode = 2

Industrial_Production <- data$INDPRO
Consumer_Price_Index <- data$CPIAUCSL
Federal_Funds_Rate <- data$FEDFUNDS

# Plot raw series

plot(data$sasdate, Industrial_Production,
     type = "l",
     main = "Industrial Production",
     xlab = "Date",
     ylab = "INDPRO")

plot(data$sasdate, Consumer_Price_Index,
     type = "l",
     main = "Consumer Price Index",
     xlab = "Date",
     ylab = "CPIAUCSL")

plot(data$sasdate, Federal_Funds_Rate,
     type = "l",
     main = "Federal Funds Rate",
     xlab = "Date",
     ylab = "FEDFUNDS")