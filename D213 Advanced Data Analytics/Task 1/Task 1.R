# Load necessary libraries
library(tseries)
library(tidyverse)  # For base packages and charts
library(forecast)
library(lubridate)  # For date manipulation
library(xts)        # For time series objects with date index


# Load the dataset
df <- read.csv(
  "C:/Users/kiara/OneDrive/WGU MSDA/D213 Advanced Data Analytics/Task 1/teleco_time_series.csv"
)
View(df)


# C1. Line Graph Visualizing
tele_revenue <- df$Revenue
day <- df$Day
tele_trend <- lm(tele_revenue ~ day)
plot(
  tele_revenue,
  xlab = "Time in Days",
  ylab = "($Millions) Revenue",
  type = "l",
  col = "darkgreen",
  main = "Time Plot: Telecom Revenue Per Day"
)

# Add the trend line
abline(tele_trend, col = "red")

# Adjust margins
opar <- par(no.readonly = TRUE)
par(mar = c(6, 4.1, 4.1, 2.1))

# Add legend to the plot
legend(
  x = "bottom",
  # Place at the bottom
  legend = c("Revenue", "Trend"),
  # Labels
  lty = c(1, 1),
  # Line types
  col = c("darkgreen", "red"),
  # Line colors
  lwd = 2,
  # Line width
  cex = 1.5,
  # Legend size
  xpd = TRUE,
  # Allow plotting outside margins
  horiz = TRUE               # Horizontal layout
)

# Reset margins to default after plotting
par(opar)

#C2. Time Step Formatting
is.data.frame(df)
df_vector <- df[['Revenue']]
is.vector(df_vector)
glimpse(df)
any(is.na(df)) # Check for NA values in data set
is.null(df)  # Check for null values in data set
anyDuplicated(df$Day) # Check for duplicates in Day
time_series <- ts(df_vector, frequency = 731 / 24) # Convert to time series
is.ts(time_series) # Verify time series object
plot(
  time_series,
  main = "Time Series",
  col = "darkgreen",
  ylab =
    "Revenue in Millions",
  xlab = "Months"
) # Plot time series


# C3. Evaluate the stationarity of the time series
kpss_result <- kpss.test(time_series)
print(kpss_result)# Print the result
adf_test_result <- adf.test(time_series) #Double Checking
print(adf_test_result)# Print the result

# Apply first-order differencing to the time series
diff_time_series <- diff(time_series, differences = 1)
# Remove NA values introduced by differencing
diff_time_series <- na.omit(diff_time_series)
# Plot the differenced time series to visually check stationarity
plot(
  diff_time_series,
  main = "Differenced Time Series",
  ylab = "Value",
  xlab = "Time",
  col = "darkblue",
  type = "l"
)

# Run ADF test on the differenced series
adf_test_diff <- adf.test(diff_time_series)
print(adf_test_diff)
# Run KPSS test on the differenced series
kpss_test_diff <- kpss.test(diff_time_series)
print(kpss_test_diff)


# C4 Data Spliting
length(time_series)
train <- time_series[1:512]
test <- time_series[513:731]
#view dimensions of training and testing set
length(train)
length(test)

## write.csv( diff_time_series, "C:\\Users\\kiara\\OneDrive\\WGU MSDA\\D213 Advanced Data Analytics\\Clean Time Series.CSV")
## write.csv( train, "C:\\Users\\kiara\\OneDrive\\WGU MSDA\\D213 Advanced Data Analytics\\Clean Time Series training.CSV")
## write.csv(test,"C:\\Users\\kiara\\OneDrive\\WGU MSDA\\D213 Advanced Data Analytics\\Clean Time Series testing.CSV")


# D1a. Seasonality
tsdata <- ts(df$Revenue, frequency = 30)
dataDecomp <- decompose(tsdata)
plot(dataDecomp$seasonal,
     main = "Decompose Seasonal",
     col = "darkgreen")

# D1b. Trends
plot(
  dataDecomp$trend,
  main = "Decompose Trend",
  col = "darkred",
  lwd = 2
)

# D1c. Auto and Partial Correlation Function
par(mfrow = c(2, 1))
acf(
  diff_time_series,
  main = 'Autocorrelation by Lag',
  col = "darkgreen",
  lwd = 2
)
pacf(
  diff_time_series,
  main = "Partial Autocorrelation by Lag",
  col = "darkred",
  lwd = 2
)

# D1d. Spectral Density
library(astsa)
par(mfcol = c(2, 2))
mvspec(train,
       log = "yes",
       col = "darkred",
       lwd = 2)
text(0.3,
     2.0,
     "Bandwidth: 0.028",
     col = "darkblue",
     cex = 2)
text(0.3, 6.0, "DoF: 29.04", col = "darkblue", cex = 2)

mvspec(
  train,
  spans = 15,
  log = "no",
  col = "darkgreen",
  lwd = 2
)
text(0.3,
     30,
     "Bandwidth: 0.142",
     col = "darkblue",
     cex = 2)
text(0.3, 20, "DoF: 145.01", col = "darkblue", cex = 2)

mvspec(
  train,
  spans =  73,
  log = "no",
  col = "purple4",
  lwd = 2
)

spec.ar(train,
        log = "no",
        col = "salmon4",
        lwd = 2)
text(0.25,
     1500,
     "AR Spectrum",
     col = "darkblue",
     cex = 2)

spectrum(diff_time_series, col = "steelblue4", lwd = 2)

# D1e. Decomposing Time Series
stl <- stl(time_series, s.window = "period") #Perform seasonal decomposition on time series
plot(stl,
     main = "Revenue",
     col = "darkgreen",
     lwd = 2) # Visualize decomposition by observed, trend, seasonal, remainder

# D1f. Residuals
par(mfcol = c(2, 1))
plot(
  stl$time.series[, "remainder"],
  main = "Residuals of Decomposed Series",
  ylab = "Residuals",
  xlab = "Time",
  col = "darkblue",
  lwd = 1,
  type = "l"
)
abline(h = 0,
       col = "darkred",
       lty = 2)  # Add a horizontal line at zero for reference
acf(
  stl$time.series[, "remainder"],
  main = "ACF of Residuals",
  col = "darkgreen",
  lwd = 3
)

# D2 ARIMA Model
par(mfrow = c(3, 1), mar = c(4, 4, 4, 2))
# Time series plot of the differenced series
plot(
  diff_time_series,
  main = "Differenced Time Series",
  ylab = "Value",
  xlab = "Time",
  col = "darkblue",
  type = "l"
)
# Autocorrelation Function (ACF) plot
acf(
  diff_time_series,
  main = "ACF of Differenced Series",
  col = "darkgreen",
  lwd = 2
)
# Partial Autocorrelation Function (PACF) plot
pacf(
  diff_time_series,
  main = "PACF of Differenced Series",
  col = "darkred",
  lwd = 2
)
# Find the Best Model
auto.arima(train, seasonal = T)
fit <- Arima(train, order = c(1, 1, 0), include.drift = TRUE)
fit1 <- arima(train, order = c(1, 1, 1))
fit2 <- arima(train, order = c(1, 2, 0))
fit3 <- arima(train, order = c(0, 1, 0))
summary(fit)
AIC(fit, fit1, fit2, fit3)
checkresiduals(fit)

# D3 Forecasting using ARIMA Model
forecast_90 <- forecast(fit, h = 90)  # Forecast using model
plot(
  forecast_90,
  col = "darkblue",
  main = "90-Day Forecast",
  xlab = "Time",
  ylab = "Revenue"
)
lines(513:(512 + length(test)),
      test,
      col = "darkred",
      lwd = 2)  # Align with test data
legend(
  "topright",
  legend = c("Train", "Test", "Forecast", "95% Confidence Interval"),
  col = c("darkblue", "darkred", "blue", "lightblue"),
  lty = c(1, 1, 1, NA),
  # Line styles (solid for Train/Test/Forecast, none for shaded area)
  lwd = c(2, 2, 2, NA),
  # Line widths for Train/Test/Forecast, none for shaded area
  pch = c(NA, NA, NA, 15),
  # Box for shaded confidence interval
  pt.cex = c(1, 1, 1, 2),
  # Size for shaded confidence interval box
  cex = 0.8
)
# Ensure test set is 90 points long
test <- test[1:90]
# Evaluate model performance on the test set
accuracy_metrics <- accuracy(forecast_90$mean, test)
print(accuracy_metrics)

# Conduct Ljung-Box test on residuals of the fitted model
box_test <- Box.test(residuals(fit), type = "Ljung", lag = 20)
print(box_test)


# E2
# Plot the forecast and test data
plot(
  forecast_90,
  col = "darkblue",
  main = "90-Day Revenue Forecast vs. Actual Data",
  xlab = "Time (Days)",
  ylab = "Revenue ($ Millions)"
)
# Overlay test data for comparison
lines(513:(512 + length(test)), 
      test, 
      col = "darkred", 
      lwd = 2)

# Add a legend for clarity
legend(
  "topright",
  legend = c("Forecast", "Test Data", "95% Confidence Interval"),
  col = c("darkblue", "darkred", "lightblue"),
  lty = c(1, 1, NA),
  lwd = c(2, 2, NA),
  fill = c(NA, NA, "lightblue"),
  border = c(NA, NA, "lightblue"),
  pt.cex = 2,
  cex = 0.8
)


# F R Markdown
install.packages("rmarkdown")
install.packages("knitr")

