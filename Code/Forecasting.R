library(readxl)
library(forecast)
library(tseries)
interest_data<-read_excel('C:/ACTL4001/actl4001/eco_data.xlsx') #Data name has been changed to Inflation, One_Yr_Spot_Rate and Ten_Yr_Spot_Rate
interest_data


# Assuming you have your interest rate data in a dataframe named 'interest_data'
# Assuming 'date' is the column containing the dates and 'rate' is the column containing interest rates

# Convert 'date' column to Date format
interest_data$Year <- as.Date(as.character(interest_data$Year), format = "%Y")

# Create a time series object
ts_data <- ts(interest_data$Inflation, frequency = 1)  # Assuming monthly data, change frequency accordingly

# Explore the data
plot(ts_data)

# Check for stationarity
adf.test(ts_data)  # Augmented Dickey-Fuller Test for stationarity

# If data is not stationary, take differences to achieve stationarity
diff_ts_data <- diff(ts_data) 

adf.test(ts_data)
adf.test(diff_ts_data)

par(mfrow=c(1,2))
acf(diff_ts_data, main="ACF")
pacf(diff_ts_data, main="PACF")

# Split data into training and testing sets
train_size <- floor(0.8 * length(diff_ts_data))
train_data <- diff_ts_data[1:train_size]
test_data <- diff_ts_data[(train_sisze + 1):length(diff_ts_data)]

# Automatically select ARIMA parameters using auto.arima
#arima_model <- auto.arima(train_data)

# Fit ARIMA model with selected parameters
arima_model <- arima(diff_ts_data, order=c(3, 1, 5))

# Forecast
forecast_values <- forecast(arima_model, h = 50)  # Forecasting next 50 time points

# Plot forecasts
plot(forecast_values)

# Evaluate the model
accuracy(forecast_values, test_data)

# Assuming forecast_values is the object containing differenced forecasts
last_observed_value <- tail(ts_data, 1)  # Get the last observed value of the original series
forecasted_mean <- forecast_values$mean  # Get the forecasted differences
lower_95 <- forecast_values$lower[, 2]  # Lower bound of 80% prediction interval
upper_95 <- forecast_values$upper[, 2]  # Upper bound of 80% prediction interval

# Create time series objects with the same time index
last_observed_series <- ts(rep(last_observed_value, length(forecasted_mean)+1), start = end(ts_data) + 1, frequency = frequency(ts_data))
forecasted_mean_series <- c(0, forecasted_mean)  # Include 0 for the first value in the cumulative sum
forecasted_lower_95_series <- c(0, lower_95)  # Include 0 for the first value in the cumulative sum
forecasted_upper_95_series <- c(0, upper_95)  # Include 0 for the first value in the cumulative sum
# Back-transform the differences
forecasted_mean <- last_observed_series + cumsum(forecasted_mean_series)
forecasted_lower_95 <- last_observed_series + cumsum(forecasted_lower_95_series)
forecasted_upper_95 <- last_observed_series + cumsum(forecasted_upper_95_series)

# Now 'forecasted_values' contains the forecasted values in the original scale

# Generate a sequence of years from 2023 to 51 years ahead
forecast_years <- seq(as.Date("2023-01-01"), by = "year", length.out = length(forecasted_values))

# Create a dataframe with the forecasted values and corresponding years
forecast_df <- data.frame(
  year = forecast_years,
  Inflation_mean = forecasted_mean,
  Inflation_lower_95 = forecasted_lower_95,
  Inflation_upper_95 = forecasted_upper_95
)
write.csv(forecast_df,file='C:\\ACTL4001\\actl4001\\inflation_forecast.csv')


#Forecast 1 year treasury 
# Create a time series object
ts_data <- ts(interest_data$One_Yr_Spot_Rate, frequency = 1)  # Assuming monthly data, change frequency accordingly

# Explore the data
plot(ts_data)

# Check for stationarity
adf.test(ts_data)  # Augmented Dickey-Fuller Test for stationarity

# If data is not stationary, take differences to achieve stationarity
diff_ts_data <- diff(ts_data) 

adf.test(ts_data)
adf.test(diff_ts_data)

par(mfrow=c(1,2))
acf(diff_ts_data, main="ACF")
pacf(diff_ts_data, main="PACF")

# Split data into training and testing sets
train_size <- floor(0.8 * length(diff_ts_data))
train_data <- diff_ts_data[1:train_size]
test_data <- diff_ts_data[(train_sisze + 1):length(diff_ts_data)]

# Automatically select ARIMA parameters using auto.arima
#arima_model <- auto.arima(train_data)

# Fit ARIMA model with selected parameters
arima_model <- arima(diff_ts_data, order=c(4, 1, 9))

# Forecast
forecast_values <- forecast(arima_model, h = 50)  # Forecasting next 50 time points

# Plot forecasts
plot(forecast_values)

# Evaluate the model
accuracy(forecast_values, test_data)

# Assuming forecast_values is the object containing differenced forecasts
last_observed_value <- tail(ts_data, 1)  # Get the last observed value of the original series
forecasted_mean <- forecast_values$mean  # Get the forecasted differences
lower_95 <- forecast_values$lower[, 2]  # Lower bound of 80% prediction interval
upper_95 <- forecast_values$upper[, 2]  # Upper bound of 80% prediction interval

# Create time series objects with the same time index
last_observed_series <- ts(rep(last_observed_value, length(forecasted_mean)+1), start = end(ts_data) + 1, frequency = frequency(ts_data))
forecasted_mean_series <- c(0, forecasted_mean)  # Include 0 for the first value in the cumulative sum
forecasted_lower_95_series <- c(0, lower_95)  # Include 0 for the first value in the cumulative sum
forecasted_upper_95_series <- c(0, upper_95)  # Include 0 for the first value in the cumulative sum
# Back-transform the differences
forecasted_mean <- last_observed_series + cumsum(forecasted_mean_series)
forecasted_lower_95 <- last_observed_series + cumsum(forecasted_lower_95_series)
forecasted_upper_95 <- last_observed_series + cumsum(forecasted_upper_95_series)

# Now 'forecasted_values' contains the forecasted values in the original scale

# Generate a sequence of years from 2023 to 51 years ahead
forecast_years <- seq(as.Date("2023-01-01"), by = "year", length.out = length(forecasted_values))

# Create a dataframe with the forecasted values and corresponding years
forecast_df <- data.frame(
  year = forecast_years,
  One_Yr_Spot_Rate_mean = forecasted_mean,
  One_Yr_Spot_Rate_lower_95 = forecasted_lower_95,
  One_Yr_Spot_Rate_upper_95 = forecasted_upper_95
)
write.csv(forecast_df,file='C:\\ACTL4001\\actl4001\\One_Spot_forecast.csv')


#Forecast 10 year treasury
# Create a time series object
ts_data <- ts(interest_data$Ten_Yr_Spot_Rate, frequency = 1)  # Assuming monthly data, change frequency accordingly

# Explore the data
plot(ts_data)

# Check for stationarity
adf.test(ts_data)  # Augmented Dickey-Fuller Test for stationarity

# If data is not stationary, take differences to achieve stationarity
diff_ts_data <- diff(ts_data) 

adf.test(ts_data)
adf.test(diff_ts_data)

par(mfrow=c(1,2))
acf(diff_ts_data, main="ACF")
pacf(diff_ts_data, main="PACF")

# Split data into training and testing sets
train_size <- floor(0.8 * length(diff_ts_data))
train_data <- diff_ts_data[1:train_size]
test_data <- diff_ts_data[(train_sisze + 1):length(diff_ts_data)]

# Automatically select ARIMA parameters using auto.arima
#arima_model <- auto.arima(train_data)

# Fit ARIMA model with selected parameters
arima_model <- arima(diff_ts_data, order=c(4, 1, 3))

# Forecast
forecast_values <- forecast(arima_model, h = 50)  # Forecasting next 50 time points

# Plot forecasts
plot(forecast_values)

# Evaluate the model
accuracy(forecast_values, test_data)

# Assuming forecast_values is the object containing differenced forecasts
last_observed_value <- tail(ts_data, 1)  # Get the last observed value of the original series
forecasted_mean <- forecast_values$mean  # Get the forecasted differences
lower_95 <- forecast_values$lower[, 2]  # Lower bound of 80% prediction interval
upper_95 <- forecast_values$upper[, 2]  # Upper bound of 80% prediction interval

# Create time series objects with the same time index
last_observed_series <- ts(rep(last_observed_value, length(forecasted_mean)+1), start = end(ts_data) + 1, frequency = frequency(ts_data))
forecasted_mean_series <- c(0, forecasted_mean)  # Include 0 for the first value in the cumulative sum
forecasted_lower_95_series <- c(0, lower_95)  # Include 0 for the first value in the cumulative sum
forecasted_upper_95_series <- c(0, upper_95)  # Include 0 for the first value in the cumulative sum
# Back-transform the differences
forecasted_mean <- last_observed_series + cumsum(forecasted_mean_series)
forecasted_lower_95 <- last_observed_series + cumsum(forecasted_lower_95_series)
forecasted_upper_95 <- last_observed_series + cumsum(forecasted_upper_95_series)

# Now 'forecasted_values' contains the forecasted values in the original scale

# Generate a sequence of years from 2023 to 51 years ahead
forecast_years <- seq(as.Date("2023-01-01"), by = "year", length.out = length(forecasted_values))

# Create a dataframe with the forecasted values and corresponding years
forecast_df <- data.frame(
  year = forecast_years,
  Ten_Yr_Spot_Rate_mean = forecasted_mean,
  Ten_Yr_Spot_Rate_lower_95 = forecasted_lower_95,
  Ten_Yr_Spot_Rate_upper_95 = forecasted_upper_95
)
write.csv(forecast_df,file='C:\\ACTL4001\\actl4001\\Ten_Spot_forecast.csv')

