# The start() and end() functions return the time index of the first and last observations, respectively. The time() function calculates a vector of time indices, with one element for each time index on which the series was observed.
# 
# The deltat() function returns the fixed time interval between observations and the frequency() function returns the number of observations per unit time. Finally, the cycle() function returns the position in the cycle of each observation.


# Plot AirPassengers

plot(AirPassengers)

# View the start and end dates of AirPassengers

start(AirPassengers)
end(AirPassengers)


# Use time(), deltat(), frequency(), and cycle() with AirPassengers 

time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)



#################################################


# Use print() and plot() to view data_vector
print(data_vector)
plot(data_vector)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <- ts(data_vector,start=2004,frequency=4)

# Use print() and plot() to view time_series

print(time_series)
plot(time_series)



################ PLotting multiple time series 

# Check whether eu_stocks is a ts object
is.ts(eu_stocks)

# View the start, end, and frequency of eu_stocks

start(eu_stocks)
end(eu_stocks)
frequency(eu_stocks)


# Generate a simple plot of eu_stocks

plot(eu_stocks)

# Use ts.plot with eu_stocks
ts.plot(eu_stocks, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(eu_stocks), lty = 1, col = 1:4, bty = "n")


############# Transformations

# The logarithmic function log() is a data transformation that can be applied to positively valued time series data. It slightly shrinks observations that are greater than one towards zero, while greatly shrinking very large observations. This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time.


# Log rapid_growth
linear_growth <- log(rapid_growth)

# Plot linear_growth using ts.plot()

ts.plot(linear_growth)


##############

#Removing trends in level by differencing
#The first difference transformation of a time series z[t] consists of the differences (changes) between successive observations over time, that is z[t]???z[t???1].
#Differencing a time series can remove a time trend. The function diff() will calculate the first difference or change series. A difference series lets you examine the increments or changes in a given time series. It always has one fewer observations than the original series.


# Generate the first difference of z
dz <- diff(z,1)

# Plot dz

ts.plot(dz)

# View the length of z and dz, respectively

length(z)
length(dz)


############### Seasonal Differences

# For time series exhibiting seasonal trends, seasonal differencing can be applied to remove these periodic patterns. For example, monthly data may exhibit a strong twelve month pattern. In such situations, changes in behavior from year to year may be of more interest than changes from month to month, which may largely follow the overall seasonal pattern.
# The function diff(..., lag = s) will calculate the lag s difference or length s seasonal change series. For monthly or quarterly data, an appropriate value of s would be 12 or 4, 

# Generate a diff of x with lag = 4. Save this to dx
dx <- diff(x,lag=4)

# Plot dx

ts.plot(dx)

# View the length of x and dx, respectively 

length(x)
length(dx)


############# simulating white noise

# Simulate the white noise model
# The white noise (WN) model is a basic time series model. It is also a basis for the more elaborate models we will consider. We will focus on the simplest form of WN, independent and identically distributed data.
# 
# The arima.sim() function can be used to simulate data from a variety of time series models. ARIMA is an abbreviation for the autoregressive integrated moving average class of models we will consider throughout this course.


# Simulate a WN model with list(order = c(0, 0, 0))
white_noise <- arima.sim(model = list(order=c(0,0,0)), n = 100)

# Plot your white_noise data

ts.plot(white_noise)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order=c(0,0,0)), n = 100, mean = 100, sd = 10)



# Plot your white_noise_2 data

ts.plot(white_noise_2)



#################### Fit Arima to simulated data


# For a given time series y we can fit the white noise (WN) model using the arima(..., order = c(0, 0, 0)) function. Recall that the WN model is an ARIMA(0,0,0) model. Applying the arima() function returns information or output about the estimated model. For the WN model this includes the estimated mean, labeled intercept, and the estimated variance, labeled sigma^2


# Fit the WN model to y using the arima command

arima(y,order=c(0,0,0))

# Calculate the sample mean and sample variance of y

mean(y)
var(y)


################## Simulate Random Walk

# Simulate the random walk model
# The random walk (RW) model is also a basic time series model. It is the cumulative sum (or integration) of a mean zero white noise (WN) series, such that the first difference series of a RW is a WN series. Note for reference that the RW model is an ARIMA(0, 1, 0) model, in which the middle entry of 1 indicates that the model's order of integration is 1.

# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order=c(0,1,0)), n = 100)

# Plot random_walk
ts.plot(random_walk)

# Calculate the first difference series
random_walk_diff <- diff(random_walk)

# Plot random_walk_diff

ts.plot(random_walk_diff)  


##############Simulate the random walk model with a drift

# A random walk (RW) need not wander about zero, it can have an upward or downward trajectory, i.e., a drift or time trend. This is done by including an intercept in the RW model, which corresponds to the slope of the RW time trend.
# 
# For an alternative formulation, you can take the cumulative sum of a constant mean white noise (WN) series, such that the mean corresponds to the slope of the RW time trend.

# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order=c(0,1,0)), n = 100, mean = 1)

# Plot rw_drift
ts.plot(rw_drift)

# Calculate the first difference series
rw_drift_diff <- diff(rw_drift,1) 

# Plot rw_drift_diff

ts.plot(rw_drift_diff)

####################################### Estimating RW using ARIMA
#NOTE : First difference of RW with drift = 2 is a white noise process with mean 2

# Estimate the random walk model
# For a given time series y we can fit the random walk model with a drift by first differencing the data, then fitting the white noise (WN) model to the differenced data using the arima() command with the order = c(0, 0, 0)) argument.
# 
# The arima() command displays information or output about the fitted model. Under the Coefficients: heading is the estimated drift variable, named the intercept. Its approximate standard error (or s.e.) is provided directly below it. The variance of the WN part of the model is also estimated under the label sigma^2.

# Difference your random_walk data
rw_diff <- diff(random_walk,1)

# Plot rw_diff

ts.plot(rw_diff)

# Now fit the WN model to the differenced data
model_wn <- arima(rw_diff,order=c(0,0,0))

# Store the value of the estimated time trend (intercept)
int_wn <- model_wn$coef

# Plot the original random_walk data

ts.plot(random_walk)

# Use abline(0, ...) to add time trend to the figure

abline(0,int_wn)

########################### Autocorrelation function (ACF)

# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)

# Estimating the autocorrelation function (ACF) at many lags allows us to assess how a time series x relates to its past. The numeric estimates are important for detailed calculations, but it is also useful to visualize the ACF as a function of the lag.
# 
# In fact, the acf() command produces a figure by default. It also makes a default choice for lag.max, the maximum number of lags to be displayed.

acf(x)

######################################


