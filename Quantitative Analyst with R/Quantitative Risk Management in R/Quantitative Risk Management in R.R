## Chapter 1
# Load DJ index
data("DJ")

# Show head() and tail() of DJ index
head(DJ)
tail(DJ)

# Plot DJ index
plot(DJ)

# Extract 2008-2009 and assign to dj0809
dj0809 <- DJ["2008/2009"]

# Plot dj0809
plot(dj0809)

# Load DJ constituents data
data("DJ_const")

# Apply names() and head() to DJ_const
names(DJ_const)
head(DJ_const)

# Extract AAPL and GS in 2008-09 and assign to stocks
stocks <- DJ_const["2008/2009", c("AAPL", "GS")]

# Plot stocks with plot.zoo()
plot.zoo(stocks)

# Load exchange rate data
data("GBP_USD")
data("EUR_USD")

# Plot the two exchange rates
plot(GBP_USD)
plot(EUR_USD)

# Plot a USD_GBP exchange rate
plot(1/GBP_USD)

# Merge the two exchange rates GBP_USD and EUR_USD
fx <- merge(GBP_USD, EUR_USD, all = TRUE)

# Extract 2010-15 data from fx and assign to fx0015
fx0015 <- fx["2010/2015"]

# Plot the exchange rates in fx0015
plot.zoo(fx0015)

# Compute the log-returns of dj0809 and assign to dj0809_x
dj0809_x <- diff(log(dj0809))

# Plot the log-returns
plot(dj0809_x)

# Compute the log-returns of djstocks and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot the two share returns
plot.zoo(djstocks_x)

# Compute the log-returns of GBP_USD and assign to erate_x
erate_x <- diff(log(GBP_USD))

# Plot the log-returns
plot(erate_x)

# Plot djstocks in four separate plots
plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks, plot.type = "single", col = 1:4)
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x, type = "h")

# Plot djx
plot(djx)

# Plot weekly log-returns of djx
plot(apply.weekly(djx, sum), type = "h")

# Plot monthly log-returns of djx
plot(apply.monthly(djx, sum), type = "h")

# Plot djreturns
plot.zoo(djreturns)

# Plot monthly log-returns of djreturns
plot.zoo(apply.monthly(djreturns, colSums), type = "h")

# Plot gold and oil prices
plot(gold)
plot(oil)

# Calculate daily log-returns
goldx <- diff(log(gold))
oilx <- diff(log(oil))

# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx, sum)
oilx_m <- apply.monthly(oilx, sum)

# Merge goldx_m and oilx_m into coms
coms <- merge(goldx_m, oilx_m)

# Plot coms with vertical bars
plot.zoo(coms, type = "h")

# Make a pairwise scatterplot of coms
pairs(as.zoo(coms))

# Compute log-returns as zcb_x and simple returns as zcb_x2
zcb_x <- diff(log(zcb))
zcb_x2 <- diff(zcb)

# Plot zcb_x for 1, 5 and 10-year maturities
plot.zoo(zcb_x[, yield_cols])

# Plot zcb_x2 for 1, 5 and 10-year maturities
plot.zoo(zcb_x2[, yield_cols])

# Plot the yield curve for the first day of zcb
plot(maturity, zcb[1, ], ylim = range(zcb), type = "l", ylab = "yield (%)", col = "red")

# Add a line for the last day of zcb
lines(maturity, zcb[nrow(zcb), ])

## Chapter 2
# Calculate average and standard deviation of djx
mu <- mean(djx)
sigma <- sd(djx)

# Plot histogram of djx
hist(djx, nclass = 20, probability = TRUE)

# Add the normal density as a red line to histogram
lines(djx, dnorm(djx, mean = mu, sd = sigma), col = "red")

# Plot non-parametric KDE of djx
plot(density(djx))

# Add the normal density as red line to KDE
lines(djx, dnorm(djx, mean = mu, sd = sigma), col = "red")

# Make a Q-Q plot of djx and add a red line
qqnorm(djx)
qqline(djx, col = "red")

# Calculate the length of djx as n
n <- length(djx)

# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(n)
qqnorm(x1)
qqline(x1, col = "red")

# Generate n Student t variables, make a Q-Q plot, add a red line
x2 <- rt(n, df = 4)
qqnorm(x2)
qqline(x2, col = "red")

# Generate n standard uniform variables, make a Q-Q plot, add red line
x3 <- runif(n)
qqnorm(x3)
qqline(x3, col = "red")

# Calculate skewness and kurtosis of djx
skewness(djx, na.rm = FALSE)
kurtosis(djx, na.rm = FALSE)

# Carry out a Jarque-Bera test for djx
jarque.test(djx)

# Calculate skewness and kurtosis of djreturns 
s <- apply(djreturns, 2, skewness)
k <- apply(djreturns, 2, kurtosis)

# Plot k against s and add text labels to identify stocks
plot(s, k, type = "n")
text(s, k, names(s), cex = 0.6)

# Carry out Jarque-Bera tests for each constituent in djreturns
apply(djreturns, 2, jarque.test)

# Calculate weekly and monthly log-returns from djx_d
djx_w <- apply.weekly(djx_d, colSums)
djx_m <- apply.monthly(djx_d, colSums)

# Calculate the p-value for each series in djx_d
apply(djx_d, 2, function(v){jarque.test(v)$p.value})

# Calculate the p-value for each series in djx_w
apply(djx_w, 2, function(v){jarque.test(v)$p.value})

# Calculate the p-value for each series in djx_m
apply(djx_m, 2, function(v){jarque.test(v)$p.value})

# Calculate a 21-day moving sum of djx
djx21 <- rollapplyr(djx, width = 21, FUN = sum)[-(1:20)]

# Calculate a 63-day moving sum of djx
djx63 <- rollapplyr(djx, width = 63, FUN = sum)[-(1:62)]

# Merge the three series and plot
djx2 <- merge(djx, djx21, djx63, all = FALSE)
plot.zoo(djx2)

# Compute the skewness and kurtosis for each series in djx2
apply(djx2, 2, kurtosis)
apply(djx2, 2, skewness)

# Conduct the Jarque-Bera test to each series in djx2
apply(djx2, 2, jarque.test)

# Fit a Student t distribution to djx
tfit <- fit.st(djx)

# Define tpars, nu, mu, and sigma
tpars <- tfit$par.ests
nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot a histogram of djx
hist(djx, nclass = 20, probability = TRUE, ylim = range(0, 40))

# Compute the fitted t density at the values djx
yvals <- dt((djx - mu)/sigma, df = nu)/sigma

# Superimpose a red line to show the fitted t density
lines(djx, yvals, col = "red")

# Plot the daily log-return series in fx_d
plot.zoo(fx_d)

# Apply the Jarque-Bera test to each of the series in fx_d
apply(fx_d, 2, jarque.test)

# Plot the monthly log-return series in fx_m
plot.zoo(fx_m)

# Apply the Jarque-Bera test to each of the series in fx_m
apply(fx_m, 2, jarque.test)

# Fit a Student t distribution to each of the series in fx_m
apply(fx_m, 2, function(v){fit.st(v)$par.ests})

# Plot the interest-rate return series zcbx_m and zcbx2_m
plot.zoo(zcbx_m, type = "h")
plot.zoo(zcbx2_m, type = "h")

# Make Q-Q plots of the 3rd component series of zcbx_m and zcbx2_m
qqnorm(zcbx_m[,3])
qqnorm(zcbx2_m[,3])

# Compute the kurtosis of each series in zcbx_m and zcbx2_m
apply(zcbx_m, 2, kurtosis)
apply(zcbx2_m, 2, kurtosis)

# Conduct the Jarque-Bera test on each series in zcbx_m and zcbx2_m
apply(zcbx_m, 2, jarque.test)
apply(zcbx2_m, 2, jarque.test)

## Chapter 3
# Compute the length n of djx 
n <- length(djx)

#  Generate a normal sample of size n with parameters given by npars
ndata <- rnorm(n)*npars[2] + npars[1]

# Generate a t-distributed sample of size n with paramaters given by tpars
tdata <- rt(n, df = tpars[1])*tpars[3] + tpars[2]

# Make ndata and tdata into xts objects
ndatax <- xts(ndata, time(djx))
tdatax <- xts(tdata, time(djx))

# Merge djx, ndatax, and tdatax and plot
alldata <- merge(djx, ndatax, tdatax)
plot.zoo(alldata, type = "h", ylim = range(alldata))

# Set up a plot region to show 3 plots at a time
par(mfrow = c(3, 1))

# Plot the acfs of djx, ndata and tdata
acf(djx)
acf(ndata)
acf(tdata)

# Plot the acfs of the absolute values
acf(abs(djx))
acf(abs(ndata))
acf(abs(tdata))

# Plot the acfs of the squares of the values
acf(djx^2)
acf(ndata^2)
acf(tdata^2)

# Apply the Ljung-Box test to djx
Box.test(djx, lag = 10, type = "Ljung")

# Apply the Ljung-Box test to absolute values of djx
Box.test(abs(djx), lag = 10, type = "Ljung")

# Apply the Ljung-Box test to all return series in djall
apply(djall, 2, Box.test, lag = 10, type = "Ljung")

# Apply the Ljung-Box test to absolute values of all returns in djall
apply(abs(djall), 2, Box.test, lag = 10, type = "Ljung")

# Create monthly log-returns from djx
djx_m <- apply.monthly(djx, sum)

# Apply Ljung-Box tests to raw and absolute values of djx_m
Box.test(djx_m, lag = 10, type = "Ljung")
Box.test(abs(djx_m), lag = 10, type = "Ljung")

# Create monthly log-returns from djall
djall_m <- apply.monthly(djall, colSums)

# Apply Ljung-Box tests to raw and absolute values of djall_m
apply(djall_m, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(djall_m), 2, Box.test, lag = 10, type = "Ljung")

# Partition plotting area into 3 pieces
par(mfrow = c(1, 3))

# Plot djx_extremes
plot(djx_extremes, type = "h")

# Compute the spaces between the times of the extremes
djx_spaces <- diff(time(djx_extremes))

# Make a histogram of these spaces
hist(as.numeric(djx_spaces))

# Make a Q-Q plot of djx_spaces against exp_quantiles
qqplot(exp_quantiles, djx_spaces)

# Carry out the previous 4 steps for iid_extremes
plot(iid_extremes, type = "h")
iid_spaces <- diff(time(iid_extremes))
hist(as.numeric(iid_spaces))
qqplot(exp_quantiles, iid_spaces)

# Make a time series plot of indexes with plot.zoo and a pairwise scatterplot with pairs
plot.zoo(indexes)
pairs(as.zoo(indexes))

# Calculate the sample correlation matrix of indexes
cor(indexes)

# Plot the sample acfs and cross-correlation functions for the returns in indexes
acf(indexes)

# Plot the sample acfs and cross-correlations functions for the absolute values of indexes
acf(abs(indexes))

# Plot fx and fx_w
plot.zoo(fx, type = "h")
plot.zoo(fx_w, type = "h")

# Make acf plots of fx and the absolute values of fx
acf(fx)
acf(abs(fx))

# Apply the Ljung-Box test to the components of fx and their absolute values
apply(fx, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(fx), 2, Box.test, lag = 10, type = "Ljung")

# Make acf plots of fx_w and the absolute values of fx_w
acf(fx_w)
acf(abs(fx_w))

# Apply the Ljung-Box test to the components of fx_w and their absolute values
apply(fx_w, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(fx_w), 2, Box.test, lag = 10, type = "Ljung")

# Make acf plots of zcb_x and the absolute values of zcb_x
acf(zcb_x)
acf(abs(zcb_x))

# Apply the Ljung-Box test to the components of zcb_x and their absolute values
apply(zcb_x, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(zcb_x), 2, Box.test, lag = 10, type = "Ljung")

# Make acf plots of zcbx_m and the absolute values of zcbx_m
acf(zcbx_m)
acf(abs(zcbx_m))

# Apply the Ljung-Box test to the components of zcbx_m and their absolute values
apply(zcbx_m, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(zcbx_m), 2, Box.test, lag = 10, type = "Ljung")

## Chapter 4
# Make a sequence of 100 x-values going from -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals, ndens, type = "l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(p = 0.99, mean = mu, sd = sigma)
ES99 <- ESnorm(p = 0.99, mu = mu, sd = sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")

# Plot the risk-factor data
plot.zoo(riskfactors)

# Calculate the log-returns, assign to returns, and plot
returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns)

# Use apply() to carry out the Jarque-Bera test for all 5 series
apply(returns, 2, jarque.test)

# Make a Q-Q plot against normal for the 5th return series and add a reference line
qqnorm(returns[, 5])
qqline(returns[, 5])

# Make a picture of the sample acfs for returns and their absolute values
acf(returns)
acf(abs(returns))

# Calculate the loss from a log-return of -0.1 for all risk factors
lossop(rep(-0.1, 5))

# Apply lossop() to returns and plot hslosses
hslosses <- lossop(returns)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)

# Plot the sample acf of hslosses and their absolute values
acf(hslosses)
acf(abs(hslosses))

# Estimate the 99th sample percentile of the distribution of hslosses
quantile(hslosses, 0.99)

# Estimate the 99% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.99)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99% quantile of a normal distribution
qnorm(p = 0.99, mu, sigma)

# Compute the 99% ES of a normal distribution
ESnorm(p = 0.99, mu, sigma)

# Set the interest rate r to be 0.01, the volatility sigma to be 0.2 and the strike K to be 100
r <- 0.01
sigma <- 0.2
K <- 100

# Look at the arguments of the Black_Scholes function
args(Black_Scholes)

# Price a European call option that matures in one year if the current stock price is 80
Black_Scholes(0, 80, r, sigma, K, 1, "call")

# Price a European call option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1, "call")

# Price a European put option that matures in one year if the current stock price is 80
Black_Scholes(0, 80, r, sigma, K, 1,"put")

# Price a European put option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1, "put")

# Plot the risk factors and the log-returns
plot.zoo(riskfactors)
plot.zoo(returns)

# Make a scatterplot of the two return series
plot(as.matrix(returns))

# Apply the Jarque-Bera test to the returns and make a Q-Q plot of the volatility log-returns
apply(returns, 2, jarque.test)
qqnorm(returns$X.VIX)

# Create the sample acf of the returns and absolute returns
acf(returns)
acf(abs(returns))

# Calculate the correlation between the log-returns
cor(returns)

# Calculate the first loss
lossop(c(-0.1, -0.1), S = 80, sigma = 0.2)

# Calculate the second loss
lossop(c(-0.1, 0.1), S = 100, sigma = 0.2)

# Create and plot hslosses
hslosses <- lossop(returns, S = 100, sigma = 0.2)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)

# Plot the sample acf of raw data and absolute values in hslosses
acf(hslosses)
acf(abs(hslosses))

# Estimate the 99.5% percentile of the distribution
quantile(hslosses, 0.995)

# Estimate the 99.5% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.995)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99.5% quantile of a normal distribution
qnorm(p = 0.995, mu, sigma)

# Compute the 99.5% ES of a normal distribution
ESnorm(p = 0.995, mu, sigma)

returns_w <- apply.weekly(returns, colSums)
hslosses <- lossop(returns_w, S = 120, sigma = 0.25)
plot(hslosses)
mu <- mean(hslosses)
sigma <- sd(hslosses)
qnorm(p = 0.99, mu, sigma)
# [1] 0.1634535
quantile(hslosses, 0.99)
#     99% 
# 0.193767