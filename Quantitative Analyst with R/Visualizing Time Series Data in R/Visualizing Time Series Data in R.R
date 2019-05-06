## Chapter 1
# Display the first few lines of the data
head(data)

# Display the column names of the data
colnames(data)

# Plot yahoo data and add title
plot(data$yahoo, main = "yahoo")

# Replot yahoo data with labels for X and Y axes
plot(data$yahoo, main = "yahoo", xlab = "date", ylab = "price")

# Plot the second time series and change title
plot(data$microsoft, main = "microsoft")

# Replot with same title, add subtitle, use bars
plot(data$microsoft, main = "microsoft", sub = "Daily closing price since 2015", type = "h")

# Change line color to red
lines(data$microsoft, col = "red")

# Plot two charts on same graphical window
par(mfrow = c(2, 1))
plot(data$yahoo, main = "yahoo")
plot(data$microsoft, main = "microsoft")

# Replot with reduced margin and character sizes
par(mfrow = c(2, 1), mex = 0.6, cex = 0.8)
plot(data$yahoo, main = "yahoo")
plot(data$microsoft, main = "microsoft")

# Plot the "microsoft" series
plot(data$microsoft, main = "Stock prices since 2015")

# Add the "dow_chemical" series in red
lines(data$dow_chemical, col = "red")

# Add a Y axis on the right side of the chart
axis(4, pretty(data$dow_chemical))

# Add a legend in the bottom right corner
legend(x = "bottomright", legend = c("microsoft", "dow_chemical"), col = c("black", "red"), lty = c(1, 1))

# Plot the "citigroup" time series
plot(data$citigroup, main = "Citigroup")

# Create vert_line to identify January 4th, 2016 in citigroup
vert_line <- which(index(data$citigroup) == as.Date("2016-01-04"))

# Add a red vertical line using vert_line
abline(v = .index(data$citigroup)[vert_line], col = "red")

# Create hori_line to identify average price of citigroup
hori_line <- mean(data$citigroup)

# Add a blue horizontal line using hori_line
abline(h = hori_line, col = "blue")

# Create period to hold the 3 months of 2015
period = c("2015-01/2015-03")

# Highlight the first three months of 2015 
chart.TimeSeries(data$citigroup, period.areas = period)

# Highlight the first three months of 2015 in light grey
chart.TimeSeries(data$citigroup, period.areas = period, period.color = "lightgrey")

# Plot the microsoft series
plot(data$microsoft, main = "Dividend date and amount")

# Add the citigroup series
lines(data$citigroup, col = "orange", lwd = 2)

# Add a new y axis for the citigroup series
axis(4, pretty(data$citigroup), col = "orange")

# Same plot as the previous exercise
plot(data$microsoft, main = "Dividend date and amount")
lines(data$citigroup, col = "orange", lwd = 2)
axis(side = 4, at = pretty(data$citigroup), col = "orange")

# Create the two legend strings
micro <- paste0("Microsoft div. of ", micro_div_value," on ", micro_div_date)
citi <- paste0("Citigroup div. of ", citi_div_value," on ", citi_div_date)

# Create the legend in the bottom right corner
legend(x = "bottomright", legend = c(micro, citi), col = c("black", "orange"), lty = c(1, 1))

## Chapter 2
# Plot Apple's stock price 
plot(data$apple, main = "Apple stock price")

# Create a time series called rtn
rtn <- ROC(data$apple)

# Plot Apple daily price and daily returns 
par(mfrow = c(1, 2))
plot(data$apple)
plot(rtn)

# Create a histogram of Apple stock returns
hist(rtn,
      probability = TRUE,
      main = "Apple stock return distribution")

# Add a density line
lines(density(rtn))

# Redraw a thicker, red density line
lines(density(rtn), lwd = 2, col = "red")

# Draw box and whisker plot for the Apple returns
boxplot(rtn)

# Draw a box and whisker plot of a normal distribution
boxplot(rnorm(1000))

# Redraw both plots on the same graphical window
par(mfrow = c(2, 1))
boxplot(rtn, horizontal = TRUE)
boxplot(rnorm(1000), horizontal = TRUE)

# Draw autocorrelation plot
acf(rtn, main = "Apple return autocorrelation")

# Redraw with a maximum lag of 10
acf(rtn, lag.max = 10, main = "Apple return autocorrelation")

# Create q-q plot
qqnorm(rtn, main = "Apple return QQ-plot")

# Add a red line showing normality
qqline(rtn, col = "red")

# Draw histogram and add red density line
hist(rtn, probability = TRUE)
lines(density(rtn), col = "red")

# Draw box and whisker plot
boxplot(rtn)

# Draw autocorrelogram
acf(rtn)

# Draw q-q plot and add a red line for normality
qqnorm(rtn)
qqline(rtn, col = "red")

# Set up 2x2 graphical window
par(mfrow = c(2, 2))

# Recreate all four plots
hist(rtn, probability = TRUE)
lines(density(rtn), col = "red")

boxplot(rtn)

acf(rtn)

qqnorm(rtn)
qqline(rtn, col = "red")

## Chapter 3
# Plot stacked barplot
barplot(portfolio)

# Plot grouped barplot
barplot(portfolio, beside = TRUE)

# Draw the scatterplot
plot(x = sp500, y = citi)

# Draw a regression line
abline(reg = lm(citi ~ sp500), col = "red", lwd = 2)

# Create correlation matrix using Pearson method
cor(my_data, method = "pearson")

# Create correlation matrix using Spearman method
cor(my_data, method = "spearman")

# Create scatterplot matrix
pairs(my_data)

# Create upper panel scatterplot matrix
pairs(my_data, lower.panel = NULL)

# Create correlation matrix
corrplot(cor_mat)

# Create correlation matrix with numbers
corrplot(cor_mat, method = "number")

# Create correlation matrix with colors
corrplot(cor_mat, method = "color")

# Create upper triangle correlation matrix
corrplot(cor_mat, method = "number", type = "upper")

# Draw heatmap of cor_mat
corrplot(cor_mat, method = "color")

# Draw upper heatmap
corrplot(cor_mat, method = "color", type = "upper")

# Draw the upper heatmap with hclust
corrplot(cor_mat, method = "color", type = "upper", order = "hclust")

## Chapter 4
# Plot the portfolio value
plot(data$value, main = "Portfolio Value")

# Plot the portfolio return
plot(data$return, main = "Portfolio Return")

# Plot a histogram of portfolio return 
hist(data$return, probability = TRUE)

# Add a density line
lines(density(data$return), lwd = 2, col = "red")

# Plot the four stocks on the same graphical window
par(mfrow = c(2, 2), mex = 0.8, cex = 0.8)
plot(data$GS)
plot(data$KO)
plot(data$DIS)
plot(data$CAT)

# Draw the scatterplot of gs against the portfolio
plot(gs, portfolio)

# Add a regression line in red
abline(reg = lm(gs ~ portfolio), col = "red", lwd = 2)

# Plot scatterplots and regression lines to a 2x2 window
par(mfrow = c(2, 2))
plot(gs, portfolio)
abline(reg = lm(gs ~ portfolio), col = "red", lwd = 2)
plot(ko, portfolio)
abline(reg = lm(ko ~ portfolio), col = "red", lwd = 2)
plot(dis, portfolio)
abline(reg = lm(dis ~ portfolio), col = "red", lwd = 2)
plot(cat, portfolio)
abline(reg = lm(cat ~ portfolio), col = "red", lwd = 2)

# Plot new and old portfolio values on same chart
plot(old.vs.new.portfolio$old.portfolio.value)
lines(old.vs.new.portfolio$new.portfolio.value, col = "red")

# Plot density of the new and old portfolio returns on same chart
plot(density(old.vs.new.portfolio$old.portfolio.rtn), col = "black")
lines(density(old.vs.new.portfolio$new.portfolio.rtn), col = "red")

# Draw value, return, drawdowns of old portfolio
charts.PerformanceSummary(old.vs.new.portfolio$old.portfolio.rtn)

# Draw value, return, drawdowns of new portfolio
charts.PerformanceSummary(old.vs.new.portfolio$new.portfolio.rtn)

# Draw both portfolios on same chart
charts.PerformanceSummary(old.vs.new.portfolio[, c(3, 4)])

