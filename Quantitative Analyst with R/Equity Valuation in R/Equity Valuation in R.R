## Chapter 1
# Calculate PV of $100 one year from now
pv_1 <- 100 / (1 + r)
pv_1

# Calculate PV of $100 two years from now
pv_2 <- 100 / (1 + r)^2
pv_2

# Calculate FCFE
fcfe <- after_tax_income + depn_amort - capex - incr_wc
fcfe

# Calculate the terminal value as of 2021
tv_2021 <- (fcfe_2021 * (1 + g)) / (ke - g)
tv_2021

# Add discount periods
fcfe$periods <- seq(1, 5, 1)
fcfe

# Calculate Present Value Factor
fcfe$pv_factor <- 1 / (1 + ke)^fcfe$periods
fcfe

# Calculate Present Value of each Cash Flow
fcfe$pv <- fcfe$fcfe * fcfe$pv_factor
fcfe

# Total Present Value
pv_fcfe <- sum(fcfe$pv)
pv_fcfe

# Calculate Present Value
pv_tv <- tv_2021 / (1 + ke)^5
pv_tv

# Calculate Equity Value
eq_val <- pv_fcfe + pv_tv
eq_val

# Calculate Equity Value Per Share
eq_val_per_share <- eq_val / shout
eq_val_per_share

## Chapter 2
# Combine hist_rev and rev_proj
rev_split <- rbind(hist_rev, rev_proj)

# Rename the column headers
colnames(rev_split) <- seq(2009, 2021, 1)

# Create a bar plot of the data
barplot(rev_split,
        col = c("red", "blue"),
        main = "Historical vs. Projected Revenues")
legend("topleft",
       legend = c("Historical", "Projected"),
       fill = c("red", "blue"))

# Create a trend variable
rev_all$trend <- seq(1, 13, 1)

# Create shift variable
rev_all$shift <- c(rep(0, 8), rep(1, 5))

# Run regression
reg <- lm(rev_proj ~ trend + shift, data = rev_all)

# Print regression summary
summary(reg)

# Calculate reinvestment amount
reinvestment <- capex[5] + incr_wc[5] - depn_amort[5]  
reinvestment

# Calculate retention ratio
retention_ratio <- reinvestment / after_tax_income[5]
retention_ratio

# Calculate expected growth rate
exp_growth_rate <- retention_ratio * ke
exp_growth_rate

# Calculate dividend of preferred stock
div <- stated_value * div_rate
div

# Calculate value of preferred stock
pref_value <- div / kp
pref_value

# Value of Preferred if dividends start five years from now
pref_value_yr5 <- (stated_value * div_rate) / (kp)
pref_value_yr5

# Value discounted to present
pref_value <- pref_value_yr5 / (1 + kp)^5
pref_value

# Preferred dividend in Years 1 to 5
high_div <- 2.5

# Create vector of Year 1-5 dividends
pref_cf <- rep(high_div, 5)

# Convert to data frame
pref_df <- data.frame(pref_cf)

# Add discount periods
pref_df$periods <- seq(1, 5, 1)

# Calculate discount factors
pref_df$pv_factor <- 1 / (1 + kp)^pref_df$periods

# Calculate PV of dividends
pref_df$pv_cf <- pref_df$pref_cf * pref_df$pv_factor

# Calculate value during high stage
pref_value_high <- sum(pref_df$pv_cf)

# Calculate value of the preferred stock
pref_value_high + pref_value_low

## Chapter 3
# Show first six observations of prices
head(prices)

# Calculate MYL monthly return
rets <- Delt(prices$myl_prc)

# Calculate SPY monthly return
rets$spy <- Delt(prices$spy_prc)

# Change label of first variable
names(rets)[1] <- "myl"

# Remove first observation - NA
rets <- rets[-1,]

# Run regression
reg <- lm(myl ~ spy, data = rets)

# Save beta
beta <- summary(reg)$coeff[2]
beta

# Calculate the Mylan Unlevered Beta Fernandez Formula
myl_unl_beta <- (myl_beta + debt_beta * (1 - 0.4) * myl_debt_eq) / (1 + (1 - 0.4) * myl_debt_eq)
myl_unl_beta

# Calculate levered beta Fernandez Formula
beta <- med_beta + ((med_beta - debt_beta) * (1 - 0.4) * debt_eq)
beta

# Review treas
head(treas)

# Extract 2016-12-30 yield 
rf <- subset(treas, treas$date == "2016-12-30")
rf

# Keep only the observation in the second column
rf_yield <- rf[, 2]
rf_yield

# Convert yield to decimal terms
rf_yield_dec <- rf_yield / 100
rf_yield_dec

# Review the first six rows of damodaran
head(damodaran)

# Calculate annual difference between stocks and bonds
diff <- damodaran$sp_500 - damodaran$tbond_10yr

# Calculate ERP
erp <- mean(diff)
erp

capm_coe <- rf + relevered_beta * erp
capm_coe

## Chapter 4
# Review the first six rows of midcap400
head(midcap400)

# Subset Pharmaceuticals firms
pharma <- subset(midcap400, midcap400$gics_subindustry == "Pharmaceuticals")
pharma

# Calculate P/LTM EPS
pharma$ltm_p_e <- ifelse(pharma$ltm_eps < 0, NA, pharma$price / pharma$ltm_eps)

# Calculate P/NTM EPS
pharma$ntm_p_e <- ifelse(pharma$ntm_eps < 0, NA, pharma$price / pharma$ntm_eps)

# Calculate P/BVPS
pharma$p_bv <- ifelse(pharma$bvps < 0, NA, pharma$price / pharma$bvps)
pharma

# Show contents of pharma
pharma

# Calculate average multiples
multiples <- colMeans(pharma[,2:4], na.rm = TRUE)
multiples

# Vector of metrics
metrics <- c(1, 2, 8)

# Calculate implied values
implied_val <- metrics * multiples
implied_val

# Calculate ROE
cons_disc$roe <- cons_disc$ltm_eps / cons_disc$bvps

# Calculate Price to Book ratio
cons_disc$p_bv <- ifelse(cons_disc$bvps <= 0, NA, cons_disc$price / cons_disc$bvps)

# Remove NA
cons_disc_no_na <- cons_disc[complete.cases(cons_disc), ]
head(cons_disc_no_na)

# Set x-axis range
x.range <- c(min(cons_disc$roe), 
             max(cons_disc$roe))

# Set y-axis range
y.range <- c(min(cons_disc$p_bv), 
             max(cons_disc$p_bv))

# Plot data
plot(y = cons_disc$p_bv,
     x = cons_disc$roe,
     xlab = "Return on Equity",
     ylab = "Price-to-Book",
     xlim = x.range,
     ylim = y.range,
     col = "blue",
     main = "Price-to-Book Value and Return on Equity
     Of Mid-Cap Consumer Discretionary Firms")

# Regress roe on p_bv
reg <- lm(p_bv ~ roe, data = cons_disc)

# Add trend line in red
abline(reg, col = "red")

# Regression model 
reg <- lm(p_bv ~ roe, data = cons_disc)

# Regression summary
summary_reg <- summary(reg)
summary_reg

# Store intercept
a <- summary_reg$coefficients[1]
a

# Store beta
b <- summary_reg$coefficients[2]
b

# Calculate implied P/B
implied_p_b <- a + 0.2 * b
implied_p_b

# Calculate implied price
implied_price <- 8 * implied_p_b
implied_price

## Chapter 5
# Create a bar chart
barplot(rev,
    col = c("red", "blue"),
    main = "Historical vs. Projected Revenues")

# Add legend
legend("topleft",
       legend = c("Historical", "Projected"),
       fill = c("red", "blue"))

# Create a data frame of single series
rev_all <- colSums(rbind(hist_rev, proj_rev))
rev_all_df <- data.frame(rev_all)

# Create Trend Variable
rev_all_df$trend <- seq(1, nrow(rev_all_df), 1)

# Create Shift Variable
rev_all_df$shift <- ifelse(rev_all_df$trend <= 7, 0, 1)

# Run regression
reg <- lm(rev_all ~ trend + shift, data = rev_all_df)
summary(reg)

# Subset Treasury data to 12/30/16
rf <- subset(treas, treas$date == "2016-12-30")

# Keep 2nd column
rf_yield <- rf[, 2]

# Convert to decimal terms
rf_yield_dec <- rf_yield / 100
rf_yield_dec

# Calcualte difference between S&P 500 Return and Treasury Return
diff <- damodaran$sp_500 - damodaran$tbond_10yr

# Calculate average difference
erp <- mean(diff)
erp

# Calculate CAPM Cost of Equity
ke <- rf_yield_dec + beta * erp
ke

# Calculate Discount Periods to 12/31/2016
fcfe$disc_periods <- seq(1, nrow(fcfe), 1)

# Calculate discount factor
fcfe$disc_factor <- 1 / (1 + ke)^fcfe$disc_periods

# Calculate PV of each period's total free cash flow
fcfe$pv <- fcfe$fcfe * fcfe$disc_factor

# Calculate Projection Period Value
pv_proj_period <- sum(fcfe$pv)
pv_proj_period

# Extract 2021 FCFE
fcfe_2021 <- fcfe[5]

# Use perpetuity with growth formula to calculate terminal value
tv_2021 <- (fcfe_2021 * (1 + pgr)) / (ke - pgr)
tv_2021

# Calculate PV of Terminal Value
pv_terminal <- tv_2021 / (1 + ke)^5
pv_terminal

# Calculate agggregate equity value
equity_value_fcfe <- pv_proj_period + pv_terminal
equity_value_fcfe

# Calculate equity value per share
equity_value_fcfe_per_share <- equity_value_fcfe / shout
equity_value_fcfe_per_share

# Use DDM to Calculate Equity Value
equity_value_ddm <- (div * (1 + pgr)) / (ke - pgr)
equity_value_ddm

# Calculate Implied Equity Value
equity_value_p_e <- eps * p_eps_multiple
equity_value_p_e

# Combine equity values per share
eq_val_per_share <- data.frame(
  DCF = eq_val_fcfe_per_share,
  DDM = eq_val_ddm_per_share,
  "P/E" = eq_val_p_e_per_share,
  check.names = FALSE
)

# See the result
eq_val_per_share