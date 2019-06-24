## Chapter 1
# Set the sales variable and cost of goods sold (cogs) variables
sales = 8000
cogs = 5400

# Calculate the gross profit (gross_profit)
gross_profit = sales - cogs

# Print the gross profit
print("The gross profit is {}.".format(gross_profit))

# Calculate the gross profit margin
gross_profit_margin = gross_profit / sales

# Print the gross profit margin
print("The gross profit margin is {}.".format(gross_profit_margin))

# Create and print the opex list
opex = [admin, travel, training, marketing, insurance]
print(opex)

# Calculate and print net profit
net_profit = gross_profit - sum(opex)
print("The net profit is {}.".format(net_profit))

# Set variables units sold and sales price of the T-shirts (basic and extensive)
salesprice_basic = 15
salesprice_custom = 25

# Calculate the combined sales price taking into account the sales mix
average_sales_price = (salesprice_basic * 0.6) + (salesprice_custom * 0.4)

# Calculate the total sales for next month
sales_USD = forecast_units * average_sales_price

# Print the total sales
print("Next month's forecast sales figure is {:.2f} USD.".format(sales_USD))

# Forecast the sales of January
sales_january = units_january * sales_price

# Forecast the discounted price
dsales_price = sales_price * 0.6

# Forecast the sales of February
sales_february = (sales_price * units_february * 0.55) + (dsales_price * units_february * 0.45)

# Print the forecast sales for January and February
print("The forecast sales for January and February are {} and {} USD respectively.".format(sales_january, sales_february))

# From previous step
fixed_costs = machine_rental 
variable_costs_per_unit = material_costs_per_unit + labour_costs_per_unit
cogs_jan = (units_jan * variable_costs_per_unit) + fixed_costs
cogs_feb = (units_feb * variable_costs_per_unit) + fixed_costs

# Calculate the unit cost for January and February
unit_cost_jan = cogs_jan / units_jan
unit_cost_feb = cogs_feb / units_feb

# Print the January and February cost per unit
print("The cost per unit for January and February are {} and {} USD respectively.".format(unit_cost_jan, unit_cost_feb))

# Calculate the break-even point (in units) for Wizit
break_even = fixed_costs / (sales_price - variable_costs_per_unit)

# Print the break even point in units
print("The break even point is {} units.".format(break_even))

# Forecast the gross profit for January and February
gross_profit_jan = (sales_price * units_jan) - cogs_jan
gross_profit_feb = (sales_price * units_feb) - cogs_feb

# Print the gross profit for January and February
print("The gross profit for January and February are {} and {} USD respectively.".format(gross_profit_jan, gross_profit_feb))

# Choose some interesting metrics
interesting_metrics = ['Revenue', 'Gross profit', 'Total operating expenses', 'Net income']

# Filter for rows containing these metrics
filtered_income_statement = income_statement[income_statement.metric.isin(interesting_metrics)]

# See the result
print(filtered_income_statement)

# Forecasting profit for Tesla
revenue_metric = ['Revenue']

# Filter for rows containing the revenue metric
filtered_income_statement = income_statement[income_statement.metric.isin(revenue_metric)]

# Get the number of columns in filtered_income_statement
n_cols = len(filtered_income_statement.columns)

# Insert a column in the correct position containing the column 'Forecast'
filtered_income_statement.insert(n_cols, 'Forecast', 13000) 

# See the result
print(filtered_income_statement)

## Chapter 2
# Create the list for sales, and empty lists for debtors and credits
sales = [500, 350, 700]
debtors = [] 
credits = []

# Create the statement to append the calculated figures to the debtors and credits lists
for mvalue in sales: 
    credits.append(mvalue * 0.6)
    if month > 0:
        debtors.append(credits[month] + credits[month-1]) 
    else:
        debtors.append(credits[month])
    month += 1
# Print the result
print("The ‘Debtors’ are {}.".format(debtors))

# Calculate the bad debts for February
bad_debts_feb = 500 * 0.3

# Calculate the feb debtors amount
debtors_feb = (debtors_jan - bad_debts_feb)

# Print the debtors for January and the bad debts and the debtors for February
print("The debtors are {} in January, {} in February. February's bad debts are {} USD.".format(debtors_jan, debtors_feb, bad_debts_feb))

# Set the cost per unit
unit_cost = 0.25

# Create the list for production units and empty list for creditors
production = [1000, 1200]
creditors = []

# Calculate the accounts payable for January and February
for mvalue in production: 
    creditors.append(mvalue * unit_cost * 0.5)
    
# Print the creditors balance for January and February
print("The creditors balance for January and February are {} and {} USD.".format(creditors[0], creditors[1]))

# Debtor days ratio
# Create the variables
debtors_end = 650
sales_tot = 12500

# Calculate the debtor days variable
ddays_ratio = (debtors_end / sales_tot) * 365

# Print the result
print("The debtor days ratio is {}.".format(ddays_ratio))

# Days payable outstanding
# Get the variables
cogs_tot = 4000
creditors_end = 650

# Calculate the days payable outstanding
dpo = (creditors_end / cogs_tot) * 365

# Print the days payable outstanding
print("The days payable outstanding is {}.".format(dpo))

# Days in inventory and asset turnover ratio
# Calculate the DII Ratio
dii_ratio = (av_inv/cogs_tot)*365

# Print the result
print("The DII ratio is {}.".format(dii_ratio))

# Calculate the Average Assets
av_assets = (ob_assets + cb_assets) / 2

# Calculate the Asset Turnover Ratio
at_ratio = sales_tot / av_assets

# Print the Asset Turnover Ratio
print("The asset turnover ratio is {}.".format(at_ratio))

# From previous step
receivables_metric = ['Receivables']
filtered_balance_sheet = balance_sheet[balance_sheet.metric.isin(receivables_metric)]

# Extract the zeroth value from the last time period (2017-12)
debtors_end = filtered_balance_sheet["2017-12"].iloc[0]

# Calculate the debtor days ratio
ddays = (debtors_end / sales) * 365

# Print the debtor days ratio
print("The debtor day ratio is {:.0f}. A higher debtors days ratio means it takes longer to collect cash from debtors.".format(ddays))

# Forecasting the balance sheet for Ford
# Calculate the forecasted sales 
f_sales = sales * (1 + 0.1)

# Solve for the forecasted debtors' ending balance
f_debtors_end = ddays * f_sales / 365

print("If sales rise by 10% and the debtor days decrease to {:.0f} then the forecasted closing balance for debtors will be {:.0f}.".format(ddays, f_debtors_end))

# From previous step
f_sales = sales * 1.10
f_debtors_end = ddays * f_sales / 365

# Get the number of columns in the filtered balance sheet
n_cols = len(filtered_balance_sheet.columns)

# Append a Forecast column of the forecasted debtors' end balance
filtered_balance_sheet.insert(n_cols, 'Forecast', f_debtors_end)

# See the result
print(filtered_balance_sheet)

## Chapter 3
# Create a list for quarters
quarters = [700, 650]

# Create a for loop to split the quarters into months into the new qrtlist
for qrt in quarters:
 month = qrt / 3
 qrtlist = [month, month, month]
 
# Print the result
print("The quarter list per month is {}.".format(qrtlist))

# Create a months list, as well as an index, and set the quarter to 0
months = [100, 100, 150, 250, 300, 10, 20]
quarter = 0
index = 1

# Create for loop for quarter, print result, and increment the index
print("The quarter totals are:")

for m in months:
    quarter = quarter + m
    if index % 3 == 0 or index == len(months):
        print(quarter)
        quarter = 0
    index = index + 1

# Import the datetime python library
from datetime import datetime

# Create a dt_object to convert the first date and print the month result
dt_object = datetime.strptime('14/02/2018', '%d/%m/%Y')
print(dt_object.month)

# Create a dt_object to convert the second date and print the month result
dt_object = datetime.strptime('2 March 2018', '%d %B %Y')
print(dt_object.month)

# Set the variable for the datetime to convert
dt = '14/02/2018'

# Create the dictionary for the month values
mm = {'01': 'January', '02': 'February', '03': 'March'}

# Split the dt string into the different parts
day, month, year = dt.split('/')

# Print the concatenated date string
print(day + ' ' + mm[month] + ' ' + year)

# Set the index to start at 0
index = 0 

# Create the dictionary for the months
tt = {'Jan': 0, 'Feb': 0, 'Mar': 0}

# Create a for loop that will iterate the date and amount values in the dataset
for date, amount in df.iteritems():
    # Create the if statement to split the day and month, then add it to the new tt variable
    if index > 0: 
        day, month = date.split('-')
        tt[month] +=float(amount[0])
    index += 1

print(tt)

# Working with datasets - combining dataset
tt = {'Jan': 0, 'Feb': 0, 'Mar': 0}
mm = {'01': 'Jan', '02': 'Feb', '03': 'Mar'}

for date, amount in df1.iteritems():
    if index > 0: 
        day, month, year = date.split('-')
        tt[month] +=float(amount[0]) 
    index += 1 

index = 0 
for date, amount in df2.iteritems():
    if index > 0: 
        day, month, year = date.split('/')
        tt[mm[month]] += float(amount[0])
    index += 1

print(tt)

## Chapter 4
# Weighted probability
# Create the combined list for sales and probability
sales_probability = ['0|0.05', '200|0.10', '300|0.40', '500|0.20', '800|0.25'] 
weighted_probability = 0

# Create a for loop to calculate the weighted probability
for pair in sales_probability:
    parts = pair.split('|')
    weighted_probability += float(parts[0]) * float(parts[1])

# Print the weighted probability result
print("The weighted probability is {}.".format(weighted_probability))

# Create the computevariance function
def computevariance(amount, sentiment):
 if (sentiment < 0.6):
  res = amount + (amount * 0.02)
 elif (sentiment > 0.8):
  res = amount + (amount * 0.07)
 else:
  res = amount + (amount * 0.05)
 return res

# Compute the variance for jan, feb and mar
jan = computevariance(500, 0.5)
feb = computevariance(500, 0.65)
mar = computevariance(500, 0.85)

print("The forecast sales considering variance due to market sentiment is {} for Jan, {} for Feb, and {} for Mar.".format(jan, feb, mar))

# Set the Sales Dependency
if sales >= 350:
    sales_dep = (350 * base_sales_price) + ((sales - 350) * (base_sales_price - 1))
else:
    sales_dep = sales * 2

# Print the results
print("The sales dependency is {} USD.".format(sales_dep))

# Set the Cost Dependency
if sales >= 500:
    cost_dep = (500 * base_cost_price) + ((sales - 500) * (base_cost_price + 2))
else:
    cost_dep = sales * base_cost_price
    
# Print the results
print("The cost dependency is {} USD.".format(cost_dep))

# Create the sales_usd list
sales_usd = [700, 350, 650]

# Create the if statement to calculate the forecast_gross_profit
for sales in sales_usd:
    if sales > 350:
        sales_dep = (350 * base_sales_price) + ((sales - 350) * (base_sales_price - 1))
    else:
        sales_dep = sales * base_sales_price
    if sales > 500:
        cost_dep = (500 * base_cost_price) + ((sales - 500) * (base_cost_price + 2))
    else:
        cost_dep = sales * base_cost_price
    forecast_gross_profit = sales_dep - cost_dep

    # Print the result
    print("The gross profit forecast for a sale unit value of {} is {} USD.".format(sales, forecast_gross_profit))

# Set the admin dependency
if emp_leave > 0:
    admin_dep = emp_leave * 80

# Print the results
print("The admin dependency for August is {} USD.".format(admin_dep))

# Build a sensitivity analysis for the net profit
admin_usd = [1500, 1500, 1500]
index = 0

print("The forecast net profit is:")
for admin in admin_usd:
    employee = emp_leave[index]
    if employee > 0:
        admin_dep = emp_leave[index] * 80 + admin
    else: 
        admin_dep = admin 
    forecast_net_profit = forecast_gross_profit[index] - admin_dep
    print(forecast_net_profit)
    index += 1

# Building an alternate forecast
def dependencies(base_cost_price, base_sales_price, sales_usd):
    res = []
    for sales in sales_usd:
        if sales >= 350:
            sales_dep = (350 * base_sales_price) + ((sales - 350) * (base_sales_price - 1))
        else:
            sales_dep = sales * base_sales_price
        if sales >= 500:
            cost_dep = (500 * base_cost_price) + ((sales - 500) * (base_cost_price + 2))
        else:
            cost_dep = sales * base_cost_price
        res.append(sales_dep - cost_dep)
    return res

forecast1 = dependencies(7, 15, [700, 350, 650])
forecast2 = dependencies(7, 15, [700, 220, 520])

print("The original forecast scenario is {}:".format(forecast1))
print("The alternative forecast scenario is {}:".format(forecast2))

# Building a gap analysis between forecasts
# Set the two results
forecast1 = dependencies(7, 15, [700, 350, 650])
forecast2 = dependencies(7, 15, [700, 220, 520])

# Create an index and the gap analysis for the forecast
index = 0
for value in forecast2:
    print("The gap between forecasts is {}".format(value - forecast1[index]))
    index += 1

# Setting dependencies for Netflix
# Create the filter
sales_metric = ['Sales']

# Filter for rows containing these metrics
filtered_netflix_f_is = netflix_f_is[netflix_f_is.metric.isin(sales_metric)]

# Extract the 2019 Sales forecast value
forecast1 = filtered_netflix_f_is['2019 fc'].iloc[0]

# Print the resulting forecast
print("The sales forecast is {}.".format(forecast1))

# Set the success percentage to 78%
pct_success = 78

# Calculate the dependency for the subscriber base
n_subscribers = n_subscribers_per_pp * pct_success

# See the result
print("The dependency for the subscriber base is {}.".format(n_subscribers))

# From previous steps
sales_metric = ['Sales']
filtered_netflix_f_is = netflix_f_is[netflix_f_is.metric.isin(sales_metric)]
forecast1 = filtered_netflix_f_is["2019 fc"].iloc[0]
pct_success = 78
n_subscribers = n_subscribers_per_pp * pct_success

# Calculate the ratio between subscribers and forecast sales
sales_subs_ratio = forecast1 / n_subscribers

# See the result
print("The ratio between subscribers and sales is 1 subscriber equals ${:.2f}.".format(sales_subs_ratio))

# Calculating an alternative forecast for Netflix
# Set the proportion of successes to 65%
pct_success2 = 65

# Calculate the number of subscribers
n_subscribers2 = n_subscribers_per_pp * pct_success2

# Calculate the new forecast
forecast2 = n_subscribers2 * sales_subs_ratio

# From previous step
pct_success2 = 65
n_subscribers2 = n_subscribers_per_pp * pct_success2
forecast2 = n_subscribers2 * sales_subs_ratio

# Append a column named Forecast2, containing forecast2
filtered_netflix_f_is.insert(len(filtered_netflix_f_is.columns), 'Forecast2', forecast2)

# Append a column named Gap, containing the difference
filtered_netflix_f_is.insert(len(filtered_netflix_f_is.columns), 'Gap', forecast1 - forecast2)

# See the result
print(filtered_netflix_f_is)