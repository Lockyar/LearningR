library(dplyr)
library(hflights)

# Both the dplyr and hflights packages are loaded

# Convert the hflights_df data.frame into a hflights tbl. It allows you to see the data as a table that fits in the console.
hflights <- tbl_df(hflights_df)

# Display the hflights tbl
hflights

# Create the object carriers
carriers <- hflights$UniqueCarrier

# Both the dplyr and hflights packages are loaded into workspace
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier]

# Glimpse at hflights
glimpse(hflights)

# The hflights tbl you built in the previous exercise is available in the workspace.

# The lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")

# Add the Code column
hflights$Code <- lut[hflights$CancellationCode]

# Glimpse at hflights
glimpse(hflights)

# select(), which returns a subset of the columns,
# filter(), that is able to return a subset of the rows,
# arrange(), that reorders the rows according to single or multiple variables,
# mutate(), used to add columns from existing data,
# summarize(), which reduces each group to a single row by calculating aggregate measures.

# hflights is pre-loaded as a tbl, together with the necessary libraries.

# Print out a tbl with the four columns of hflights related to delay
select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Print out the columns Origin up to Cancelled of hflights
select(hflights, Origin:Cancelled)

# Answer to last question: be concise!
names(hflights)
select(hflights, Year:DayOfWeek, ArrDelay:Diverted)

# dplyr comes with a set of helper functions that can help you select groups of variables inside a select() call:
# starts_with("X"): every name that starts with "X",
# ends_with("X"): every name that ends with "X",
# contains("X"): every name that contains "X",
# matches("X"): every name that matches "X", where "X" can be a regular expression,
# num_range("x", 1:5): the variables named x01, x02, x03, x04 and x05,
# one_of(x): every name that appears in x, which should be a character vector.

# As usual, hflights is pre-loaded as a tbl, together with the necessary libraries.

# Print out a tbl containing just ArrDelay and DepDelay
select(hflights, contains("Delay"))

# Print out a tbl as described in the second instruction, using both helper functions and variable names
select(hflights, UniqueCarrier, ends_with("Num"), contains("Cancell"))

# Print out a tbl as described in the third instruction, using only helper functions.
select(hflights, ends_with("Time"), ends_with("Delay"))

# both hflights and dplyr are available

# Finish select call so that ex1d matches ex1r
ex1r <- hflights[c("TaxiIn", "TaxiOut", "Distance")]
ex1d <- select(hflights, contains("Taxi"), Distance)

# Finish select call so that ex2d matches ex2r
ex2r <- hflights[c("Year", "Month", "DayOfWeek", "DepTime", "ArrTime")]
ex2d <- select(hflights, Year:ArrTime, -DayofMonth)

# Finish select call so that ex3d matches ex3r
ex3r <- hflights[c("TailNum", "TaxiIn", "TaxiOut")]
ex3d <- select(hflights, starts_with("T"))

# mutate(df, z = x + y)
# hflights and dplyr are loaded and ready to serve you.

# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.
g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)

# Add the new variable GroundTime to g1. Save the result as g2.
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)

# Add the new variable AverageSpeed to g2. Save the result as g3.
g3 <- mutate(g2, AverageSpeed = 60 * Distance / AirTime)

# Print out g3
g3

# hflights and dplyr are ready, are you?

# Add a second variable loss_ratio to the dataset: m1
m1 <- mutate(hflights, loss = ArrDelay - DepDelay, loss_ratio = loss / DepDelay)

# Add the three variables as described in the third instruction: m2
m2 <- mutate(hflights, TotalTaxi = TaxiIn + TaxiOut, ActualGroundTime = ActualElapsedTime - AirTime, Diff = TotalTaxi - ActualGroundTime)

# filter(df, a > 0)
# hflights is at your disposal as a tbl, with clean carrier names
names(hflights)
# All flights that traveled 3000 miles or more
filter(hflights, Distance >= 3000)

# All flights flown by one of JetBlue, Southwest, or Delta
filter(hflights, UniqueCarrier %in% c("JetBlue", "Southwest", "Delta"))

# All flights where taxiing took longer than flying
filter(hflights, TaxiIn + TaxiOut > AirTime)

# hflights is at your service as a tbl!
names(hflights)
head(hflights)
# All flights that departed before 5am or arrived after 10pm
filter(hflights, DepTime < 500 | ArrTime > 2200)

# All flights that departed late but arrived ahead of schedule
filter(hflights, DepDelay > 0 & ArrDelay < 0)

# All flights that were cancelled after being delayed
filter(hflights, DepDelay > 0 & Cancelled == 1)

# hflights is already available in the workspace
names(hflights)
# Select the flights that had JFK as their destination: c1
c1 <- filter(hflights, Dest == "JFK")
c1
# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 <- mutate(c1, Date = paste(Year, Month, DayofMonth, sep = "-"))

# Print out a selection of columns of c2
select(c2, Date, DepTime, ArrTime, TailNum)

# dplyr and the hflights tbl are available
names(dtc)
# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
arrange(dtc, DepDelay)

# Arrange dtc so that cancellation reasons are grouped
arrange(dtc, CancellationCode)

# Arrange dtc according to carrier and departure delays
arrange(dtc, UniqueCarrier, DepDelay)

# Arrange according to carrier and decreasing departure delays
arrange(hflights, UniqueCarrier, desc(DepDelay))

# Arrange flights by total delay (normal order).
arrange(hflights, Total_Delay = DepDelay + ArrDelay)

# Print out a summary with variables min_dist and max_dist
summarize(hflights, min_dist = min(Distance), max_dist = max(Distance))

# Print out a summary with variable max_div
summarize(filter(hflights, Diverted == 1), max_div = max(Distance))

# Remove rows that have NA ArrDelay: temp1
temp1 <- filter(hflights, !is.na(ArrDelay))
temp1
# Generate summary about ArrDelay column of temp1
summarize(temp1, earliest = min(ArrDelay), average = mean(ArrDelay), latest = max(ArrDelay), sd = sd(ArrDelay))

# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 <- filter(hflights, !is.na(TaxiIn) & !is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarize()
summarize(temp2, max_taxi_diff = max(abs(TaxiIn - TaxiOut)))

# hflights is available with full names for the carriers
names(hflights)
# Generate summarizing statistics for hflights
summarize(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest))

# All American Airline flights
aa <- filter(hflights, UniqueCarrier == "American")

# Generate summarizing statistics for aa 
summarize(aa,
          n_flights = n(),
          n_canc = sum(Cancelled == 1),
          avg_delay = mean(ArrDelay, na.rm = TRUE))

# Write the 'piped' version of the English sentences. %>% = to "then"
# first(x) - The first element of vector x.
# last(x) - The last element of vector x.
# nth(x, n) - The nth element of vector x.
# n() - The number of rows in the data.frame or group of observations that summarize() describes.
# n_distinct(x) - The number of unique values in vector x.

hflights %>% 
  mutate(diff =  TaxiOut - TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarize(avg = mean(diff))

# Chain together mutate(), filter() and summarize()
hflights %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = ((Distance * 60) / RealTime)) %>%
  filter(!is.na(mph) & mph < 70) %>%
  summarize(n_less = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

# Finish the command with a filter() and summarize() call
hflights %>%
  mutate(
    RealTime = ActualElapsedTime + 100, 
    mph = 60 * Distance / RealTime) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarize(n_non = n(),
            n_dest = n_distinct(Dest),
            min_dist = min(Distance),
            max_dist = max(Distance))

# Count the number of overnight flights
hflights %>%
  filter(!is.na(DepTime) & !is.na(ArrTime) & DepTime > ArrTime) %>%
  summarize(num = n())

# Make an ordered per-carrier summary of hflights
hflights %>%
  group_by(UniqueCarrier) %>%
  summarize(
    p_canc = 100 * mean(Cancelled == 1),
    avg_delay = mean(ArrDelay, na.rm = TRUE)
  ) %>%
  arrange(avg_delay, p_canc)

# Ordered overview of average arrival delays per carrier
hflights %>%
  filter(!is.na(ArrDelay) & ArrDelay > 0) %>%
  group_by(UniqueCarrier) %>%
  summarize(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)

# How many airplanes only flew to one destination?
hflights %>%
  group_by(TailNum) %>%
  summarize(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarize(nplanes = n())

# Find the most visited destination for each carrier
hflights %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarize(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

# Use summarize to calculate n_carrier
hflights2 %>%
  summarize(n_carrier = n_distinct(UniqueCarrier))

# Set up a connection to the mysql database
my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

# Reference a table within that source: nycflights
nycflights <- tbl(my_db, "dplyr")

# glimpse at nycflights
glimpse(nycflights)

# Ordered, grouped summary of nycflights
nycflights %>%
  group_by(carrier) %>%
  summarize(n_flights = n(), avg_delay = mean(arr_delay)) %>%
  arrange(avg_delay)

