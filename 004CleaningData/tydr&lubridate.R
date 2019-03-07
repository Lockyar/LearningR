# View the first 6 rows of data
head(weather)

# View the last 6 rows of data
tail(weather)

# View a condensed summary of the data
str(weather)

# Check the class of bmi
class(bmi)

# Check the dimensions of bmi
dim(bmi)

# View the column names of bmi
names(bmi)

# Check the structure of bmi
str(bmi)

# Load dplyr
library(dplyr)

# Check the structure of bmi, the dplyr way
glimpse(bmi)

# View a summary of bmi
summary(bmi)

# Print bmi to the console
bmi

# View the first 6 rows
head(bmi)

# View the first 15 rows
head(bmi, n = 15)

# View the last 6 rows
tail(bmi)

# View the last 10 rows
tail(bmi, n = 10)

# Histogram of BMIs from 2008
hist(bmi$Y2008)

# Scatter plot comparing BMIs from 1980 to those from 2008
plot(bmi$Y1980, bmi$Y2008)

# Apply gather() to bmi and save the result as bmi_long. 
# gather(wide_df, my_key, my_val, -col)
# gather() should be used when you have columns that are not variables and you want to collapse them into key-value pairs.
bmi_long <- gather(bmi, year, bmi_val, -Country)

# View the first 20 rows of the result
head(bmi_long, n = 20)

# The opposite of gather() is spread(), which takes key-values pairs and spreads them across multiple columns.
# spread(long_df, my_key, my_val)
# Apply spread() to bmi_long
bmi_wide <- spread(bmi_long, year, bmi_val)

# View the head of bmi_wide
head(bmi_wide)

# The separate() function allows you to separate one column into multiple columns.
# separate(treatments, year_mo, c("year", "month"))
# Apply separate() to bmi_cc
bmi_cc_clean <- separate(bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")

# Print the head of the result
head(bmi_cc_clean)

# The opposite of separate() is unite(), which takes multiple columns and pastes them together. 
# unite(treatments, year_mo, year, month)
# Apply unite() to bmi_cc_clean
bmi_cc <- unite(bmi_cc_clean, Country_ISO, Country, ISO, sep = "-")

# View the head of the result
head(bmi_cc)

# View the head of census
head(census)

# Gather the month columns
census2 <- gather(census, MONTH, AMOUNT, -YEAR)

# Arrange rows by YEAR using dplyr's arrange
census2_arr <- arrange(census2, YEAR)

# View first 20 rows of census2
head(census2_arr, n = 20)

# View first 50 rows of census_long
head(census_long, n = 50)

# Spread the type column
census_long2 <- spread(census_long, type, amount)

# View first 20 rows of census_long2
head(census_long2, n = 20)

# View the head of census_long3
head(census_long3)

# Separate the yr_month column into two
census_long4 <- separate(census_long3, yr_month, c("year", "month"), sep = "_")

# View the first 6 rows of the result
head(census_long4)

# Make this evaluate to "character"
class(as.character(TRUE))
# Make this evaluate to "numeric"
class(as.numeric("8484.00"))
# Make this evaluate to "integer"
class(as.integer(99))
# Make this evaluate to "factor"
class(as.factor("factor"))
# Make this evaluate to "logical"
class(as.logical("FALSE"))

# Preview students with str()
str(students)

# Coerce Grades to character
students$Grades <- as.character(students$Grades) 

# Coerce Medu to factor
students$Medu <- as.factor(students$Medu)

# Coerce Fedu to factor
students$Fedu <- as.factor(students$Fedu)
    
# Look at students once more with str()
str(students)

# Preview students2 with str()
str(students2)

# As you saw in the video, these functions combine the letters y, m, d, h, m, s, which stand for year, month, day, hour, minute, and second, respectively.
# Load the lubridate package
library(lubridate)

# Parse as date
dmy("17 Sep 2015")

# Parse as date and time (with no seconds!)
mdy_hm("July 15, 2012 12:56")

# Coerce dob to a date (with no time)
students2$dob <- ymd(students2$dob)

# Coerce nurse_visit to a date and time
students2$nurse_visit <- ymd_hms(students2$nurse_visit)
    
# Look at students2 once more with str()
str(students2)

# Load the stringr package
library(stringr)

# One common issue that comes up when cleaning data is the need to remove leading and/or trailing white space. The str_trim()
# Trim all leading and trailing whitespace
str_trim(c("   Filip ", "Nick  ", " Jonathan"))
# A similar issue is when you need to pad strings to make them a certain number of characters wide.
# Pad these strings with leading zeros
str_pad(c("23485W", "8823453Q", "994Z"), width = 9, side = "left", pad = "0")

# Print state abbreviations
states
# Making strings uppercase or lowercase is very straightforward in (base) R thanks to toupper() and tolower().
# Make states all uppercase and save result to states_upper
states_upper <- toupper(states)
states_upper
# Make states_upper all lowercase again
tolower(states_upper)

# Copy of students2: students3
students3 <- students2

# Look at the head of students3
head(students3)

# In the case of str_detect(), this is the pattern we are searching for. 
# Detect all dates of birth (dob) in 1997
str_detect(students3$dob, "1997")
# In the case of str_replace(), this is the pattern we want to replace. Finally, str_replace() has a third argument, which is the string to replace with.
# In the sex column, replace "F" with "Female" ...
students3$sex <- str_replace(students3$sex, "F", "Female")

# ... and "M" with "Male"
students3$sex <- str_replace(students3$sex, "M", "Male")

# View the head of students3
head(students3)

# Call is.na() on the full social_df to spot all NAs
is.na(social_df)

# Use the any() function to ask whether there are any NAs in the data
any(is.na(social_df))

# View a summary() of the dataset
summary(social_df)

# Call table() on the status column
table(social_df$status)

# Replace all empty strings in status with NA
social_df$status[social_df$status == ""] <- NA

# Print social_df to the console
social_df

# Use complete.cases() to see which rows have no missing values
complete.cases(social_df)

# Use na.omit() to remove all rows with any missing values
na.omit(social_df)

# Look at a summary() of students3
summary(students3)

# View a histogram of the age variable
hist(students3$age)

# View a histogram of the absences variable
hist(students3$absences)

# View a histogram of absences, but force zeros to be bucketed to the right of zero
hist(students3$absences, right = FALSE)

# Load the tidyr package
library(tidyr)

# Gather the columns
weather2 <- gather(weather, day, value, X1:X31, na.rm = TRUE)

# View the head
head(weather2)

# First remove column of row names
without_x <- weather2[, -1]
head(without_x)
# Spread the data
weather3 <- spread(without_x, measure, value)

# View the head
head(weather3)

# Load the stringr and lubridate packages
library(stringr)
library(lubridate)
head(weather3)
# Remove X's from day column
weather3$day <- str_replace(weather3$day, "X", "")

# Unite the year, month, and day columns
weather4 <- unite(weather3, date, year, month, day, sep = "-")

# Convert date column to proper date format using lubridates's ymd()
weather4$date <- ymd(weather4$date)

# Rearrange columns using dplyr's select()
weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)

# View the head of weather5
head(weather5)

# View the structure of weather5
str(weather5)

# Examine the first 20 rows of weather5. Are most of the characters numeric?
head(weather5, n = 20)

# See what happens if we try to convert PrecipitationIn to numeric
as.numeric(weather5$PrecipitationIn)

# Replace "T" with "0" (T = trace)
weather5$PrecipitationIn <- str_replace(weather5$PrecipitationIn, "T", "0")

# Convert characters to numerics
weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))

# Look at result
str(weather6)

# Count missing values
sum(is.na(weather6))

# Find missing values
summary(weather6)

# Find indices of NAs in Max.Gust.SpeedMPH
ind <- which(is.na(weather6$Max.Gust.SpeedMPH))

# Look at the full rows for records missing Max.Gust.SpeedMPH
weather6[ind, ]

# Review distributions for all variables
summary(weather6)

# Find row with Max.Humidity of 1000
ind <- which(weather6$Max.Humidity == 1000)

# Look at the data for that day
weather6[ind, ]

# Change 1000 to 100
weather6$Max.Humidity[ind] <- 100

# Look at summary of Mean.VisibilityMiles
summary(weather6$Mean.VisibilityMiles)

# Get index of row with -1 value
ind <- which(weather6$Mean.VisibilityMiles == -1)

# Look at full row
weather6[ind, ]

# Set Mean.VisibilityMiles to the appropriate value
weather6$Mean.VisibilityMiles[ind] <- 10

# Review summary of full data once more
summary(weather6)

# Look at histogram for MeanDew.PointF
hist(weather6$MeanDew.PointF)

# Look at histogram for Min.TemperatureF
hist(weather6$Min.TemperatureF)

# Compare to histogram for Mean.TemperatureF
hist(weather6$Mean.TemperatureF)

# Clean up column names
names(weather6) <- new_colnames

# Replace empty cells in events column
weather6$events[weather6$events == ""] <- "None"
    
# Print the first 6 rows of weather6
head(weather6)
