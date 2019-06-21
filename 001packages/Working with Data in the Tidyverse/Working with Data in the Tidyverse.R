## Chapter 1
# Load readr
library(readr)

# Create bakeoff from "bakeoff.csv"
bakeoff <- read_csv("bakeoff.csv")

# Print bakeoff
bakeoff

# Load readr
library(readr)

# Create bakeoff but skip first row
bakeoff <- read_csv("bakeoff.csv", skip = 1)

# Print bakeoff
bakeoff

# Load dplyr
library(dplyr)

# Filter rows where showstopper is UNKNOWN 
bakeoff %>% 
    filter(showstopper == "UNKNOWN")

# Edit to add list of missing values, with the argument na = c("", "NA", "UNKNOWN") to add unknown as NA type
bakeoff <- read_csv("bakeoff.csv", skip = 1,
                    na = c("", "NA", "UNKNOWN"))

# Filter rows where showstopper is NA 
bakeoff %>%
    filter(is.na(showstopper))

# When was the first US episode air?
bakeoff %>% 
	arrange(us_airdate) %>% 
	glimpse()

# Load skimr
library(skimr)

# Filter and skim This will produce summary statistics for each series.
bakeoff %>%
  filter(!is.na(us_season)) %>% 
  skim()

# Load skimr
library(skimr)

# Edit to filter, group by, and skim
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season)  %>% 
  skim()

# How many variables of each type do we have in the bakeoff data? 
bakeoff %>%
	skim() %>% 
	summary()

# View the distinct values for the result variable. Is "SB" one of them?
# View distinct results
bakeoff %>%
    distinct(result)

# How many star bakers are there?
# Count rows for each result
bakeoff %>% 
  count(result) 

# Adapt your code to count by a logical condition instead: if the result is equal to "SB".
# Count whether or not star baker
bakeoff %>% 
  count(result == "SB")

# Find format to parse uk_airdate 
parse_date("17 August 2010", format = "%d %B %Y")

# Add second count by series
bakeoff %>% 
  count(series, episode) %>%
  count(series)

# Count the number of rows by series and baker
bakers_by_series <- bakeoff %>% 
  count(series, baker)
  
# Print to view
bakers_by_series

# Which series had the most bakers? Which had the least?
# Count again by series
bakers_by_series %>% 
  count(series)

# What's the most common baker name?
# Count again by baker
bakers_by_series %>%
  count(baker, sort = TRUE)

# plot the number of bakers per episode along the x-axis.
ggplot(bakeoff, aes(x = episode)) + 
    geom_bar() + 
    facet_wrap(~series)

# Chapter 2
# Try to cast technical as a number
desserts <- read_csv("desserts.csv",
                      col_types = cols(
                        technical = col_number())
                     )

# which value could not be parsed (hint: actual)?
# View parsing problems
problems(desserts)

# Adapt your code to add the value that read_csv() had trouble parsing to the character vector of missing values.
# Edit code to fix the parsing error 
desserts <- read_csv("desserts.csv",
                      col_types = cols(
                        technical = col_number()),
                        na = c("", "NA", "N/A") 
                     )

# View parsing problems
problems(desserts)

# Find format to parse uk_airdate 
parse_date("17 August 2010", format = "%d %B %Y")

# Edit to cast uk_airdate
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       technical = col_number(),
                       uk_airdate = col_date(format = "%d %B %Y")
                     ))

#  when did the last episode air in the UK?
# Print by descending uk_airdate
desserts %>%
      arrange(desc(uk_airdate))

# Cast result a factor
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       technical = col_number(),
                       uk_airdate = col_date(format = "%d %B %Y"),
                       result = col_factor(levels = NULL)
                     ))
                    
# Glimpse to view
glimpse(desserts)

# How many bakes included filberts as the nut?
# Count rows grouping by nut variable
desserts %>%
    count(nut, sort = TRUE)

# Count rows grouping by nut variable
desserts %>% 
    count(nut, sort = TRUE)
    
# Recode filberts as hazelnuts
desserts_2 <- desserts %>% 
  mutate(nut = recode(nut, "filbert" = "hazelnut"))

# Count rows again 
desserts_2 %>% 
    count(nut, sort = TRUE)

# Count rows grouping by nut variable
desserts %>% 
    count(nut, sort = TRUE)
    
# Edit code to recode "no nut" as missing
desserts_2 <- desserts %>% 
  mutate(nut = recode(nut, "filbert" = "hazelnut", 
                           "no nut" = NA_character_))

# Count rows again 
desserts_2 %>% 
    count(nut, sort = TRUE)

# Create dummy variable: 1 if won, 0 if not
desserts <- desserts %>%
                mutate(tech_win = recode(technical, '1' = 1,
                                                    .default = 0))

# Create dummy variable: 1 if won, 0 if not
desserts <- desserts %>% 
  mutate(tech_win = recode(technical, `1` = 1,
                           .default = 0))

# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)

# Edit to recode tech_win as factor
desserts <- desserts %>% 
  mutate(tech_win = recode_factor(technical, `1` = 1,
                           .default = 0))

# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)

#
ratings %>% 
	select(series, episodes, e1_viewers, e10_viewers) %>%
	filter(series == 10) %>%
	mutate(growth = e10_viewers/e1_viewers)

# Recode channel as factor: bbc (1) or not (0)
ratings <- ratings %>% 
  mutate(bbc = recode_factor(channel, 
                             "Channel 4" = 0,
                             .default = 1))
                            
# Select to look at variables to plot next
ratings %>% 
  select(series, channel, bbc, viewer_growth)
  
# Make a filled bar chart
ggplot(ratings, aes(x = series, y = viewer_growth, fill = bbc)) +
  geom_col()

# Move channel to first column, keeping all other variables using everything().
ratings %>% 
  select(channel, everything())

# Adapt your code to drop only those variables that end with "day".
# Drop 7- and 28-day episode ratings
ratings %>% 
  select(-ends_with("day"))

# Move channel to the first column and keep all other variables except those that end with "day".
# Move channel to front and drop 7-/28-day episode ratings
ratings %>% 
  select(channel, everything(), -ends_with("day"))

# Glimpse to see variable names
glimpse(messy_ratings)

# Load janitor
library(janitor)

# Reformat to lower camelcase
ratings <- messy_ratings %>%
  clean_names(case = "lower_camel")
    
# Glimpse new tibble
glimpse(ratings)

# Glimpse to see variable names
glimpse(messy_ratings)

# Load janitor
library(janitor)

# Reformat to snake case
ratings <- messy_ratings %>%  
  clean_names("snake")

# Glimpse cleaned names
glimpse(ratings)

# Select 7-day viewer data by series
viewers_7day <- ratings %>%
                    select(series, ends_with("7day"))

# Glimpse
glimpse(viewers_7day)

# Adapt your code to rename each 7-day viewers variable to viewers_7day_<episode>
# Adapt code to also rename 7-day viewer data
viewers_7day <- ratings %>% 
    select(series, viewers_7day_ = ends_with("7day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to drop 28-day columns; keep 7-day in front
viewers_7day <- ratings %>% 
    select(viewers_7day_ = ends_with("7day"),
        everything(),
        -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to keep original order
viewers_7day <- ratings %>% 
    select(everything(),
    viewers_7day_ = ends_with("7day"), 
    -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)

## Chapter 3
# Plot of episode 1 viewers by series
ggplot(ratings, aes(x = series, y = e1)) +
    geom_col()

# Adapt code to plot episode 2 viewers by series
ggplot(ratings, aes(x = series, y = e2)) +
    geom_col()

# tiding the data
tidy_ratings <- ratings %>%
    # Gather and convert episode to factor
	gather(key = "episode", value = "viewers_7day", -series, 
           factor_key = TRUE, na.rm = TRUE) %>%
	# Sort in ascending order by series and episode
    arrange(series, episode) %>% 
	# Create new variable using row_number()
    mutate(episode_count = row_number())

# Plot viewers by episode and series
ggplot(tidy_ratings, aes(x = episode_count, 
                y = viewers_7day, 
                fill = series)) +
    geom_col()

# Tidying the data
week_ratings <- ratings2  %>% 
	# Select 7-day viewer ratings
    select(series, ends_with("7day")) %>% 
	# Gather 7-day viewers by episode
    gather(episode, viewers_7day, ends_with("7day"), na.rm = TRUE, factor_key = TRUE)
    
# Plot 7-day viewers by episode and series
ggplot(week_ratings, aes(x = episode, 
                         y = viewers_7day, 
                         group = series)) +
    geom_line() +
    facet_wrap(~series)

# Create week_ratings
week_ratings <- ratings2 %>% 
    select(series, ends_with("7day")) %>% 
    gather(episode, viewers_7day, ends_with("7day"), 
           na.rm = TRUE) %>% 
    # Edit to separate key column and drop extra
    separate(episode, into = "episode", extra = "drop") %>% 
    # Edit to parse episode number
    mutate(episode = parse_number(episode))
    
# Edit your code to color by series and add a theme
ggplot(week_ratings, aes(x = episode, y = viewers_7day, 
                         group = series, color = series)) +
    geom_line() +
    facet_wrap(~series) +
    guides(color = FALSE) +
    theme_minimal()

# Tidy the data
ratings3 <- ratings2  %>% 
# Unite and change the separator
unite(viewers_7day, viewers_millions, viewers_decimal, sep = "") %>%
# Adapt to cast viewers as a number
mutate(viewers_7day = parse_number(viewers_7day))

# Print to view
ratings3

# Create tidy data with 7- and 28-day viewers
tidy_ratings_all <- ratings2 %>% 
    gather(episode, viewers, ends_with("day"), na.rm = TRUE) %>% 
    separate(episode, into = c("episode", "days")) %>%  
    mutate(episode = parse_number(episode),
           days = parse_number(days)) 

tidy_ratings_all %>% 
	# Count viewers by series and days
    count(series, days, wt = viewers) %>%
	# Adapt to spread counted values
    spread(days, n, sep = "_")

# Fill in blanks to get premiere/finale data
tidy_ratings <- ratings %>%
    gather(episode, viewers, -series, na.rm = TRUE) %>%
    mutate(episode = parse_number(episode)) %>% 
    group_by(series) %>% 
    filter(episode == 1 | episode == max(episode)) %>% 
    ungroup()

# Recode first/last episodes
first_last <- tidy_ratings %>% 
  mutate(episode = recode(episode, `1` = "first", .default = "last")) 

# Switch the variables mapping x-axis and color
ggplot(first_last, aes(x = series, y = viewers, color = episode)) +
  geom_point() + # keep
  geom_line(aes(group = series)) + # keep
  coord_flip() # keep

# Calculate relative increase in viewers
bump_by_series <- first_last %>% 
  spread(episode, viewers) %>%   
  mutate(bump = (last - first) / first)
  
# Fill in to make bar chart of bumps by series
ggplot(bump_by_series, aes(x = series, y = bump)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) # converts to %

## Chapter 4
# Create skill variable with 3 levels
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    TRUE ~ "well_rounded"
  ))
  
# Filter zeroes to examine skill variable
bakers_skill %>% 
  filter(star_baker == 0 & technical_winner == 0) %>% 
  count(skill)

# Add pipe to drop skill = NA
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  )) %>% 
  drop_na(skill)
  
# Count bakers by skill
bakers_skill %>%
  count(skill)

# Using forcats package to treat factors
# Cast skill as a factor
bakers <- bakers %>% 
  mutate(skill = as.factor(skill))

# Examine levels
bakers %>%
  dplyr::pull(skill) %>%
  levels()

# Plot counts of bakers by skill, fill by winner
ggplot(bakers, aes(x = skill, fill = series_winner)) +
  geom_bar()

# Edit to reverse x-axis order
ggplot(bakers, aes(x = fct_rev(skill), fill = series_winner)) +
  geom_bar()

# Using Lubridate
# Cast last_date_appeared_us as a date
baker_dates_cast <- baker_dates %>%
  mutate(last_date_appeared_us = dmy(last_date_appeared_us))

# Add a line to extract labeled month
baker_dates_cast <- baker_dates %>% 
  mutate(last_date_appeared_us = dmy(last_date_appeared_us),
         last_month_us = month(last_date_appeared_us, label = TRUE))
         
# Make bar chart by last month
ggplot(baker_dates_cast, aes(x = last_month_us)) +
  geom_bar()

# Add a line to create whole months on air variable
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1),
         months_on_air = time_on_air %/% months(1))

# Using the stringr package
# Add another mutate to replace "THIRD PLACE" with "RUNNER UP"and count
bakers <- bakers %>% 
  # make it upper cases
  mutate(position_reached = str_to_upper(position_reached),
  # replace dashes with a space
         position_reached = str_replace(position_reached, "-", " "),
  # replace "THIRD PLACE" with "RUNNER UP"
         position_reached = str_replace(position_reached, "THIRD PLACE", "RUNNER UP"))

# Count rows
bakers %>%
  count(position_reached)

# Add a line to create new variable called student
bakers <- bakers %>% 
    mutate(occupation = str_to_lower(occupation), 
           student = str_detect(occupation, "student"))

# Find all students and examine occupations
bakers %>%
  filter(student == TRUE) %>%
  select(baker, occupation, student)