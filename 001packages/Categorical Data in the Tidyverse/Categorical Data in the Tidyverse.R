## Chapter 1
# Print out the dataset
multiple_choice_responses 

# Check if CurrentJobTitleSelect is a factor
is.factor(multiple_choice_responses$CurrentJobTitleSelect)

# Change all the character columns to factors
responses_as_factors <- multiple_choice_responses %>%
    mutate_if(is.character, as.factor)

number_of_levels <- responses_as_factors %>%
	# apply the function nlevels to each column
    summarise_all(nlevels) %>%
    # change the dataset from wide to long
    gather(variable, num_levels)

# Select the 3 rows with the highest number of levels 
number_of_levels %>%
    top_n(3, num_levels)
    
number_of_levels %>%
	# filter for where variable is CurrentJobTitleSelect
    filter(variable == "CurrentJobTitleSelect") %>%
	# pull num_levels
    pull(num_levels)

# Examining Levels
responses_as_factors %>%
    # pull CurrentJobTitleSelect
    pull(CurrentJobTitleSelect) %>%
    # get the values of the levels
    levels()

# Make a bar plot
ggplot(multiple_choice_responses, aes(x = EmployerIndustry)) + 
    geom_bar() + 
    # flip the coordinates
    coord_flip()

# Make a bar plot
ggplot(multiple_choice_responses) + 
    geom_bar(aes(x = fct_rev(fct_infreq(EmployerIndustry)))) + 
    # flip the coordinates
    coord_flip()

# Ordering one variable by another
multiple_choice_responses %>%
  # remove NAs
  filter(!is.na(EmployerIndustry) & !is.na(Age)) %>%
  # get mean_age by EmployerIndustry
  group_by(EmployerIndustry) %>%
  summarise(mean_age = mean(Age)) %>%
  # reorder EmployerIndustry by mean_age 
  mutate(EmployerIndustry = fct_reorder(EmployerIndustry, mean_age)) %>%
  # make a scatterplot of EmployerIndustry by mean_age
  ggplot(aes(x = EmployerIndustry, y = mean_age)) + 
    geom_point() + 
    coord_flip()

## Chapter 2
# Get the levels of WorkInternalVsExternalTools
levels(multiple_choice_responses$WorkInternalVsExternalTools)

# Reorder the levels from internal to external 
mc_responses_reordered <- multiple_choice_responses %>%
    mutate(WorkInternalVsExternalTools = fct_relevel(WorkInternalVsExternalTools,
                                            "Entirely internal", 
                                             "More internal than external",
                                             "Approximately half internal and half external",
                                             "More external than internal", 
                                             "Entirely external",
                                             "Do not know"))

# Make a bar plot of the responses
ggplot(mc_responses_reordered, aes(x = WorkInternalVsExternalTools)) + 
    geom_bar() + 
    coord_flip()

# Tricks of fct_relevel()
multiple_choice_responses %>%
    # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
    mutate(FormalEducation = fct_relevel(FormalEducation, "I did not complete any formal education past high school", "Some college/university study without earning a bachelor's degree")) %>%
    # Move "I prefer not to answer" to be the last level.
    mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
    # Move "Doctoral degree" to be the sixth level
    mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
    # Examine the new level order
    pull(FormalEducation) %>%
    levels()

# make a bar plot of the frequency of FormalEducation
ggplot(multiple_choice_responses, aes(x = FormalEducation)) + 
  geom_bar()

# Renaming a few levels
multiple_choice_responses %>%
    # rename the appropriate levels to "High School" and "Some college"
    mutate(FormalEducation = fct_recode(FormalEducation,
    "High school" = "I did not complete any formal education past high school", 
    "Some college" = "Some college/university study without earning a bachelor's degree")) %>%
    # make a bar plot of FormalEducation
    ggplot(aes(x = FormalEducation)) + 
    geom_bar()

# Manually collapsing levels
multiple_choice_responses %>%
    # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
    mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect, 
        "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"), 
        "Researcher" = "Scientist/Researcher", 
        "Data Analyst/Scientist/Engineer" = c("DBA/Database Engineer", "Data Scientist", 
                                              "Business Analyst", "Data Analyst", 
                                              "Data Miner", "Predictive Modeler"))) %>%
    # Turn every title that isn't now one of the grouped_titles into "Other"
    mutate(grouped_titles = fct_other(grouped_titles, 
                             keep = c("Computer Scientist", 
                                     "Researcher", 
                                     "Data Analyst/Scientist/Engineer"))) %>% 
    # Get a count of the grouped titles
    count(grouped_titles)

# Lumping variables by proportion
multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = 0.05)) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)

# Preserving the most common levels
multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n = 5, other_level = "other method")) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)

## Chapter 3
# Grouping and reshaping similar columns
learning_platform_usefulness <- multiple_choice_responses %>%
  # select columns with LearningPlatformUsefulness in title
  select(contains("LearningPlatformUsefulness")) %>%
  # change data from wide to long
  gather(learning_platform, usefulness) %>%
  # remove rows where usefulness is NA
  filter(!is.na(usefulness)) %>%
  # remove "LearningPlatformUsefulness" from each string in `learning_platform 
  mutate(learning_platform = str_remove(learning_platform, "LearningPlatformUsefulness"))

# Summarizing data
perc_useful_platform <- learning_platform_usefulness %>%
  # change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness) %>%
  # use add_count to create column with total number of answers for that learning_platform
  add_count(learning_platform, wt = n) %>%
  # create a new column, perc, that is the percentage of people giving that response for that learning_platform
  mutate(perc = n / nn)

# create a line graph for each question with usefulness on x-axis and percentage of responses on y
ggplot(perc_useful_platform, aes(x = usefulness, y = perc, group = learning_platform)) + 
  geom_line() + 
  facet_wrap(~ learning_platform)

# Creating an initial plot
usefulness_by_platform <- learning_platform_usefulness %>%
    # If usefulness is "Not Useful", make 0, else 1 
    mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1)) %>%
    # Group by learning platform 
    group_by(learning_platform) %>%
    # Summarize the mean usefulness for each platform
    summarize(avg_usefulness = mean(usefulness))

# Make a scatter plot of average usefulness by learning platform 
ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) + 
        geom_point()

# Editing plot text
ggplot(usefulness_by_platform, aes(x = learning_platform, y = avg_usefulness)) + 
    geom_point() + 
    # rotate x-axis text by 90 degrees
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    # rename y and x axis labels
    labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
    # change y axis scale to percentage
    scale_y_continuous(labels = scales::percent)

# Reordering graphs
usefulness_by_platform %>%
	# reorder learning_platform by avg_usefulness
	mutate(learning_platform = fct_reorder(learning_platform, avg_usefulness)) %>%
	# reverse the order of learning_platform
	mutate(learning_platform = fct_rev(learning_platform)) %>%
	ggplot(aes(x = learning_platform, y = avg_usefulness)) + 
	geom_point() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(x = "Learning Platform", y = "Percent finding at least somewhat useful") + 
    scale_y_continuous(labels = scales::percent)

# case_when() with single variable
# Check the min age
min(multiple_choice_responses$Age, na.rm = TRUE)
# Check the max age
max(multiple_choice_responses$Age, na.rm = TRUE)

# case_when() with single variable
multiple_choice_responses %>%
    # Filter for rows where Age is between 10 and 90
    filter(between(Age, 10, 90)) %>%
    # Create the generation variable based on age
    mutate(generation = case_when(
    between(Age, 10, 22) ~ "Gen Z", 
    between(Age, 23, 37) ~ "Gen Y", 
    between(Age, 38, 52) ~ "Gen X", 
    between(Age, 53, 71) ~ "Baby Boomer", 
    between(Age, 72, 90) ~ "Silent"
    )) %>%
    # Get a count of how many answers in each generation
    count(generation)

# case_when() from multiple columns
multiple_choice_responses %>%
    # Filter out people who selected Data Scientist as their Job Title
    filter(CurrentJobTitleSelect != "Data Scientist")  %>%
    # Create a new variable, job_identity
    mutate(job_identity = case_when(
        CurrentJobTitleSelect == "Data Analyst" & 
        DataScienceIdentitySelect == "Yes" ~ "DS analysts", 
        CurrentJobTitleSelect == "Data Analyst" & 
        DataScienceIdentitySelect %in% c("No", "Sort of (Explain more)") ~ "NDS analyst", 
        CurrentJobTitleSelect != "Data Analyst" & 
        DataScienceIdentitySelect == "Yes" ~ "DS non-analysts", 
        TRUE ~ "NDS non analysts")) %>%
    # Get the average job satisfaction by job_identity, removing NAs
    group_by(job_identity) %>%
    summarize(avg_js = mean(JobSatisfaction, na.rm = TRUE))

## Chapter 4
# Changing characters to factors
flying_etiquette %>%
    # Change characters to factors
    mutate_if(is.character, as.factor) %>%
    # Filter out those who have never flown on a plane
    filter(`How often do you travel by plane?` != "Never")

# Tidying data
gathered_data <- flying_etiquette %>%
    mutate_if(is.character, as.factor) %>%
    filter(`How often do you travel by plane?` != "Never") %>%
    # Select columns containing "rude"
    select(contains("rude")) %>%
    # Change format from wide to long
    gather(response_var, value)

# Cleaning up strings
gathered_data %>%
    # Remove everything before and including "rude to "
    mutate(response_var = str_remove(response_var, ".*rude to ")) %>%
    # Remove "on a plane"
    mutate(response_var = str_remove(response_var, "on a plane"))

# Dichotomizing variables
dichotimized_data <- gathered_data %>%
    mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
    mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
	# Remove rows that are NA in the value column
	filter(!is.na(value)) %>%
    # Dichotomize the value variable to make a new variable, rude
    mutate(rude = if_else(value %in% c('No, not rude at all', 'No, not at all rude'), 0, 1))

# Summarizing data
rude_behaviors <- gathered_data %>%
    mutate(response_var = str_replace(response_var, '.*rude to ', '')) %>%
    mutate(response_var = str_replace(response_var, 'on a plane', '')) %>%
	# Remove rows that are NA in the value column
	filter(!is.na(value)) %>%
    mutate(rude = if_else(value %in% c("No, not rude at all", "No, not at all rude"), 0, 1)) %>%
    # Create perc_rude, the percent considering each behavior rude
    group_by(response_var) %>%
    summarise(perc_rude = mean(rude))

rude_behaviors

# Creating an initial plot
initial_plot <- rude_behaviors %>%
	# reorder response_var by perc_rude
	mutate(response_var = fct_reorder(response_var, perc_rude)) %>%
	# make a bar plot of perc_rude by response_var
	ggplot(aes(x = response_var, y = perc_rude)) + 
    geom_col()


# View your plot
initial_plot

# Fixing labels
titled_plot <- initial_plot + 
    # Add the title, subtitle, and caption
    labs(title = "Hell Is Other People In A Pressurized Metal Tube",
         subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
         caption = "Source: SurveyMonkey Audience", 
         # Remove the x- and y-axis labels
         x = "",
         y = "") 

titled_plot

# Flipping things around
flipped_plot <- titled_plot + 
    # Flip the axes
    coord_flip() + 
    # Remove the x-axis ticks and labels
    theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())

# Finalizing the chart
flipped_plot + 
    # Add labels above the bar with the perc value
    geom_text(aes(label = percent(perc_rude), 
                  y = perc_rude + .03), 
              position = position_dodge(0.9),
              vjust = 1)
