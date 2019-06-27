## Chapter 1
# Load the ToothGrowth dataset
data("ToothGrowth")

# Perform a two-sided t-test
t.test(x = ToothGrowth$len, alternative = "two.sided", mu = 18)

# Perform a t-test
ToothGrowth_ttest <- t.test(len ~ supp, data = ToothGrowth)

# Load broom
library(broom)

# Tidy ToothGrowth_ttest
tidy(ToothGrowth_ttest)

# Load dplyr
library(dplyr)

# Count number of observations for each combination of supp and dose
ToothGrowth %>% 
    count(supp, dose)

# Create a boxplot with geom_boxplot()
ggplot(ToothGrowth, aes(x = dose, y = len)) + 
    geom_boxplot()

# Create ToothGrowth_aov
ToothGrowth_aov <- aov(len ~ dose + supp, data = ToothGrowth)

# Examine ToothGrowth_aov with summary()
summary(ToothGrowth_aov)

# Less than
t.test(x = ToothGrowth$len,
       alternative = "less",
       mu = 18)

# Greater than
t.test(x = ToothGrowth$len,
      alternative = "greater",
      mu = 18)

# Load the pwr package
library(pwr)

# Calculate power using an effect size of 0.35, a sample size of 100 in each group, and a significance level of 0.10.
# Calculate power
pwr.t.test(n = 100, 
           d = 0.35,
           sig.level = 0.10,
           type = "two.sample", 
           alternative = "two.sided",
           power = NULL)

# Calculate the sample size needed with an effect size of 0.25, a significance level of 0.05, and a power of 0.8.
# Calculate sample size
pwr.t.test(n = NULL, 
           d = 0.25, 
           sig.level = 0.05, 
           type = "one.sample", alternative = "greater", 
           power = 0.8)

## Chapter 2
# Examine the variables with glimpse()
glimpse(lendingclub)

# Find median loan_amnt, mean int_rate, and mean annual_inc with summarise()
lendingclub %>%
	summarise(median(loan_amnt), 
				mean(int_rate),
				mean(annual_inc))

# Use ggplot2 to build a bar chart of purpose
ggplot(data=lendingclub, aes(x = purpose)) + 
	geom_bar() +
	coord_flip()

# Use recode() to create the new purpose_recode variable
lendingclub$purpose_recode <- lendingclub$purpose %>% recode( 
        "credit_card" = "debt_related", 
  		"debt_consolidation" = "debt_related",
  		"medical" = "debt_related",
        "car" = "big_purchase", 
  		"major_purchase" = "big_purchase", 
  		"vacation" = "big_purchase",
        "moving" = "life_change", 
  		"small_business" = "life_change", 
  		"wedding" = "life_change",
        "house" = "home_related", 
  		"home_improvement" = "home_related")

# Build a linear regression model, purpose_recode_model
purpose_recode_model <- lm(funded_amnt ~ purpose_recode, data = lendingclub)

# Examine results of purpose_recode_model
summary(purpose_recode_model)

# Get anova results and save as purpose_recode_anova
purpose_recode_anova <- anova(purpose_recode_model)

# Print purpose_recode_anova
purpose_recode_anova

# Examine class of purpose_recode_anova
class(purpose_recode_anova)

# Use aov() to build purpose_aov
purpose_aov <- aov(funded_amnt ~ purpose_recode, data = lendingclub)

# at least one mean was different. But which one? We should use Tukey's HSD test, which stands for Honest Significant Difference.
# Conduct Tukey's HSD test to create tukey_output
tukey_output <- TukeyHSD(purpose_aov, conf.level = 0.95)

# Tidy tukey_output to make sense of the results
tidy(tukey_output)

# Use aov() to build purpose_emp_aov
purpose_emp_aov <- aov(funded_amnt ~ purpose_recode + emp_length, data = lendingclub)

# Print purpose_emp_aov to the console
purpose_emp_aov

# Call summary() to see the p-values
summary(purpose_emp_aov)

# Examine the summary of int_rate
summary(lendingclub$int_rate)

# Examine int_rate by grade
lendingclub %>% 
	group_by(grade) %>% 
	summarise(mean = mean(int_rate), var = var(int_rate), median = median(int_rate))

# Make a boxplot of int_rate by grade
ggplot(lendingclub, aes(x = grade, y = int_rate)) + 
	geom_boxplot()

# Use aov() to create grade_aov plus call summary() to print results
grade_aov <- aov(int_rate ~ grade, data = lendingclub)
summary(grade_aov)

# For a 2x2 grid of plots:
par(mfrow=c(2, 2))

# Plot grade_aov
plot(grade_aov)

# Bartlett's test for homogeneity of variance
bartlett.test(int_rate ~ grade, data = lendingclub)

# Conduct the Kruskal-Wallis rank sum test
kruskal.test(int_rate ~ grade,
             data = lendingclub)

# Load the pwr package
library(pwr)

# Use the correct function from pwr to find the sample size
pwr.t.test(n = NULL, 
    alternative = "two.sided", 
    d = 0.2, 
    sig.level = 0.05, 
    power = 0.8)

# Plot the A/B test results
ggplot(lendingclub_ab, aes(x = Group, y = loan_amnt)) + 
	geom_boxplot()

# Conduct a two-sided t-test
t.test(loan_amnt ~ Group, data = lendingclub_ab, alternative = "two.sided")

# Build lendingclub_multi
lendingclub_multi <- lm(loan_amnt ~ Group + grade + verification_status, data = lendingclub_ab)

# Examine lendingclub_multi results
tidy(lendingclub_multi)

## Chapter 3
# Load haven
library(haven)

# Import the three datasets using read_xpt()
nhanes_demo <- read_xpt(DEMO_file)
nhanes_medical <- read_xpt(MCQ_file)
nhanes_bodymeasures <- read_xpt(BMX_file)

# Merge the 3 datasets you just created to create nhanes_combined
nhanes_combined <- list(nhanes_demo, nhanes_medical, nhanes_bodymeasures) %>%
  Reduce(function(df1, df2) inner_join(df1, df2, by = "SEQN"), .)

# Fill in the dplyr code
nhanes_combined %>% group_by(mcq365d) %>% summarise(mean = mean(bmxwt, na.rm = TRUE))

# Fill in the ggplot2 code
nhanes_combined %>% filter(ridageyr > 16) %>% 
  ggplot(aes(as.factor(mcq365d), bmxwt)) +
  geom_boxplot()

# Filter to keep only those 16+
nhanes_filter <- nhanes_combined %>% filter(ridageyr > 16)

# Load simputation & impute bmxwt by riagendr
library(simputation)
nhanes_final <- impute_median(nhanes_filter, bmxwt ~ riagendr)

# Recode mcq365d with recode() & examine with count()
nhanes_final$mcq365d <- recode(nhanes_final$mcq365d, 
                               `1` = 1,
                               `2` = 2,
                               `9` = 2)
nhanes_final %>% count(mcq365d)

# Use sample_n() to create nhanes_srs
nhanes_srs <- nhanes_final %>% sample_n(2500)

# Create nhanes_stratified with group_by() and sample_n()
nhanes_stratified <- nhanes_final %>%
group_by(riagendr) %>% sample_n(2000)
count(nhanes_stratified)

# Load sampling package and create nhanes_cluster with cluster()
library(sampling)
nhanes_cluster <- cluster(nhanes_final, c("indhhin2"), 6, method = "srswor")

# Create designs using ls()
designs <- ls("package:agricolae", pattern = "design")
print(designs)

# Use str() to view design.rcbd's criteria
str(design.rcbd)

# Build treats and rep
treats <- LETTERS[1:5]
blocks <- 4

# Build my_design_rcbd and view the sketch
my_design_rcbd <- design.rcbd(treats, r = blocks, seed = 42)
my_design_rcbd$sketch

# Use aov() to create nhanes_rcbd
nhanes_rcbd <- aov(bmxwt ~ mcq365d + riagendr, data = nhanes_final)

# Check the results of nhanes_rcbd with summary()
summary(nhanes_rcbd)

# Print the difference in weights by mcq365d and riagendr
nhanes_final %>% 
  group_by(mcq365d, riagendr) %>% 
  summarise(mean_wt = mean(bmxwt))

# Set up the 2x2 plotting grid and plot nhanes_rcbd
par(mfrow = c(2, 2))
plot(nhanes_rcbd)

# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(mcq365d, riagendr, bmxwt))

# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(riagendr, mcq365d, bmxwt))

#create my_design_bibd_1
my_design_bibd_1 <- design.bib(LETTERS[1:3], k = 4, seed = 42)

#create my_design_bibd_2
my_design_bibd_2 <- design.bib(LETTERS[1:8], k = 3, seed = 42)

# Create my_design_bibd_3
my_design_bibd_3 <- design.bib(LETTERS[1:4], k = 4, seed = 42)
my_design_bibd_3$sketch

# Calculate lambda
lambda(t = 4, k = 3, r = 3)

# Build the data.frame
creatinine <- c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22)
food <- as.factor(c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"))
color <- as.factor(rep(c("Black", "White", "Orange", "Spotted"), each = 3))
cat_experiment <- as.data.frame(cbind(creatinine, food, color))

# Create cat_model and examine with summary()
cat_model <- aov(creatinine ~ food + color, data = cat_experiment)
summary(cat_model)

# Calculate lambda
lambda(t = 3, k = 2, r = 2)

# Create weightlift_model & examine results
weightlift_model <- aov(bmxarmc ~ weightlift_treat + ridreth1, data = nhanes_final)
summary(weightlift_model)

## Chapter 4
# Mean, var, and median of Math score
nyc_scores %>%
    group_by(Borough) %>% 
    summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
              var = var(Average_Score_SAT_Math, na.rm = TRUE),
              median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
    group_by(Teacher_Education_Level) %>% 
    summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
              var = var(Average_Score_SAT_Math, na.rm = TRUE),
              median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by both
nyc_scores %>%
    group_by(Borough, Teacher_Education_Level) %>% 
    summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
              var = var(Average_Score_SAT_Math, na.rm = TRUE),
              median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Load naniar
library(naniar)

# Examine missingness with miss_var_summary()
miss_var_summary(nyc_scores)

# Examine missingness with md.pattern()
md.pattern(nyc_scores)

# Impute the Math score by Borough
nyc_scores_2 <- impute_median(nyc_scores, Average_Score_SAT_Math ~ Borough)

# Examine missingness with md.pattern()
md.pattern(nyc_scores)

# Impute the Math score by Borough
nyc_scores_2 <- impute_median(nyc_scores, Average_Score_SAT_Math ~ Borough)

# Convert Math score to numeric
nyc_scores_2$Average_Score_SAT_Math <- as.numeric(nyc_scores_2$Average_Score_SAT_Math)

# Examine scores by Borough in both datasets, before and after imputation
nyc_scores %>% 
  group_by(Borough) %>% 
  summarise(median = median(Average_Score_SAT_Math, na.rm = TRUE), 
            mean = mean(Average_Score_SAT_Math, na.rm = TRUE))
nyc_scores_2 %>% 
  group_by(Borough) %>% 
  summarise(median = median(Average_Score_SAT_Math, na.rm = TRUE),
            mean = mean(Average_Score_SAT_Math, na.rm = TRUE))

# Load agricolae
library(agricolae)

# Design a LS with 5 treatments A:E then look at the sketch
my_design_lsd <- design.lsd(LETTERS[1:5], seed = 42)
my_design_lsd

# Build nyc_scores_ls_lm
nyc_scores_ls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level, data=nyc_scores_ls)

# Tidy the results with broom
tidy(nyc_scores_ls_lm)

# Examine the results with anova
anova(nyc_scores_ls_lm)

# Create a boxplot of Math scores by Borough, with a title and x/y axis labels
ggplot(nyc_scores, aes(x = Borough, y = Average_Score_SAT_Math)) +
  geom_boxplot() + 
  ggtitle("Average SAT Math Scores by Borough, NYC") +
  xlab("Borough (NYC)") +
  ylab("Average SAT Math Scores (2014-15)")

# Create trt1 and trt2
trt1 <- LETTERS[1:5]
trt2 <- seq(1:5)

# Create my_graeco_design
my_graeco_design <- design.graeco(trt1, trt2, serie = 0, seed = 42)

# Examine the parameters and sketch
my_graeco_design$parameters
my_graeco_design$sketch

# Build nyc_scores_gls_lm
nyc_scores_gls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level + Homework_Type,
                        data=nyc_scores_gls)

# Tidy the results with broom
tidy(nyc_scores_gls_lm)

# Examine the results with anova
anova(nyc_scores_gls_lm)

# Load ggplot2
library(ggplot2)

# Build the boxplot for the tutoring program vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Tutoring_Program, y = Average_Score_SAT_Math)) + 
    geom_boxplot()

# Build the boxplot for the percent black vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Black_HL, y = Average_Score_SAT_Math)) + 
    geom_boxplot()

# Build the boxplot for percent tested vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Tested_HL, y = Average_Score_SAT_Math)) + 
    geom_boxplot()

# Create nyc_scores_factorial and examine the results
nyc_scores_factorial <- aov(Average_Score_SAT_Math ~ Percent_Tested_HL * Percent_Black_HL * Tutoring_Program, data = nyc_scores)
tidy(nyc_scores_factorial)

# Use shapiro.test() to test the outcome
shapiro.test(nyc_scores$Average_Score_SAT_Math)

# Plot nyc_scores_factorial to examine residuals
par(mfrow = c(2,2))
plot(nyc_scores_factorial)
