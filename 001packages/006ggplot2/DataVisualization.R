# Load the ggplot2 package
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

# This time the x-axis does not contain variables like 5 or 7, only the values that are present in the dataset.
# Using factor()
# Load the ggplot2 package
library(ggplot2)

# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_point()

# A scatter plot has been made for you
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# change the color argument in aes() (which stands for aesthetics). The color should be dependent on the displacement of the car engine, found in disp.
# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()

# change the size argument in aes() (which stands for aesthetics). The size should be dependent on the displacement of the car engine, found in disp.
# Replace ___ with the correct column
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

# Here you'll use two common geom layer functions: geom_point() and geom_smooth(). We already saw in the earlier exercises how these are added using the + operator.
# Explore the diamonds data frame with str()
str(diamonds)

# Add geom_point() with +
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

# Add geom_point() and geom_smooth() with +
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth()

# 1 - The plot you created in the previous exercise
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

# 2 - Copy the above command but show only the smooth line
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth()

# 3 - Copy the above command and assign the correct value to col in aes()
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_smooth()

# 4 - Keep the color settings from previous command. Plot only the points with argument alpha.
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.4)

# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot + geom_point()

# Add the same geom layer, but with aes() inside. Note that you can also call aes() within the geom_point() function.
dia_plot + geom_point(aes(color = clarity))

# 1 - The dia_plot object has been created for you
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# 2 - Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(alpha = 0.2)

# 3 - Plot dia_plot with additional geom_smooth() with se set to FALSE
# You don't want any error shading, which can be achieved by setting the se = FALSE in geom_smooth().
dia_plot + geom_smooth(se = FALSE)

# 4 - Copy the command from above and add aes() with the correct mapping to geom_smooth()
dia_plot + geom_smooth(aes(col = clarity), se = FALSE)

# chapter 2
# Plot the correct variables of mtcars
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Change cyl inside mtcars to a factor
mtcars$fcyl <- as.factor(mtcars$cyl)

# Make the same plot as in the first instruction
plot(mtcars$wt, mtcars$mpg, col = mtcars$fcyl)

# Use lm() to calculate a linear model and save it as carModel
carModel <- lm(mpg ~ wt, data = mtcars)

# Basic plot
mtcars$cyl <- as.factor(mtcars$cyl)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Call abline() with carModel as first argument and set lty to 2
abline(carModel, lty = 2)

# Plot each subset efficiently with lapply
# You don't have to edit this code
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
  })

# This code will draw the legend of the plot
# You don't have to edit this code
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

# In this exercise you'll recreate the base package plot in ggplot2
# Convert cyl to factor (don't need to change)
mtcars$cyl <- as.factor(mtcars$cyl)

# Example from base R (don't need to change)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
abline(lm(mpg ~ wt, data = mtcars), lty = 2)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
  })
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

# Plot 1: add geom_point() to this command to create a scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()  # Fill in using instructions Plot 1

# Plot 2: include the lines of the linear models, per cyl
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Copy from Plot 1
  geom_smooth(method = lm, se = FALSE) # Fill in using instructions Plot 2

# Plot 3: include a lm for the entire dataset in its whole
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Copy from Plot 2
  geom_smooth(method = lm, se = FALSE) + # Copy from Plot 2
  geom_smooth(aes(group = 1), method = lm, linetype = 2, se = FALSE)   # Fill in using instructions Plot 3

# Consider the structure of iris, iris.wide and iris.tidy (in that order)
str(iris)
str(iris.wide)
str(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)

# Load the tidyr package
library(tidyr)

# The gather() function moves information from the columns to the rows. It takes multiple columns and gathers them into a single column by adding rows.
# The separate() function splits one column into two or more columns according to a pattern you define.
# Fill in the ___ to produce to the correct iris.tidy dataset
iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\.")

# The 3 data frames (iris, iris.wide and iris.tidy) are available in your environment
# Execute head() on iris, iris.wide and iris.tidy (in that order)
head(iris)
head(iris.wide)
head(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.wide, aes(x = Length, y = Width, color = Part)) +
  geom_jitter() +
  facet_grid(. ~ Species)

# Load the tidyr package
library(tidyr)

# Add column with unique ids (don't need to change)
iris$Flower <- 1:nrow(iris)

# Fill in the ___ to produce to the correct iris.wide dataset
iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)

# All about aesthetics
# These are the aesthetics you can consider within aes() in this chapter: x, y, color, fill, size, alpha, labels and shape.
# 1 - Map mpg to x and cyl to y
ggplot(mtcars, aes(mpg, cyl)) +
  geom_point()
  
# 2 - Reverse: Map cyl to x and mpg to y
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# 3 - Map wt to x, mpg to y and cyl to col
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point()

# 4 - Change shape and size of the points in the above plot
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

# am and cyl are factors, wt is numeric
class(mtcars$am)
class(mtcars$cyl)
class(mtcars$wt)

# From the previous exercise
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

# 1 - Map cyl to fill
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 1, size = 4)

# 2 - Change shape and alpha of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 21, alpha = 0.6, size = 4)

# 3 - Map am to col in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl, col = am)) +
  geom_point(shape = 21, alpha = 0.6, size = 4)

# Map cyl to size
ggplot(mtcars, aes(wt, mpg, size = cyl)) +
  geom_point()

# Map cyl to alpha
ggplot(mtcars, aes(wt, mpg, alpha = cyl)) +
  geom_point()

# Map cyl to shape 
ggplot(mtcars, aes(wt, mpg, shape = cyl)) +
  geom_point()

# Map cyl to label
ggplot(mtcars, aes(wt, mpg, label = cyl)) +
  geom_text()

# All about attributes
# Notice that if an aesthetic and an attribute are set with the same argument, the attribute takes precedence.
# Define a hexadecimal color
my_color <- "#4ABEFF"

# Draw a scatter plot with color *aesthetic*
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point()

# Same, but set color *attribute* in geom layer 
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(col = my_color)

# Set the fill aesthetic; color, size and shape attributes
ggplot(mtcars, aes(wt, mpg, fill = cyl)) +
  geom_point(size = 10, shape = 23, col = my_color)

# Expand to draw points with alpha 0.5
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(alpha = 0.5)
  
# Expand to draw points with shape 24 and color yellow
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(shape = 24, col = "yellow")
  
# Expand to draw text with label rownames(mtcars) and color red
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_text(label = rownames(mtcars), col = "red")

# Map mpg onto x, qsec onto y and factor(cyl) onto col
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl))) +
  geom_point()

# Add mapping: factor(am) onto shape
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl), shape = factor(am))) +
  geom_point()

# Add mapping: (hp/wt) onto size
ggplot(mtcars, aes(mpg, qsec, col = factor(cyl), shape = factor(am), size = (hp/wt))) +
  geom_point()

# label and shape are only applicable to categorical data

# The base layer, cyl.am, is available for you
cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

# Add geom (position = "stack" by default) 
cyl.am + 
  geom_bar()

# Fill - show proportion
cyl.am + 
  geom_bar(position = "fill")  

# Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge") 

# Clean up the axes with scale_ functions
val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab) 

# Setting a dummy aesthetic
# In the base package you can make univariate plots with stripchart()
# 1 - Create jittered plot of mtcars, mpg onto x, 0 onto y
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter()

# 2 - Add function to change y axis limits
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter() +
  scale_y_continuous(limits = c(-2, 2))

# Overplotting 1 - Point shape and transparency
# Basic scatter plot: wt on x-axis and mpg on y-axis; map cyl to col
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(size = 4)

# Hollow circles - an improvement
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(size = 4, shape = 1)

# Add transparency - very nice
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(size = 4, alpha = 0.6)

# Overplotting 2 - alpha with large datasets
# Scatter plot: carat (x), price (y), clarity (color)
ggplot(diamonds, aes(carat, price, col = clarity)) +
  geom_point()

# Adjust for overplotting
ggplot(diamonds, aes(carat, price, col = clarity)) +
  geom_point(alpha = 0.5)

# Scatter plot: clarity (x), carat (y), price (color)
ggplot(diamonds, aes(clarity, carat, col = price)) +
  geom_point(alpha = 0.5)

# Dot plot with jittering
ggplot(diamonds, aes(clarity, carat, col = price)) +
  geom_point(alpha = 0.5, position = "jitter")

# Scatter plots and jittering (1)
# Shown in the viewer:
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_point()

# Good job! Notice that jitter can be 1) an argument in geom_point(position = 'jitter'), 2) a geom itself, geom_jitter(), or 3) a position function, position_jitter(0.1)
# Solutions:
# 1 - With geom_jitter()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter()

# 2 - Set width in geom_jitter()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter(width = 0.1)

# 3 - Set position = position_jitter() in geom_point() ()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_point(position = position_jitter(0.1))

# Examine the structure of Vocab
str(Vocab)

# Basic scatter plot of vocabulary (y) against education (x). Use geom_point()
ggplot(Vocab, aes(education, vocabulary)) +
  geom_point()

# Use geom_jitter() instead of geom_point()
ggplot(Vocab, aes(education, vocabulary)) +
  geom_jitter()

# Using the above plotting command, set alpha to a very low 0.2
ggplot(Vocab, aes(education, vocabulary)) +
  geom_jitter(alpha = 0.2)

# Using the above plotting command, set the shape to 1
ggplot(Vocab, aes(education, vocabulary)) +
  geom_jitter(alpha = 0.2, shape = 1)

# Histograms
# 1 - Make a univariate histogram
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

# 2 - Plot 1, plus set binwidth to 1 in the geom layer
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 1)

# 3 - Plot 2, plus MAP ..density.. to the y aesthetic (i.e. in a second aes() function)
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1)

# 4 - plot 3, plus SET the fill attribute to "#377EB8"
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#377EB8")

# Position
# Draw a bar plot of cyl, filled according to am
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar()

# Change the position argument to stack
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "stack")

# Change the position argument to fill
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill")

# Change the position argument to dodge
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

# 1 - The last plot form the previous exercise
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

# 2 - Define posn_d with position_dodge()
posn_d <- position_dodge(width = 0.2)

# 3 - Change the position argument to posn_d
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d)

# 4 - Use posn_d as position and adjust alpha to 0.6
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d, alpha = 0.6)

# Overlapping histograms
# A basic histogram, add coloring defined by cyl
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1)

# Change position to identity
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "identity")


# Change geom to freqpoly (position is identity by default)
ggplot(mtcars, aes(mpg, col = cyl)) +
  geom_freqpoly(binwidth = 1, position = "identity")

# Bar plots with color ramp, part 1
# Example of how to use a brewed color palette
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")

# Use str() on Vocab to check out the structure
str(Vocab)

# Plot education on x and vocabulary on fill
# Use the default brewed color palette
ggplot(Vocab, aes(education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()

# Final plot of last exercise
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()

# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues") # from the RColorBrewer package

# 1 - Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

# 2 - Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11))

# Overlapping histograms  
# 1 - Basic histogram plot command
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 1)

# 2 - Plot 1, Expand aesthetics: am onto fill
ggplot(mtcars, aes(mpg, fill = am)) +
  geom_histogram(binwidth = 1)

# 3 - Plot 2, change position = "dodge"
ggplot(mtcars, aes(mpg, fill = am)) +
  geom_histogram(binwidth = 1, position = "dodge")

# 4 - Plot 3, change position = "fill"
ggplot(mtcars, aes(mpg, fill = am)) +
  geom_histogram(binwidth = 1, position = "fill")

# 5 - Plot 4, plus change position = "identity" and alpha = 0.4
ggplot(mtcars, aes(mpg, fill = am)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)

# 6 - Plot 5, plus change mapping: cyl onto fill
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)

# Line plots
# Print out head of economics
head(economics)

# Plot unemploy as a function of date using a line plot
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()

# Adjust plot to represent the fraction of total population that is unemployed
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

# Periods of recession
# Basic line plot
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

# Expand the following command with geom_rect() to draw the recess periods
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
         aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
         inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()

# Multiple time series
# Check the structure as a starting point
str(fish.species)

# Use gather to go from fish.species to fish.tidy
fish.tidy <- gather(fish.species,  Species, Capture, -Year)

# Recreate the plot shown on the right
ggplot(fish.tidy, aes(x = Year, y = Capture, col = Species)) +
  geom_line()

# Using qplot
# The old way (shown)
plot(mpg ~ wt, data = mtcars) # formula notation
with(mtcars, plot(wt, mpg)) # x, y notation

# Using ggplot:
ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

# Using qplot:
qplot(wt, mpg, data = mtcars)

# basic qplot scatter plot:
qplot(wt, mpg, data = mtcars)

# Categorical variable mapped onto size:
# cyl
qplot(wt, mpg, data = mtcars, size = factor(cyl))

# gear
qplot(wt, mpg, data = mtcars, size = gear)

# Continuous variable mapped onto col:
# hp
qplot(wt, mpg, data = mtcars, col = hp)

# qsec
qplot(wt, mpg, data = mtcars, col = qsec)

# Choosing geoms
# qplot() with x only
qplot(factor(cyl), data = mtcars)

# qplot() with x and y
qplot(factor(cyl), factor(vs), data = mtcars)

# qplot() with geom set to jitter manually
qplot(factor(cyl), factor(vs), data = mtcars, geom = "jitter")

# cyl and am are factors, wt is numeric
class(mtcars$cyl)
class(mtcars$am)
class(mtcars$wt)

# "Basic" dot plot, with geom_point():
ggplot(mtcars, aes(cyl, wt, col = am)) +
  geom_point(position = position_jitter(0.2, 0))

# 1 - "True" dot plot, with geom_dotplot():
ggplot(mtcars, aes(cyl, wt, fill = am)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

# 2 - qplot with geom "dotplot", binaxis = "y" and stackdir = "center"
qplot(
  cyl, wt,
  data = mtcars,
  fill = am,
  geom = "dotplot",
  binaxis = "y",
  stackdir = "center"
)

# Wrap Up
# ChickWeight is available in your workspace
# 1 - Check out the head of ChickWeight
head(ChickWeight)

# 2 - Basic line plot
ggplot(ChickWeight, aes(x = Time, y = weight)) +
  geom_line(aes(group = Chick))

# 3 - Take plot 2, map Diet onto col.
ggplot(ChickWeight, aes(x = Time, y = weight, col = Diet)) +
  geom_line(aes(group = Chick))

# 4 - Take plot 3, add geom_smooth()
ggplot(ChickWeight, aes(x = Time, y = weight, col = Diet)) +
  geom_line(aes(group = Chick), alpha = 0.3) +
  geom_smooth(lwd = 2, se = FALSE)

# titanic is avaliable in your workspace
# 1 - Check the structure of titanic
str(titanic)

# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_grid(.~Survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = Pclass, y = Age, col = Sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) +
  facet_grid(.~Survived)