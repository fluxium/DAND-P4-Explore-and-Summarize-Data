# setwd for Home workstation
setwd("C:/Users/Mahlon/Source/Repos/DAND-P4-Explore-and-Summarize-Data/EDA_Course_Materials/lesson4")

# setwd for Work workstation
setwd("C:/Source (Remote)/Repos/DAND-P4-Explore-and-Summarize-Data/EDA_Course_Materials/lesson4")

load('.rdata')

library(ggplot2)
library(tidyr)
library(dplyr)
library(xlsx)
library(lubridate)
library(gridExtra)

data(diamonds)

# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================

ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point() +
  scale_x_continuous(breaks = seq(0,19000, 1000))

ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point(alpha = 1/40)

ggplot(aes(x = x, y = log(price)), data = diamonds) +
  geom_point()

cor.test(diamonds$x, diamonds$price)
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

# Create a simple scatter plot of price vs depth.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
#==================================================

ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point()

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.

# This assignment is not graded and
# will be marked as correct when you submit.

# ALTER THE CODE BELOW THIS LINE
#============================================

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0,100,2))

cor.test(diamonds$depth, diamonds$price)

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ========================================

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() +
  scale_x_continuous()

price_lim = quantile(diamonds$price, c(.99))[[1]]
carat_lim = quantile(diamonds$carat, c(.99))[[1]]

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, carat_lim)) +
  scale_y_continuous(limits = c(0, price_lim))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# =================================================================

diamonds$volume <- with(data = diamonds, x * y * z)

# Depends on library(plyr)
# count(diamonds$volume == 0) # 20 0 volumes found
# detach("package:plyr", unload=TRUE)

ggplot(data = diamonds, aes(x = volume, y = price)) + 
  geom_point()

diamonds_cor_test <- subset(diamonds, diamonds$volume > 0 & diamonds$volume <= 800)
cor.test(diamonds_cor_test$volume, diamonds_cor_test$price)

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

# We encourage you to think about this next question and
# to post your thoughts in the discussion section.

# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ========================================

diamonds_cor_test <- subset(diamonds, diamonds$volume > 0 & diamonds$volume <= 800)

ggplot(data = diamonds_cor_test, aes(x = volume, y = price)) + 
  geom_point(alpha = 1/40) +
  scale_y_continuous(limits = c(0, 20000)) +
  # Linear model is not a very good fit for these data
  stat_smooth(method = "lm", formula = y ~ x, size = 1)

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

# This assignment WILL BE automatically
# graded!

# DO NOT ALTER THE NEXT THREE LINES OF CODE.
# ======================================================
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
data(diamonds)

# ENTER YOUR CODE BELOW THIS LINE
# ======================================================

diamondsByClarity <- group_by(diamonds, clarity) %>%
                     summarise(mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n()) %>% 
                     arrange(clarity, n)


# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

# This assignment is not graded and
# will be marked as correct when you submit.

# See the Instructor Notes for more info on bar charts
# and for a hint on this task.

# DO NOT DELETE THE LINES OF CODE BELOW
# ===================================================================
data(diamonds)
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

# ENTER YOUR CODE BELOW THIS LINE
# ===================================================================

diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))

p1 <- ggplot(data = diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) +
  geom_bar(stat = "identity")

p2 <- ggplot(data = diamonds_mp_by_color, aes(x = color, y = mean_price)) +
  geom_bar(stat = "identity")

p3 <- ggplot(data = diamonds_mp_by_cut, aes(x = cut, y = mean_price)) +
  geom_bar(stat = "identity")

grid.arrange(p1, p2, p3)
  
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 3 or you can start fresh and choose a different
# data set from Gapminder.

# If you're feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================

# Copied from PS 3

ysmen <- read.xlsx("Years in school men 25-34.xlsx", 1)

ysmen <- gather(ysmen, key = year, value = years_in_school,
                -Row.Labels)

yswomen <- read.xlsx("Years in school women 25-34.xlsx", 1)

yswomen <- gather(yswomen, key = year, value = years_in_school,
                  -Row.Labels)

ysmen$gender <- factor('M')
yswomen$gender <- factor('F')

# this is basically a union, http://www.statmethods.net/management/merging.html
ys <- rbind(ysmen, yswomen)

ys$year <- substring(ys$year, 2)

ys$year <- factor(ys$year)
ys$years_in_school <- as.numeric(ys$years_in_school)
ys$country <- ys$Row.Labels

ys <- subset(ys, select = -Row.Labels)

# In this plot we can see the points for the yearly observations by country
ggplot(aes(x = country, y = years_in_school), data = ys) +
  geom_point() +
  # Handy line to adjust the rotation of x-axis labels
  # http://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# This might be clearer as a boxplot
ggplot(aes(x = year, y = years_in_school), data = ys) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot of above
ggplot(aes(x = year, y = years_in_school), data = ys) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What are the most improved countries
ys_most_improved <- group_by(ys, country) %>%
                    summarize(delta = max(years_in_school) - min(years_in_school),
                              mean = mean(years_in_school),
                              median = median(years_in_school),
                              n = n())

# Plot the delta over time by country
ggplot(aes(x = country, y = delta), data = ys_most_improved) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# most improved by gender, by country
ys_most_improved_gender <- group_by(ys, country, gender) %>%
                           summarize(delta = max(years_in_school) - min(years_in_school),
                                     mean = mean(years_in_school),
                                     median = median(years_in_school),
                                     n = n())

# Plot the delta over time by gender and country
ggplot(aes(x = country, y = delta, color = gender), data = ys_most_improved_gender) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 10))


