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

ggplot(data = diamonds, aes(x = carat, y = price), xlim = ) + 
  geom_point() +
  scale_x_continuous()