# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================

library(ggplot2)
library(RColorBrewer)
library(dplyr)

diamondsMB <- diamonds
# Releveling the cut ord. factor since the stacking behavior
# changed in ggplot 2.2.1
# https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
diamondsMB$cut <- factor(diamondsMB$cut, levels = rev(levels(diamondsMB$cut)))

ggplot(diamondsMB, aes(x=price, fill=cut)) +
  # binwidth took some trial and error haha
  geom_histogram(binwidth = 0.0582) +
  facet_wrap(~color) +
  scale_x_log10() +
  scale_y_continuous(breaks = seq(0, 700, 200)) +
  scale_fill_brewer(type = 'qual', direction = -1) +
  # The qual palette looks pretty good on dark
  theme_dark()

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================

library(ggplot2)
library(RColorBrewer)
library(dplyr)

diamondsMB <- diamonds
# Releveling the cut ord. factor since the stacking behavior
# changed in ggplot 2.2.1
# https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
diamondsMB$cut <- factor(diamondsMB$cut, levels = rev(levels(diamondsMB$cut)))

ggplot(diamondsMB, aes(x = table, y = price, color = cut)) +
  geom_point() +
  scale_x_continuous(breaks = seq(50, 80, 2), limits = c(50, 80)) +
  scale_color_brewer(type = 'qual', direction = -1) +
  theme_dark()

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================

library(ggplot2)
library(RColorBrewer)
library(dplyr)

diamondsMB <- diamonds
# Releveling the cut ord. factor since the stacking behavior
# changed in ggplot 2.2.1
# https://stackoverflow.com/questions/32345923/how-to-control-ordering-of-stacked-bar-chart-using-identity-on-ggplot2
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
diamondsMB$cut <- factor(diamondsMB$cut, levels = rev(levels(diamondsMB$cut)))
diamondsMB$vol <- with(data = diamondsMB, x * y * z)

# Some diamonds have zeros for one of the dimentions, removing
diamondsMB <- subset(diamondsMB, vol > 0)

ggplot(diamondsMB, aes(x = vol, y = price, color = clarity)) +
  geom_point() +
  scale_y_log10() +
  # quantile to remove top 1%
  scale_x_continuous(limits = c(0, quantile(diamondsMB$vol, .99)[[1]])) +
  scale_color_brewer(type = 'div') +
  theme_dark()

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

# This programming assignment WILL BE automatically graded.

# DO NOT DELETE THIS NEXT LINE OF CODE
# ========================================================================
pf <- read.delim('./datasets/pseudo_facebook.tsv')


# ENTER YOUR CODE BELOW THIS LINE
# ========================================================================
pf$prop_initiated <- with(pf, friendships_initiated / ifelse(friend_count == 0, 1, friend_count))

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================================
