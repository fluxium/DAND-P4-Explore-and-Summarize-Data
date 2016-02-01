# Home Working Directory
setwd("C:/Users/Mahlon/Source/Repos/DAND-P4-Explore-and-Summarize-Data/EDA_Course_Materials/lesson3")

# Work Working Directory
setwd("C:/Source (Remote)/Repos/DAND-P4-Explore-and-Summarize-Data/EDA_Course_Materials/lesson3")

load('.rdata')

library(ggplot2)
library('tidyr')
library('dplyr')
library(xlsx)
library(lubridate)

data(diamonds)
dim(diamonds)
summary(diamonds)
str(diamonds)
?diamonds
levels(diamonds$color)

qplot(x = price, data = diamonds)
summary(diamonds$price)

lt500 <- diamonds$price < 500
sum(lt500 == "TRUE")
lt250 <- diamonds$price < 250
sum(lt250 == "TRUE")
gte15000 <- diamonds$price >= 15000
sum(gte15000 == "TRUE")

qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous(limits = c(0, 3000))
ggsave('diamond_price_0_to_3000_bw_10.png')

qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous(limits = c(1000, 2000))
ggsave('diamond_price_1000_to_2000_bw_10_void_1500.png')

cheapdiamonds <- subset(diamonds, price < 1500)

summary(cheapdiamonds$price)

qplot(x = price, data = cheapdiamonds, binwidth = 1) +
  scale_x_continuous(limits = c(0, 2000))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

qplot(x = price, data = diamonds, binwidth = 0.1) +
  scale_x_log10()

qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous() +
  facet_wrap(~cut)
ggsave('diamond_price_by_cut.png')

qplot(x = cut, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian(ylim = c(18000, 19000))

qplot(x = cut, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian(ylim = c(250, 400))

qplot(x = cut, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian()

by(diamonds$price, diamonds$cut, summary)

qplot(x = price, data = diamonds) + facet_wrap(~cut)

?facet_wrap

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")

qplot(x = price/carat, data = diamonds) + 
  facet_wrap(~cut, scales = "free_y") +
  scale_x_log10()

# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

qplot(x = clarity, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian()

qplot(x = clarity, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian() +
  scale_y_log10()

by(diamonds$price, diamonds$clarity, summary)

qplot(x = color, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian()

qplot(x = color, y = price, data = diamonds, geom = 'boxplot') +
  coord_cartesian() +
  scale_y_log10()

by(diamonds$price, diamonds$color, summary)

by(diamonds$price / diamonds$carat, diamonds$color, summary)

qplot(x = color, y = price / carat, data = diamonds, geom = 'boxplot') +
  coord_cartesian()

qplot(x = color, y = price / carat, data = diamonds, geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 7500))
ggsave('price_per_carat_by_color.png')

qplot(x = color, y = price / carat, data = diamonds, geom = 'boxplot') +
  coord_cartesian() +
  scale_y_log10()

qplot(x = color, y = log10(price / carat), data = diamonds, geom = 'boxplot') +
  coord_cartesian()

str(fbdf$carat)

summary(diamonds$carat)

by(diamonds$carat, diamonds$color, summary)

qplot(x = color, y = carat, data = diamonds, geom = 'boxplot') +
  coord_cartesian()
ggsave('carat_by_color.png')

qplot(x = carat, data = diamonds, 
      binwidth = 0.01, geom = 'freqpoly')

qplot(x = carat, data = diamonds, binwidth = 0.01)

install.packages('tidyr')

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================================

ysmen <- read.xlsx("Years in school men 25-34.xlsx", 1)

#head(ysmen)

ysmen <- gather(ysmen, key = year, value = years_in_school,
                -Row.Labels)

yswomen <- read.xlsx("Years in school women 25-34.xlsx", 1)

#head(yswomen)

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

sum(is.na(ys$year))
subset(ys, is.na(ys$year))

summary(ys$years_in_school)

# Are there the same number of observations in each year
qplot(year, data = subset(ys, !is.na(ys$years_in_school)), binwidth = 1)

# The spike around 0.5 years in school shows that more countries
# report that far more girls receive less than a two years of education than
# boys
qplot(years_in_school, data = subset(ys, !is.na(ys$years_in_school)), binwidth = 0.1,
      geom = 'freqpoly', color = gender)

# zoom in on the low end
qplot(years_in_school, data = subset(ys, !is.na(ys$years_in_school)), binwidth = 0.1,
      geom = 'freqpoly', color = gender, xlim = c(0, 5))

# In these data most years_in_school averages are between 2 and 12
qplot(years_in_school, data = ys, binwidth = 0.1) 

qplot(x = year, y = country, data = ys)

# Average years in school globally growing, 
ggplot(aes(x = year, y = years_in_school), data = 
         subset(ys, !is.na(ys$years))) +
  geom_point()
  
# box plot shows the growth more clearly
ggplot(aes(x = year, y = years_in_school), data = 
         subset(ys, !is.na(ys$years))) +
  geom_boxplot()

# Might be interesting to plot the delta(max - min) over time
# This really didn't show anything :(
ggplot(aes(x = year, y = sqrt(max(years_in_school) - min(years_in_school))),
       data = subset(ys, !is.na(ys$years))) +
  geom_point()

by(ys$years_in_school, ys$gender, summary)

# I am wondering what the global inequality of education looks like
ggplot(aes(y = years_in_school, x = gender), data = ys) +
  geom_boxplot()
# Interesting to note that at the maximum Females are higher, the other
# values make sense

# At the very low end the disparity is very evident
ggplot(aes(y = years_in_school, x = gender), data = ys) +
  ylim(c(0,5)) +
  geom_boxplot()

# At the upper end Females are leading with highter median, 3qt and max
ggplot(aes(y = years_in_school, x = gender), data = ys) +
  ylim(c(10,15)) +
  geom_boxplot()

# How is the low end changing in the data set
ggplot(aes(y = years_in_school, x = gender), data = ys) +
  ylim(c(0,5)) +
  geom_boxplot() +
  facet_wrap(~year)

# How is the upper end is changing in the data set, seems like access
# Education has been fairly equal at the upper end
ggplot(aes(y = years_in_school, x = gender), data = ys) +
  ylim(c(10,15)) +
  geom_boxplot() +
  facet_wrap(~year)

# Your task is to investigate the distribution of your friends'
# birth months and days.

# Here some questions you could answer, and we hope you think of others.

# **********************************************************************

# How many people share your birthday? Do you know them?
# (Reserve time with them or save money to buy them a gift!)

# Which month contains the most number of birthdays?

# How many birthdays are in each month?

# Which day of the year has the most number of birthdays?

# Do you have at least 365 friends that have birthdays on everyday
# of the year?

# **********************************************************************

# You will need to do some data munging and additional research to
# complete this task. This task won't be easy, and you may encounter some
# unexpected challenges along the way. We hope you learn a lot from it though.

# You can expect to spend 30 min or more on this task depending if you
# use the provided data or obtain your personal data. We also encourage you
# to use the lubridate package for working with dates. Read over the documentation
# in RStudio and search for examples online if you need help.

# You'll need to export your Facebooks friends' birthdays to a csv file.
# You may need to create a calendar of your Facebook friends' birthdays
# in a program like Outlook or Gmail and then export the calendar as a
# csv file.

# Once you load the data into R Studio, you can use the strptime() function
# to extract the birth months and birth days. We recommend looking up the
# documentation for the function and finding examples online.

# We've included some links in the Instructor Notes to help get you started.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation below the line. Submit it when you are ready.
# ===============================================================================

bdays <- read.csv("friends_bdays.csv")
head(bdays)
# Sort needed because strsplit outputs an ordered list of cleaned names
subset(bdays, select = c(Title, Start.date)) %>% arrange(Title) -> bdays
bdays <- subset(bdays, !(Title == "")) # Excel added an empty row
row.names(bdays) <- c(1:111) # Reindex so row.names matches output of sapply
bdays$name <- sapply(strsplit(levels(bdays$Title), "'")[2:112], function (x) x[1] )
bdays <- subset(bdays, select = c(name, Start.date))
colnames(bdays) <- c("name", "birthday")

# Clean the birthday column and separate in to month and day
bdays$birthday <- strptime(bdays$birthday, "%m/%d/%Y", tz = "GMT")
bdays$bmonth <- month(bdays$birthday)
bdays$bday <- day(bdays$birthday)
bdays$bmonthday <- paste(month(bdays$birthday), day(bdays$birthday))

# Exploration
ggplot(aes(x = birthday), data = bdays) +
  geom_histogram(bins = 365) +
  scale_x_datetime(date_breaks = '1 month', date_labels = "%m")

# How many people share your birthday? Do you know them?
# (Reserve time with them or save money to buy them a gift!)

# https://jonkimanalyze.wordpress.com/2014/03/25/ggplot2-time-series-axis-control-breaks-label-limitshttpsjonkimanalyze-wordpress-comwp-adminedit-phpinline-edit/
# No one shares my birthday
month_start <- strptime(paste("2016-03-01", "00:00:00"), "%Y-%m-%d %H:%M:%S")
month_end <- strptime(paste("2016-03-31", "00:00:00"), "%Y-%m-%d %H:%M:%S")
month_lim <- as.POSIXct(c(month_start, month_end), origin="1970-01-01", tz="GMT")

ggplot(aes(x = birthday), data = bdays) +
  geom_histogram(bins = 31) +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%m-%d",
                   limits = month_lim)

# Which month contains the most number of birthdays?

# Seems like March is the most commmon birth month is these data
ggplot(aes(x = bmonth), data = bdays, xlim = c(1, 12)) +
  geom_histogram(binwidth = 1) +
  scale_x_discrete() +
  scale_y_discrete(breaks = seq(0,20))

# How many birthdays are in each month

# J 6, F 9, M 13, A 12, M 5, J 9, J 12, A 7, S 12, O 9, N 6, D 12
ggplot(aes(x = bmonth), data = bdays, xlim = c(1, 12)) +
  geom_histogram(binwidth = 1) +
  scale_x_discrete() +
  scale_y_discrete(breaks = seq(0,20))

# Which day of the year has the most number of birthdays?

# I can see that March has the day with the most number of birthdays
ggplot(aes(x = birthday), data = bdays) +
  geom_histogram(bins = 365) +
  scale_x_datetime(date_breaks = '1 month', date_labels = "%m")

# March 11 has 4 birthdays
month_start <- strptime(paste("2016-03-01", "00:00:00"), "%Y-%m-%d %H:%M:%S")
month_end <- strptime(paste("2016-03-31", "00:00:00"), "%Y-%m-%d %H:%M:%S")
month_lim <- as.POSIXct(c(month_start, month_end), origin="1970-01-01", tz="GMT")

ggplot(aes(x = birthday), data = bdays) +
  geom_histogram(bins = 31) +
  scale_x_datetime(date_breaks = '1 day', date_labels = "%m-%d",
                   limits = month_lim)

# Other exploration
# Most common day of the month for a birthday is 2nd, least common is 16th with
# 0 occurances in these data
ggplot(aes(x = bday), data = bdays) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 31))

# I have no friends with a leap year birthday
by(bdays$bday, bdays$bmonth, summary)

