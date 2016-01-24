library(ggplot2)
library('tidyr')
library('dplyr')
library(xlsx)

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
# This really didn't so anything :(
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


