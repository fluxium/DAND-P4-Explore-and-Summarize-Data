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