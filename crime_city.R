library(ggplot2)
library(RCurl)
library(dplyr)
library(ggthemes)
library(broom)

# Get data, delete extraneous columns and filter to complete cases
x <- getURL("https://raw.githubusercontent.com/themarshallproject/city-crime/master/data/ucr_crime_1975_2015.csv")
crime <- read.csv(text = x)
crime.s <- crime[,1:15]
cf <- crime.s[complete.cases(crime.s),]

# Plot homicide rate by city size by year
p <- ggplot(cf, aes(total_pop, homs_per_100k)) + 
  geom_point(colour="red", alpha=0.5, aes(size=total_pop)) +
  geom_smooth(method = "lm", se = FALSE) + 
  scale_x_log10(breaks = c(1e+05,1e+06,1e+07), labels = c("100k", "1m", "10m")) +
  facet_wrap(~ year) +
  labs(title = "US city population and homicide rates, 1975-2014",
       x = "City population", y = "Homicide rate per 100,000 population")
p + theme_minimal() + theme(panel.margin=unit(1, "lines")) +
  theme(legend.title=element_blank())

# Calculate correlation of pop and homicide rate by year
cor <- cf %>%
  group_by(year) %>%
  summarise(correlation = cor(total_pop, homs_per_100k))
cor

# Regression model for each year
regs <- cf %>%
  group_by(year) %>% 
  do(tidy(lm(homs_per_100k ~ total_pop, data = .)))