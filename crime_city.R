# Analysis of the trend in homicide rates for US cities 

# Load the necessary libraries
library(RCurl) # for grabbing data
library(ggplot2) # for plot
library(ggthemes) # for plot themes
library(dplyr) # for summarising
library(broom) # for regression analysis

# Get data
x <- getURL("https://raw.githubusercontent.com/themarshallproject/city-crime/master/data/ucr_crime_1975_2015.csv")
crime <- read.csv(text = x) # Import data
crime <- crime[,1:15] # Delete extraneous columns
cf <- crime[complete.cases(crime),] # Filter out incomplete records

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

# Calculate the log population
cf$logpop <- log10(cf$total_pop)

# Calculate correlation of log10 population and homicide rate by year
cor <- cf %>%
  group_by(year) %>%
  summarise(correlation = cor(logpop, homs_per_100k))
plot(cor)

# Regression model for each year
regs <- cf %>%
  group_by(year) %>% 
  do(tidy(lm(homs_per_100k ~ total_pop, data = .)))
plot(regs$estimate)
pop.est <- filter(regs, term=="total_pop")
plot(pop.est$estimate)
