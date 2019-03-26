library(tidyverse)
weather <- read_csv("https://www.iun.edu/~cisjw/ds/files/data/weather.csv")
summary(table(weather$windy, weather$play))
summary(table(weather$outlook, weather$play))


x <- c(2,-1,3,5)
y <- c(-1,0,5,3)
z <- c(0,2,-3,5)
data <- tibble(x,y,z)
corrmat <- cor(data, method = 'pearson')
corrmat

iris
cor(iris[,1:4])
iris %>% select(1:4) %>% cor

library(polycor)
weather.cor <- hetcor(weather$play, weather$outlook, weather$temperature,
       weather$humidity,weather$windy)
weather.cor$correlations

names <- c("play", "outlook", "temperature", "humidity", "windy")
weather.corrmat <- as_tibble(weather.cor$correlations)
colnames(weather.corrmat) <- names

weather.corrmat

weather.corrmat <- weather.corrmat %>%
  mutate(attribute=names) %>%
  select(attribute, play, outlook, temperature, humidity, windy)

rank <- weather.corrmat %>%
  select(attribute, play) %>%
  mutate(squared.correlation = play^2) %>%
  arrange(desc(squared.correlation))

rank
rank$attribute

install.packages("corrplot")
library(corrplot)

weather.cor <- hetcor(weather$play, weather$outlook, weather$temperature,
                      weather$humidity,weather$windy)
corrplot(weather.cor$correlations, method="shade", type="upper", tl.col="black", tl.srt=45)







