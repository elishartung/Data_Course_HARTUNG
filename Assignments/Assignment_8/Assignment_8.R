library(tidyverse)
library(modelr)
library(stringr)
library(skimr)
library(GGally)
library(broom)
library(MASS)

# 1 load
mushroom<- read.csv("../../Data/mushroom_growth.csv")

# 2 several plots
ggpairs(mushroom)

ggplot(mushroom, aes(y=GrowthRate, x=Light, color= Species)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(mushroom, aes(y=GrowthRate, x=Humidity, color= Species)) +
  geom_point()

ggplot(mushroom, aes(y=GrowthRate, x=Temperature, color= Species)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(mushroom, aes(y=GrowthRate, x=Nitrogen, color= Species)) +
  geom_point() +
  geom_smooth(method="lm")

# 3 models

mod1 <- glm(data=mushroom, formula= GrowthRate ~Light* Humidity)
mod2 <- aov(formula= GrowthRate ~Light* Humidity*Species,mushroom)

mod1summary <- summary(mod1)
mod2summary <- summary(mod2)

# 4 mean squared error


mean(mod1$residuals^2)
mean(mod2$residuals^2)


#5-6-7 make predictions and add to plot of mod2 (the better model)

add_predictions(mushroom,mod2) %>%
  ggplot(aes(x=Light, color=Species)) +
  geom_point(aes(y= GrowthRate)) +
  geom_point(aes(y=pred), color= "Black") +
  geom_smooth(aes(y=GrowthRate), method="lm")

non<-read.csv("../../Data/non_linear_relationship.csv")
ggplot(non,aes(x=predictor, y=response)) +
  geom_point() +
  geom_smooth(method="nls")

library()
mod3<- glm(data=non, formula= response~ cuberoot(predictor))

add_predictions(non,mod3) %>%
  ggplot(aes(x=predictor)) +
  geom_point(aes(y= response)) +
  geom_point(aes(y=pred), color="Red") +
  geom_smooth(aes(y=response), method="lm")

library(gnm)
mod3 <- gnm(formula=response~predictor, data=non)