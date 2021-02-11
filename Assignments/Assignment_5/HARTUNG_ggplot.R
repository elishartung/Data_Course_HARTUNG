library(tidyverse)

ggplot(iris, aes(x=Sepal.Length,y=Petal.Length,color=Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "whatever", subtitle = "something")

ggplot(iris, aes(x=Petal.Width,fill=Species)) +
  geom_density(alpha=.5) +
  theme_minimal()

ggplot(iris, aes(x=Species, y= Petal.Width / Sepal.Width, fill=Species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="Ratio of")

iris$Deviance <- iris$Sepal.Length-mean(iris$Sepal.Length)


iris2 <- iris[order(iris$Deviance),]
  
ggplot(iris2, aes(x=1:150,y=Sepal.Length - mean(iris$Sepal.Length),fill=Species)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal()

