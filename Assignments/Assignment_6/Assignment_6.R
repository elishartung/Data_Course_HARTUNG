install.packages('patchwork')
library(tidyverse)
library(patchwork)
#1-3
data("mtcars")
mtcars
?mtcars
my_subset <- mtcars[mtcars$am==0,]
write.csv(my_subset,"automatic_mtcars.csv")

#4-5
ggplot(my_subset, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Horsepower vs Miles-per-Gallon", y="mpg", x="horsepower") 
ggsave("mpg_vs_hp_auto.png")

#6-7
ggplot(my_subset, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Weight vs Miles-per-Gallon", y="mpg", x="wt (lb x 1000)") 
ggsave("mpg_vs_wt_auto.tiff")



#8-9
subset_2 <-  mtcars[mtcars$disp<=200 & mtcars$am==0,]
write.csv(subset_2,"mtcars_max200_displ.csv")

#10-11
max(mtcars$hp)
max(my_subset$hp)
max(subset_2$hp)
subset_3 <- c(max(mtcars$hp),max(my_subset$hp),max(subset_2$hp))
write.table(subset_3, "hp_maximums.txt")
#12-13
p1 <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title= "wt vs mpg", x= "wt", y="mpg")
mtcars$cyl <- as.factor(mtcars$cyl)
p2 <- ggplot(mtcars, aes(x=cyl,y=mpg, color=cyl))+
  geom_violin() +
  facet_wrap(~cyl)
p3 <- ggplot(mtcars, aes(x=hp, y=mpg, color=cyl)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title= "hp vs mpg", x= "hp", y="mpg")
(p1+p3)/p2
ggsave("combined_mtcars_plot.png")