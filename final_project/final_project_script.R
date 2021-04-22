library(tidyverse)
library(janitor)
library(broom)
library(GGally)
library(purrr)
library(readxl)
library(dplyr)
library(g

beehive<-read_excel("../../microsite/beehive_clean.xlsx")
wd<- read_excel("../../microsite/wd_clean.xlsx")
wd$roughness<- as.character(wd$roughness)
rb<- read_excel("../../microsite/rb_clean.xlsx")
wb<- full_join(beehive, wd)
rb$roughness <- as.character(rb$roughness)
full<- full_join(wb,rb)

Disturbance <- ggplot(full, aes(x = disturbance, colour= population)) + 
  geom_bar() +
  theme_minimal() +
  labs(x= "Disturbance", y= "Number of Plants In Disturbed Plots (n= ~200)") +
  theme(axis.text.x = element_text(angle=90)) +
  theme(axis.text.x = element_text(size= 6)) +
  facet_wrap(~population)
ggsave("Disturbance.jpg")

Cover <- ggplot(full) +
  geom_bar()


