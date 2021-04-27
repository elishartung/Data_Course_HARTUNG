library(tidyverse)
library(janitor)
library(broom)
library(GGally)
library(purrr)
library(readxl)
library(dplyr)
library(stringr)


#1
faculty <- read.csv("FacultySalaries_1995.csv")


names(faculty) <- names(faculty) %>% make_clean_names() %>%
  str_remove("avg_") %>%
  str_remove("_prof_salary") 

names(faculty)[names(faculty) == "full"] <- "Full"
names(faculty)[names(faculty) == "assoc"] <- "Assoc"
names(faculty)[names(faculty) == "assist"] <- "Assist"

clean_faculty <- faculty %>% 
  filter(univ_name != "Shepherd College") %>%
  pivot_longer(cols= c("Full","Assoc","Assist"),
   names_to = "Rank",
  values_to = "Salary") 

HARTUNG_Fig_1 <- ggplot(clean_faculty, aes( x=Rank, y= Salary, fill= Rank)) +
  geom_boxplot(lwd=.75, outlier.size=3) +
  facet_wrap(~tier) +
  theme_minimal() +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) + 
  theme(axis.text = element_text(size=15)) + 
  theme(legend.key.size = unit(2,"line")) +
  theme(legend.text = element_text(size=15)) +
  theme(legend.title = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 60)) +
  theme(strip.text= element_text(size=15))
ggsave("HARTUNG_Fig_1.jpg")

#2
Salary_ANOVA_Summary <- aov(Salary ~ state + Rank + tier, data= clean_faculty) 
capture.output(summary(Salary_ANOVA_Summary),file="Salary_ANOVA_Summary.txt")
  

#3
juniper <- read.csv("juniper_Oils.csv")
names(juniper) <- names(juniper) %>% make_clean_names()
clean_juniper <- juniper %>% 
  pivot_longer( cols = c("alpha_pinene","para_cymene","alpha_terpineol",
                         "cedr_9_ene","alpha_cedrene","beta_cedrene","cis_thujopsene",
                         "alpha_himachalene","beta_chamigrene","cuparene","compound_1",
                         "alpha_chamigrene","widdrol","cedrol","beta_acorenol",
                         "alpha_acorenol","gamma_eudesmol","beta_eudesmol","alpha_eudesmol",
                         "cedr_8_en_13_ol","cedr_8_en_15_ol","compound_2","thujopsenal"),
                names_to = "ChemicalID",
                values_to = "Concentration")
names(clean_juniper)[names(clean_juniper) == "years_since_burn"] <- "YearsSinceBurn"


#4
HARTUNG_Fig_2 <- ggplot(clean_juniper, aes( x=YearsSinceBurn, y= Concentration,)) +
  geom_smooth(lwd=1.25) +
  facet_wrap(~ChemicalID, scales="free") +
  theme_minimal() +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) + 
  theme(axis.text = element_text(size=10)) +
  theme(strip.text= element_text(size=10)) 
ggsave("HARTUNG_Fig_2.jpg")

#5
mod1 <- glm(Concentration ~ ChemicalID + YearsSinceBurn, data= clean_juniper)
tibble <-tidy(mod1) 
filter(tibble, p.value<= .05)

