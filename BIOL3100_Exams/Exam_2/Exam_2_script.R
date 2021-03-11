library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
landdata<-read.csv("landdata-states.csv")
unicef<-read.csv("unicef-u5mr.csv")

landdata<-landdata %>% 
  rename(
    Region = region)

    
#1
HARTUNG_Fig_1<-ggplot(landdata, aes(x=Year, y=Land.Value, color=Region)) + 
         geom_smooth(size=1.25)+
  theme(aspect.ratio = ) +
  theme_minimal() +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) + 
  theme(axis.text = element_text(size=10)) + 
  theme(legend.key.size = unit(1.25,"line")) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.title = element_text(size=15)) +
  labs(y="Land Value (USD)")
ggsave("HARTUNG_Fig_1.jpg")

#2
landdata_NA<-subset(landdata, select= c(State,Region))
landdata_unique<- unique(landdata_NA)
landdata_unique[!complete.cases(landdata_unique), ] 





#3
colnames(unicef)=str_remove(colnames(unicef), pattern = ("U5MR."))

unicef_l = gather(unicef, Year, MortalityRate, 2:67, na.rm = TRUE)
unicef_l$Yearn= as.numeric(unicef_l$Year)
#4

HARTUNG_Fig_2<-ggplot(unicef_l, aes(x=Yearn, y=MortalityRate, color=Continent)) +
  geom_point(size=2.25) +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1960, 2000, 20), limits=c(1950,2010)) + 
  theme(axis.title.x = element_text(size=15))+ 
  theme(axis.title.y = element_text(size=15)) + 
  theme(axis.text = element_text(size=10)) + 
  theme(legend.key.size = unit(1.25,"line")) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.title = element_text(size=15)) + 
  xlab("Year")
ggsave("HARTUNG_Fig_2.jpg")

#5
meanmortality <- unicef_l %>% 
  group_by(Continent, Yearn) %>% 
  summarise(meanmortality=mean(MortalityRate))
HARTUNG_Fig_3<-ggplot(meanmortality, aes(x=Yearn, y=meanmortality, group=Continent, color=Continent)) +
  geom_line(size=2.5) + 
  theme_minimal() +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.title.y = element_text(size=15)) + 
  theme(axis.text = element_text(size=10)) + 
  theme(legend.key.size = unit(1.25,"line")) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.title = element_text(size=15)) +
  labs(x="Year", y="Mean Mortality Rate (deaths per 1000 live births)")
ggsave("HARTUNG_Fig_3.jpg")

#6
unicef_l$pmort = unicef_l$MortalityRate/1000 
 
HARTUNG_Fig_4<-ggplot(unicef_l, aes(x= Yearn, y=pmort)) +
  geom_point(size=.25, color="blue") +
  theme_minimal() +
  facet_wrap(~Region) +
  theme(strip.background = element_rect(colour = "black"))+
  theme(strip.text= element_text(size=7)) +
  labs(x="Year", y="Mortality Rate")

ggsave("HARTUNG_Fig_4.jpg")



