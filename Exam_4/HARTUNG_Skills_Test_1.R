
library(ggplot2)
library(tidyverse)

DNA <-read.csv("DNA_Conc_by_Extraction_Date.csv")
#1
ggplot(DNA, aes(x=DNA_Concentration_Katy)) +
  geom_histogram(color="black",fill="lightblue") +
  labs(title= "Katy's DNA Concentration Counts", x="DNA Concentration")
ggplot(DNA, aes(x=DNA_Concentration_Ben)) + 
  geom_histogram(color= "black",fill="red") +
  labs(title= "Ben's DNA Concentration Counts", x="DNA Concentration")

#2-3
katy_plot <- ggplot(DNA, aes(x=factor(Year_Collected), y=DNA_Concentration_Katy,)) +
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))
katy_plot + theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(0,1,0.2)) + scale_x_discrete(breaks=seq(2000,2012,2)) +
  labs(title="Katy's Extractions", x="YEAR", y="DNA Concentrations") + theme(plot.title = element_text(hjust=0.5, face="bold")) +
  theme(aspect.ratio=1)
ggsave(filename="HARTUNG_Plot1.jpeg",)

ben_plot <- ggplot(DNA, aes(x=factor(Year_Collected), y=DNA_Concentration_Ben,)) +
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))
ben_plot + theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(0,3,0.5)) + scale_x_discrete(breaks=seq(2000,2012,2)) +
  labs(title="Ben's Extractions", x="YEAR", y="DNA Concentrations") + theme(plot.title = element_text(hjust=0.5, face="bold")) +
  theme(aspect.ratio=1)
ggsave(filename="HARTUNG_Plot2.jpeg",)

#4
ben_extrac <- c(DNA$DNA_Concentration_Ben)
katy_extrac <- c(DNA$DNA_Concentration_Katy)
DNA$differences <- ben_extrac-katy_extrac
DNA$Year_Collected[DNA$differences == min(DNA$differences)]

#5
downstairs_data <- DNA[which(DNA$Lab=="Downstairs"),] 
ggplot(downstairs_data, aes(x=Date_Collected, y=DNA_Concentration_Ben)) + 
  geom_point(shape=1) +
  theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) +
  theme(axis.line = element_line(colour = 'black', size = .25)) +
  theme(axis.ticks.length=unit(.25, "cm")) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  theme(aspect.ratio=1) + 
  scale_x_discrete(breaks = seq(2006, 2012, by = 2))
ggsave("Ben_DNA_over_time.jpg")

#6
tiny_data_set <- aggregate(DNA$DNA_Concentration_Ben~DNA$Year_Collected,DNA,mean)
subset(tiny_data_set,DNA$Year_Collected== "2007")
write.csv(tiny_data_set,"Ben_Average_Conc.csv")



