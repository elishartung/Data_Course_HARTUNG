DNA_Conc_by_Extraction_Date
head(DNA_Conc_by_Extraction_Date)
library(ggplot2)
library(tidyverse)

#1
ggplot(DNA_Conc_by_Extraction_Date, aes(x=DNA_Concentration_Katy)) +
  geom_histogram(color="black",fill="lightblue") +
  labs(title= "Katy's DNA Concentration Counts", x="DNA Concentration")
ggplot(DNA_Conc_by_Extraction_Date, aes(x=DNA_Concentration_Ben)) + 
  geom_histogram(color= "black",fill="red") +
  labs(title= "Ben's DNA Concentration Counts", x="DNA Concentration")

#2-3
katy_plot <- ggplot(DNA_Conc_by_Extraction_Date, aes(x=factor(Year_Collected), y=DNA_Concentration_Katy,)) +
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))
katy_plot + theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(0,1,0.2)) + scale_x_discrete(breaks=seq(2000,2012,2)) +
  labs(title="Katy's Extractions", x="YEAR", y="DNA Concentrations") + theme(plot.title = element_text(hjust=0.5, face="bold")) +
  theme(aspect.ratio=1)
ggsave(filename="HARTUNG_Plot1.jpeg",)

ben_plot <- ggplot(DNA_Conc_by_Extraction_Date, aes(x=factor(Year_Collected), y=DNA_Concentration_Ben,)) +
  geom_boxplot(linetype = "dashed", outlier.shape = 1) +stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))
ben_plot + theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=1)) + scale_y_continuous(breaks=seq(0,3,0.5)) + scale_x_discrete(breaks=seq(2000,2012,2)) +
  labs(title="Ben's Extractions", x="YEAR", y="DNA Concentrations") + theme(plot.title = element_text(hjust=0.5, face="bold")) +
  theme(aspect.ratio=1)
ggsave(filename="HARTUNG_Plot2.jpeg",)

#4
ben_extrac <- c(DNA_Conc_by_Extraction_Date$DNA_Concentration_Ben)
katy_extrac <- c(DNA_Conc_by_Extraction_Date$DNA_Concentration_Katy)
DNA_Conc_by_Extraction_Date$differences <- ben_extrac-katy_extrac
DNA_Conc_by_Extraction_Date$Year_Collected[DNA_Conc_by_Extraction_Date$differences == min(DNA_Conc_by_Extraction_Date$differences)]

#5
downstairs_data <- DNA_Conc_by_Extraction_Date[which(DNA_Conc_by_Extraction_Date$Lab=="Downstairs"),] 
ggplot(downstairs_data, aes(x=Date_Collected, y=DNA_Concentration_Ben)) + 
  geom_point(shape=1) +
  theme_classic() +
  theme( panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  theme(axis.title.x=element_text(size=9)) +
  theme(axis.title.y=element_text(size=9)) +
  theme(axis.line = element_line(colour = 'black', size = .25)) +
  theme(axis.ticks.length=unit(.25, "cm")) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  theme(aspect.ratio=1)
ggsave("Ben_DNA_over_time.jpg")

#6
tiny_data_set <- aggregate(DNA_Conc_by_Extraction_Date$DNA_Concentration_Ben~DNA_Conc_by_Extraction_Date$Year_Collected,DNA_Conc_by_Extraction_Date,mean)
subset(tiny_data_set,DNA_Conc_by_Extraction_Date$Year_Collected== "2007")
write.csv(tiny_data_set,"Ben_Average_Conc.csv")
