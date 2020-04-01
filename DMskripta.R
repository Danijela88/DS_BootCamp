#install.packages("rmarkdown")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("DT")
#install.packages("readr") 
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(DT))

athlete_events <- read_csv("athlete_events.csv")

glimpse(athlete_events)

ae_var_end_t <- select(athlete_events, ends_with("t"))
head(ae_var_end_t, n=1)

ae_var_start_S <- select(athlete_events, starts_with("S"))
head(ae_var_start_S, n=1)

ae_var_S_t <- select(athlete_events, ends_with("t") & starts_with("S"))
head(ae_var_S_t, n=1)

athlete_events <- mutate(athlete_events, BMI=Weight/(Height/100)^2)
head(athlete_events, n=1)

olympicSR <- filter(athlete_events, Team == "Serbia") 
olympicSR21c <- filter(olympicSR, Year >= 2000 & Weight > 100 & Height > 200)
dim(olympicSR)
dim(olympicSR21c)

arrange(olympicSR21c,Height)
arrange(olympicSR21c,desc(Height))

olympicSR$Name[olympicSR$Age==min(olympicSR$Age)]
(youngest <- filter(olympicSR,Age==min(Age)))

olympicSR$Name[olympicSR$Weight==max(olympicSR$Weight)]
(heaviest <- filter(olympicSR,Weight==max(Weight)))

summarise(olympicSR, max_Age = max(Age), max_BMI = max(BMI))
summarise(olympicSR, mean_Age = mean(Age), mean_BMI = mean(BMI))

athlete_events %>% 
  group_by(Medal) %>% 
  summarise(n())

athlete_events %>%
  filter(!is.na(Medal)) %>% 
  group_by(Team) %>% 
  summarise(n_medals = n()) %>%  
  arrange(desc(n_medals)) %>%  
  DT::datatable()

#install.packages("ggplot2")
library(ggplot2)
athlete_events %>%
  filter(!is.na(Medal)) %>%  
  group_by(Team) %>% 
  summarise(n_medals = n()) %>%   
  ggplot(aes(x=n_medals, y=Team))+
  geom_point(alpha = 0.9, shape = 20, col = "blue", size = 3) + 
  # give a title an label axes
  labs(title = "Broj medalja po timovima", 
       x = "Ukupan broj medalja", y = "Timovi") + 
  
  # modify the appearance
  theme(legend.position = "none", 
        panel.border = element_rect(fill = NA,  size = .75),
        plot.title=element_text(hjust=0.5))+
  scale_colour_brewer(palette = "Set1")



