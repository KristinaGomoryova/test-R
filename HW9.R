install.packages("readr")
library(readr)
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
library(plotly)
tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
str(tx_injuries)
ncol(tx_injuries)
nrow(tx_injuries)
str(safer_parks)
ncol(safer_parks)
tx_injuries$age <- as.numeric(as.character(tx_injuries$age))
age <- tx_injuries %>% 
  group_by(gender) %>% 
  summarise(mean = mean(age))
tx_injuries %>% 
  group_by(city, gender) %>% 
  ggplot(., aes(x = city, y = gender))+
  geom_bar(aes(fill = gender),stat = "identity")
tx_injuries$gender[tx_injuries$gender == "m"] <- "M"
tx_injuries$gender[tx_injuries$gender == "n/a"] <- "NA"
tx_injuries$gender[tx_injuries$gender == "N/A"] <- "NA"
tx_injuries$gender[is.na(tx_injuries["gender"])] <- "NA"
max_age <- tx_injuries %>% 
  group_by(body_part) %>% 
  summarise(max = max(age))

safer_parks %>% 
  group_by(age_youngest) %>% 
  ggplot(aes(x=age_youngest, y=num_injured))+
  geom_point()
m_age <- safer_parks %>% 
  group_by(age_youngest) %>% 
  summarise(max = max(num_injured))
interactive <- plot_ly(data = safer_parks,
             x = ~age_youngest,
             y = ~num_injured,
             type = 'scatter',
             mode = 'markers',
             color = ~num_injured,
             text = ~injury_desc)
female <- safer_parks %>% 
  filter(gender == "F") %>% 
  filter(age_youngest >20) %>% 
  filter(num_injured <2) %>% 
  summarise(n = n())
male <- safer_parks %>%
  filter(gender == "M") %>% 
  group_by(device_category) %>% 
  summarise(max = max(num_injured))
state_F <- safer_parks %>% 
  filter(gender == "F") %>% 
  group_by(acc_state) %>% 
  summarise(sum = sum(num_injured))
safer_parks$age_youngest[is.na(safer_parks$age_youngest)] <- 0
safer_parks %>% 
  group_by(acc_state) %>% 
  summarise(max_age1 = max(age_youngest)) %>% 
  ggplot(aes(x = acc_state, y = max_age1, fill = acc_state))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90)) ##rotation of axis
safer_parks %>% 
  group_by(bus_type, industry_sector, fix_port) %>% 
  summarise(med = median(age_youngest)) %>% 
  ggplot(aes(x=bus_type, y=med,))+
  geom_point(aes(col=fix_port))+
  facet_grid(~industry_sector)+
  scale_x_discrete(guide = guide_axis(angle = 90))
