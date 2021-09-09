library(dplyr)
library(here)
library(tidyr)
library(forcats)
library(ggplot2)
data <- read.csv("nyc_squirrels.csv", sep = ",", stringsAsFactors = FALSE)
str(data)
ncol(data)
nrow(data)
AM <- data %>% 
  filter(shift == "AM") %>% 
  summarise(AM = n())
nrow(AM)
cinnamon <- data %>% 
  filter(primary_fur_color == "Cinnamon" & location == "Above Ground" ) %>% 
  summarise(cinnamon = n())
age <- data %>% 
  group_by(age) %>% 
  summarise(groups = n())
true <- data %>% 
  filter(primary_fur_color == "Gray") %>% 
  filter(climbing == "TRUE" & eating == "TRUE" & running == "TRUE")
activities <- data %>% 
  select(X, running:foraging) 
longer <- activities %>% 
  pivot_longer(cols = "running":"foraging", names_to = "Activity", values_to = "Yes")
longer <- longer %>% 
  filter(Yes == "TRUE")
longer %>%
  group_by(Activity) %>%
  summarise(nr = n()) %>%
  ggplot(., aes(x = Activity, y = nr, col = Activity, fill = Activity))+
  geom_bar(stat = "identity")
approaches <- boxplot(lat ~ approaches, data = data)
plot(data$long, data$lat, col = data$runs_from) #runs more in bigger latitude
ggplot(data, aes(x=long, y=lat))+
  geom_point(aes(col=runs_from), size=3)+
  labs(title = "Scare of people",
       subtitle = "Finding Food",
       x = "longitude",
       y = "latitude")
ggplot(data, aes(x=long, y=lat))+
  geom_point(aes(col=combination_of_primary_and_highlight_color), size=2)+
  labs(title = "Color of fur")
color_of_fur <- data %>% 
  group_by(combination_of_primary_and_highlight_color) %>% 
  summarise(n = n())
shift_age <- data %>%
  group_by(age, shift) %>% 
  ggplot(., aes(x=long, y=lat)) +
  geom_point(aes(col = shift), size=3) +
  facet_grid(primary_fur_color ~ age)
sum <- data %>% 
  group_by(age, shift) %>% 
  summarise(n = n())
dataset <- data %>% 
  group_by(age, runs_from) %>% 
  summarise(n = n()) %>% 
  mutate(percent = (n / sum(n)) * 100) %>% 
  ggplot(., aes(x=age, y=percent))+
  geom_bar(aes(fill = runs_from), stat = "identity", position = "dodge")
  