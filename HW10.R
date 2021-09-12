library(here)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(readr)
lemurs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv")
str(lemurs)
ncol(lemurs)
nrow(lemurs)
sex <- lemurs %>% 
  group_by(sex) %>% 
  summarise(n = n())
lemurs %>% 
  group_by(sex, birth_month) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = birth_month, y = n, fill = sex))+
  geom_bar(stat = "identity", position = "dodge")
lemurs %>% 
  group_by(sex, age_category) %>% 
  ggplot(aes(x=age_category, y=weight_g))+
  geom_boxplot()+
  facet_grid(~ sex)
lemurs %>% 
  ggplot(., aes(x=birth_month, y=expected_gestation_d, fill = birth_month))+
  geom_bar(stat = "identity")+
  coord_flip()
taxon <- lemurs %>% 
  group_by(taxon) %>% 
  summarise(n = n())
lemurs %>% 
  ggplot(., aes(x=birth_type, y=dam_age_at_concep_y))+
  geom_point(aes(col = birth_type))
weight <- lemurs %>% 
  group_by(month_of_weight) %>% 
  ggplot(., aes(x = month_of_weight, y= weight_g))+
  geom_bar(stat = "identity")
lemurs %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  ggplot(., aes(x= name, y = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))
lemurs %>% 
  ggplot(., aes(x=weight_g, y=avg_daily_wt_change_g, col = concep_month))+
  geom_point()
interactive <- plot_ly(data = lemurs,
                       x = ~weight_g,
                       y = ~avg_daily_wt_change_g,
                       type = "scatter",
                       mode = "markers",
                       color = ~concep_month,
                       text = ~dlc_id ~name)
lemurs %>% 
  ggplot(., aes(x=n_known_offspring, y=weight_g, col=n_known_offspring))+
  geom_point()
dead <- lemurs %>% 
  group_by(days_before_death) %>% 
  summarise(n = n())
