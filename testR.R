library(here)
library(dplyr)
here()
data <- read.delim("mtcars.txt")
str(data)
ncol(data)
nrow(data)
data %>% 
  summarise(max = max(mpg))
data %>% 
  summarise(mean = mean(drat))
data <- data %>% 
  mutate(classification = case_when(
    gear == 3 ~ "low",
    gear == 4 ~ "medium",
    TRUE ~ "high"
  ))
class_fact <- factor(data$classification)
levels(class_fact)
nlevels(class_fact)
levels(class_fact) <- c(levels(class_fact), "zero")
class_fact <- factor(class_fact, levels = c("zero", "medium", "low", "high"))
data1 <- data %>% 
  mutate(class_fact = class_fact) %>% 
  group_by(class_fact) %>% 
  summarise(meanqsec = mean(qsec)) %>% 
  filter(class_fact == "high")
data2 <- data %>% 
  mutate(class_fact = class_fact) %>% 
  group_by(class_fact) %>% 
  summarise(meddrat = median(drat)) %>% 
  filter(class_fact == "low")
datacyl <- data %>% 
  summarise(sumcyl = sum(cyl >5))
datavs <- data %>% 
  filter(vs == 0)
datac <- data %>% 
  select(starts_with("c"))
