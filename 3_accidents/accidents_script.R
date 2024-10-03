library(here)
library(tidyverse)

accidents <- readRDS(here("3_accidents", "data", "accidents.rds"))

weekdays = accidents %>% 
  select(day_of_week) %>% 
  filter(day_of_week != "Saturday" & day_of_week != "Sunday")

weekend = accidents %>% 
  select(day_of_week) %>% 
  filter(day_of_week == "Saturday" | day_of_week == "Sunday")

a <- accidents %>% 
  mutate(end_or_day = day_of_week) %>% 
  mutate(end_or_day = str_replace(end_or_day, "Saturday", "Weekend")) %>% 
  mutate(end_or_day = str_replace(end_or_day, "Sunday", "Weekend")) %>% 
  ggplot(aes(x= time, colour = severity))+
  geom_density()+
  facet_wrap(~"Weekend")

accidents %>% 
  mutate(day_of_week = str_replace(day_of_week, "Saturday", "Weekend")) %>%
  mutate(day_of_week = str_replace(day_of_week, "Sunday", "Weekend")) %>%
  ggplot(aes(x= time, colour = severity)) +
  geom_density() +
  facet_wrap(~ day_of_week == "Weekend", nrow = 2) +
  labs()
  
accidents %>% 
  mutate(day_of_week = str_replace(day_of_week, "Saturday", "Weekend")) %>%
  mutate(day_of_week = str_replace(day_of_week, "Sunday", "Weekend")) %>%
  mutate(day_of_week = if_else(day_of_week == "Weekend", "Weekend", "Weekday")) %>% 
  ggplot(aes(x= time, colour = severity, fill = severity)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ day_of_week, nrow = 2) +
  labs()




