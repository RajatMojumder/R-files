install.packages("gapminder")
library(gapminder)
library(tidyverse)
data("gapminder_unfiltered")
gapminder %>% filter(country=="Bangladesh") %>% head(10)
gapminder %>% filter(country=="Oman" & year>1980 & year<=2000) %>% head()

data("starwars")
View(starwars)

starwars %>% filter(height>150 & mass<200) %>% 
  mutate(height_in_meters=height/100) %>% 
  select(height_in_meters,mass) %>% 
  arrange(mass) %>% 
  View()
plot()

starwars %>% 
  select(name,height,mass)
starwars %>% select(1:3)
mean(starwars$height,na.rm=T)
library(ggplot2)

data("starwars")
#How can I check missing value without looking data?
is.na(starwars)

starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x=height))+
  geom_histogram()
#venture capital

starwars %>% 
  drop_na(height) %>% 
  ggplot(mapping = aes(x=height))+
  geom_boxplot(fill="steelblue")+
  theme_bw()+labs(title = "Boxplot of Height",x="Height of Characters")

starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c("male","female")) %>% 
  ggplot(mapping = aes(x=height, color=sex, fill=sex))+
  geom_density(alpha=0.2)+
  theme_bw()+
  labs(title = "Boxplot of Height",x="Height of Characters")


