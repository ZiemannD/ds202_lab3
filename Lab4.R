library(readxl)
library(readr)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages('readr')
install.packages('readxl')
defense <- read_excel('cyclonesFootball2020.xlsx', sheet='Defensive')
offense <- read_excel('cyclonesFootball2020.xlsx', sheet='Offensive')
biography <- read_excel('cyclonesFootball2020.xlsx', sheet='Biography')
str(offense)
str(defense)
str(biography)
#Part 1
#Question 1
offense %>% 
  factor("Name", "Opponent_Opponent")
defense %>% 
  factor("Name", "Opponent_Opponent")
biography %>% 
  factor("Name")
#Question 2
offense1 <- offense %>%
  mutate(
    across(everything(), ~replace_na(0)))

defense1 <- defense %>% 
  mutate(
    across(everything(), ~replace_na(0)))

biography <- biography %>% 
  mutate(
    across(everything(), ~replace_na(0)))
offense1 <- offense %>%
  dplyr::mutate(across(Receiving_REC:Passing_INT, as.numeric, 
                       ~replace_na(0)))
str(offClean)
#Question 3
biography$Height <- as.numeric(biography$Height)
biography$Weight <- as.numeric(biography$Weight)
str(defense1)
#Question 4
defense1 %>% 
 group_by(Name, Opponent_Opponent) %>% 
  mutate(Game = row_number())
offense1 %>% 
  group_by(Name, Opponent_Opponent) %>% 
  mutate(Game = row_number())
str(defense1)
defClean <- defense1
offClean <- offense1
bioClean <- biography
str(offClean)
str(defClean)
str(bioClean)
#Part 2
#Question 1
offClean <- pivot_longer(offClean, Receiving_REC:Passing_INT, 
             names_to = 'stat', values_to = 'score')
#Question 2
statistics <- offClean %>% 
  group_by(Name) %>% 
  summarize(
    Sum = n(),
    once = (Sum==1)) %>% 
  select(-once)
statistics
#Question 3
ggplot(offClean, aes(x= Name, y= score)) +
  geom_bar(stat='identity') +
  facet_wrap(~stat) + coord_flip()
#Question 4
off1 <- offClean %>% 
  filter(Opponent_Opponent == 'Oklahoma' && 
           Opponent_Opponent == 'Oregon') %>% 
  pivot_wider(names_from = 'Opponent_Opponent')
off1 %>% ggplot(aes(x=`Oregon`, y=`Oklahoma`)) + geom_point()
#Biography
#Question 5
str(bioClean)
biography1 <- bioClean %>% 
  separate(Hometown, c('City', 'State'), sep=',')
head(biography1$City)
head(biography1$State)
#Question 6
biography2 <- biography1 %>% group_by('State')
table(biography2$Name, biography2$State)
#Question 7
defPurdy <- defClean %>% 
  filter(Name == 'Purdy, Brock')
offPurdy <- offClean %>% 
  filter(Name == 'Purdy, Brock')
