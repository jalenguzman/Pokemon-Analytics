library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plyr)
#library(tools/basic helper)

Pokemon <- read.csv('pokemons_basic.csv')
head(Pokemon)

#Checking our data
Pokemon %>%
  filter((hp + atk + def + spatk + spdef + speed) != total) %>%
  select(name)

Pokemon %>%
  select(name) %>% distinct() %>% nrow()

Pokemon %>%
  filter(type1 == type2) %>% select(name, type1, type2)

Pokemon %>%
  select(rank) %>% table()

str_count(Pokemon$abilities, ' ') %>% max()

#Data Cleaning
Pokemon <- Pokemon %>%
  #for pokemon without 2nd typing
  mutate(type2 = ifelse(is.na(type2), 'None', type2)) %>%
  
  #clean string columns cases
  mutate(name = str_to_title(name),
         type1 = str_to_title(type1),
         type2 = str_to_title(type2),
         evolves_from = str_to_title(evolves_from),
         desc = str_to_sentence(desc)) %>%
  
  #evo type
  
  separate(abilities, c('ability1', 'ability2', 'ability3'),  sep = ' ', fill = "right")

#creates effectivness matrix based off typings
effectivness_matrix <- create_effectivness_matrix()

#best types by type advantages
rowSums(effectivness_matrix)
colSums(effectivness_matrix)

#what is typically the highest/lowest stats?
highest_stat_counter(data = Pokemon)

#we can also do this with different typings
highest_stat_counter(data = Pokemon, type = c('Dark'))

#apply effectivness matrix to pokemon
Pokemon <- Pokemon %>%
  #stab attack advantages
  mutate(atk_eff = get_attack_effectivness(effectivness_matrix, type1, type2),
         #defense disadvantages
         def_eff = get_defend_effectivness(effectivness_matrix, type1, type2),
         #aggregated
         eff_total = atk_eff + def_eff)

#Data Visualization
#what are the most/least common typings?
table(Pokemon$type1, Pokemon$type2) %>%
  as.data.frame() %>%
  rename(type1 = Var1, type2 = Var2, frequency = Freq) %>%
  ggplot(aes(x = type1, y = type2, fill = frequency)) +
  geom_tile() +
  geom_text(aes(label = frequency), size = 8) +
  labs(x = 'Type 1', y = 'Type 2', title = 'Pokemon Typing Combinations') +
  scale_fill_gradient(low = 'white', high = 'red') +
  theme(text = element_text(size = 25))

#by bar chart
data.frame(typing = c(Pokemon$type1, Pokemon$type2), count = 1) %>%
  filter(typing != 'None') %>%
  ggplot(aes(x = reorder(typing, -count), y = count)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = count), vjust = 1.6, color = "white", size = 8)

#pokemon with top atk eff
#pokemon with top def eff
#pokemon with top general eff
  
