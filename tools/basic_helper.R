library(dplyr)

#returns string of all pokemon types
poke_type_names <- function(){
  types <- c("Normal", "Fire", "Water", "Grass", "Electric", "Ice", "Fighting", "Poison", "Ground", 
             "Flying", "Psychic", "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy", "None")
  return(types)
}

#returns string of colors corresponding to poke_types
poke_type_colors <- function(){
  colors <- c("#a9b923", "#745444", "#897aec", "#f8c833", "#eb9beb", "#b95242", 
              "#fb4324", "#8a9afb", "#6262b9", "#74cc54", "#dbba53", "#63cbfb", 
              "#abac9b", "#a95399", "#fa539a", "#bcac63", "#aaaaba", "#3398f8")
  return(colors)
}

poke_types <- function(){
  return(cbind(types, colors))
}

#creates an effectiveness matrix
create_effectivness_matrix <- function(){
  #pokemon types
  types <- poke_type_names()
  #None added for pokemon with no 2nd typing
  
  #creating empty dataframe for type weaknesses and strengths
  effectiveness_matrix <- data.frame(matrix(ncol = length(types), nrow = length(types)))
  colnames(effectiveness_matrix) <- types #type of the defender
  rownames(effectiveness_matrix) <- types #type of the attacker
  
  #assigning values to type combinations
  effectivness_matrix <- fill_effectivness_matrix(effectivness_matrix)
  
  return(effectiveness_matrix)  
}

#fills effectiveness matrix with values needed
#not very neat or 'programmatic' but - oops
fill_effectivness_matrix <- function(effectivness_matrix){
  #super effective
  #reads as: Fire is super effective against Grass, Ice, Bug, and Steel
  effectiveness_matrix["Fire", c("Grass", "Ice", "Bug", "Steel")] <- 2
  effectiveness_matrix["Water", c("Fire", "Ground", "Rock")] <- 2
  effectiveness_matrix["Grass", c("Water", "Ground", "Rock")] <- 2
  effectiveness_matrix["Electric", c("Water", "Flying")] <- 2
  effectiveness_matrix["Ice", c("Grass", "Ground", "Flying", "Dragon")] <- 2
  effectiveness_matrix["Fighting", c("Normal", "Ice", "Rock", "Dark", "Steel")] <- 2
  effectiveness_matrix["Poison", c("Grass", "Fairy")] <- 2
  effectiveness_matrix["Ground", c("Fire", "Electric", "Poison", "Rock", "Steel")] <- 2
  effectiveness_matrix["Flying", c("Grass", "Fighting", "Bug")] <- 2
  effectiveness_matrix["Psychic", c("Fighting", "Poison")] <- 2
  effectiveness_matrix["Bug", c("Grass", "Psychic", "Dark")] <- 2
  effectiveness_matrix["Rock", c("Fire", "Ice", "Flying", "Bug")] <- 2
  effectiveness_matrix["Ghost", c("Psychic", "Ghost")] <- 2
  effectiveness_matrix["Dragon", c("Dragon")] <- 2
  effectiveness_matrix["Dark", c("Psychic", "Ghost")] <- 2
  effectiveness_matrix["Steel", c("Ice", "Rock", "Fairy")] <- 2
  
  #not very effective
  #reads as: Fire is not very effective against Fire, Water, Rock, and Dragon
  effectiveness_matrix["Fire", c("Fire", "Water", "Rock", "Dragon")] <- 0.5
  effectiveness_matrix["Water", c("Water", "Grass", "Electric", "Dragon")] <- 0.5
  effectiveness_matrix["Grass", c("Fire", "Grass", "Poison", "Flying", "Bug", "Dragon", "Steel")] <- 0.5
  effectiveness_matrix["Electric", c("Electric", "Steel")] <- 0.5
  effectiveness_matrix["Fighting", c("Poison", "Flying", "Psychic", "Bug", "Fairy")] <- 0.5
  effectiveness_matrix["Ground", c("Grass", "Ice", "Water", "Electric")] <- 0.5
  effectiveness_matrix["Psychic", c("Psychic", "Steel")] <- 0.5
  effectiveness_matrix["Bug", c("Fire", "Fighting", "Poison", "Flying", "Ghost", "Steel", "Fairy")] <- 0.5
  effectiveness_matrix["Rock", c("Fighting", "Ground", "Steel")] <- 0.5
  effectiveness_matrix["Dragon", c("Steel")] <- 0.5
  effectiveness_matrix["Dark", c("Fighting", "Dark", "Fairy")] <- 0.5
  effectiveness_matrix["Steel", c("Fire", "Water", "Electric", "Steel")] <- 0.5
  effectiveness_matrix["Fairy", c("Fire", "Poison", "Steel")] <- 0.5
  
  #ineffective
  #reads as: normal doesn't effect ghost
  effectiveness_matrix["Normal", c("Ghost")] <- 0
  effectiveness_matrix["Electric", c("Ground")] <- 0
  effectiveness_matrix["Fighting", c("Ghost")] <- 0
  effectiveness_matrix["Poison", c("Steel")] <- 0
  effectiveness_matrix["Ground", c("Flying")] <- 0
  effectiveness_matrix["Psychic", c("Dark")] <- 0
  effectiveness_matrix["Ghost", c("Normal")] <- 0
  effectiveness_matrix["Dragon", c("Fairy")] <- 0
  
  #all other neutral typing relationships
  effectiveness_matrix <- effectiveness_matrix %>%
    replace(., is.na(.), 1) #1 if the attack is neutral against defender
  
  return(effectivness_matrix)
}

#applies effectiveness matrix given typing of the pokemon in question
get_attack_effectivness() <- function(effectivness_matrix, type1, type2){
  effectivness_matrix %>% 
      filter(row.names(.) %in% c(type1, type2)) %>% #select only columns for given 
      rowSums() %>% #get stats for particular typing
      sum() -> atk_eff #sum typings together
  
  return(atk_eff * +1)
}

get_defend_effectivness() <- function(effectiveness_matrix, type1, type2){
  effectivness_matrix %>%
      select(c(type1, type2)) %>%
      colSums() %>%
      sum() -> def_eff
  
  return(def_eff * -1) #send as negative as to counterbalance atk_eff
}

highest_stat_counter <- function(data = data, type = type, stats = stats, gen = generation, evo = evo_type){
  #if there is a value for type
  
  #if there is a value for specific stats to compare
  
  #if there is a value for specific generation
  
  #if there is a value for specific evolution type
  data %>%
    select(hp, atk, def, spatk, spdef, speed) %>%
    rowwise() %>%
    mutate(HighestStat = colnames(.)[which.max(c_across(c(hp, atk, def, spatk, spdef, speed)))]) %>%
    count(HighestStat) %>%
    arrange(-n)
}

#indicator for if something is a basic evolution, mid evolution, or final evolution
get_evo_type <- function(){
  
}

#return current ability descriptions if given
get_ability_desc <- function(ability_name){
  
}
