library(data.table)
library(bigmemory)
library(ff)
require(sqldf)
library(sparklyr)
library(tidyverse)

hero_powers <- read_csv("super_hero_powers.csv", 
                        na = c("","-", "NA")) 
hero_info   <- read_csv("heroes_information.csv", 
                        na = c("","-","NA"))
View(hero_powers)
View(hero_info)

glimpse(hero_powers)
glimpse(hero_info)

library(janitor)

hero_info   <- clean_names(hero_info)
hero_powers <- clean_names(hero_powers)

hero_info   <- hero_info %>% select(-x1)

## Q4

cbind(select(hero_powers,hero_names),
      mutate_all(select(hero_powers,-hero_names), as.logical))


add_column(select(hero_powers,hero_names),
           mutate_all(select(hero_powers,-hero_names), as.logical))

## Q5

levels(factor(hero_info$publisher))

hero_info %>% count(publisher) 
nome <- levels(factor(hero_info$publisher)) ; 
which(nome == "Marvel Comics") 
which(nome == "DC Comics") 
nome <- nome[-c(3,12)]

hero_info %>% mutate(publisher = fct_collapse(publisher,
                     Marvel = "Marvel Comics",
                     DC     = "DC Comics",
                     Outros = nome))

hero_info1 <- hero_info %>% mutate(publisher = case_when(publisher == "Marvel Comics" ~ "Marvel",
                                           publisher == "DC Comics" ~ "DC",
                                           publisher != c("Marvel Comics", "Marvel") ~ "Outros"))

hero_info1 %>% count(publisher)

## Q6

hero_info %>% filter(publisher == "DC Comics") %>% count(race)

## Q7

max(table(hero_info$eye_color[hero_info$gender == "Male"]))
max(table(hero_info$eye_color[hero_info$gender == "Female"]))

hero_info %>% filter(gender == "Male") %>% count(eye_color) # Ver slice

# group_by realiza uma funcao por grupo que você selecionar
hero_info %>% group_by(gender) %>% count(eye_color) %>% na.omit()
# Ver fct_infreq

# Q8

hero_powers %>% summarise_if(is.logical, mean, na.rm = TRUE)

# Q9

hero_powers %>% 
  gather(poder, possui_poder, -hero_names) %>% 
  group_by(poder) %>% summarise(media = mean(possui_poder))

# Q10

(hero  <- inner_join(hero_info, hero_powers, by = c("name" = "hero_names")))
View(hero)

# Q11
hero %>% group_by(publisher) %>% summarise(media = mean(telepathy))

# Q12
hero %>% select(name, publisher, flight, weight) %>%  
          filter(flight == TRUE) %>% 
            top_n(weight, n = 10)

# Q13
write_csv(x = hero, path = "herois_completo.csv")


