library(tidyverse)
library(dplyr)
rne <- read_csv("Repertoire-national-des-elus.csv")
mean(rne$Age[rne$Député  == TRUE], na.rm = T)
#the pipe (ctrl+shift+m) to create pipelines
# functions for tidying data:
# filter(), slice(), arrange(), select(), rename(), distinct(), mutate(), group_by(), summarise(), sample_n / sample_frac()
summary(rne)
rne %>%
  filter(`Code sexe` %in% "F") #only elected women %in% instead of == to get rid of NAs
# pipe is particularly useful when using several filters:
rne_female <- rne %>%
  filter(`Code sexe` %in% "F") %>%
  mutate(`Code sexe` = recode(`Code sexe`, "F" = "Female", "M" = "Male")) %>%
  #mutate because we want to modify a bariable, recode to recode it
  arrange(desc(`Date de naissance`)) %>% #desc for descending otherwise arrange(`Date de naissance`)
  # select(`Code sexe`, `Date de naissance`, `Libellé de la profession`) %>% #-VARIABLE if you want to remove
  group_by(`Libellé de la profession`) %>% #creating group doesn't change anything
  summarise(n = n()) %>%
  arrange(desc(n)) %>% #we want to summarise
  View()
