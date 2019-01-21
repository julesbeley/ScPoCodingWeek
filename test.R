library(tidyverse)
library(dplyr)
rne <- read_csv("Repertoire-national-des-elus.csv")
mean(rne$Age[rne$Député == TRUE], na.rm = T)
#the pipe (ctrl+shift+m) to create pipelines
# functions for tidying data:
# filter(), slice(), arrange(), select(), rename(), distinct(), mutate(), group_by(), summarise(), sample_n / sample_frac()
summary(rne)
rne %>% 
  filter(`Code sexe` %in% "F") #only elected women %in% instead of == to get rid of NAs 
# pipe is particularly useful when using several filters:
rne_female <- rne %>% 
  # filter(`Code sexe` %in% "F") %>% 
  mutate(`Code sexe` = recode(`Code sexe`, "F" = "Female", "M" = "Male")) %>% 
#mutate because we want to modify a bariable, recode to recode it
  View()
