library(tidyverse)
rne <- read_csv("Repertoire-national-des-elus.csv")
mean(rne$Age[rne$Député == TRUE], na.rm = T)
#the pipe (ctrl+shift+m) to create pipelines
# functions for tidying data:
# filter(), slice(), arrange(), select(), rename(), distinct(), mutate(), group_by(), summarise(), sample_n / sample_frac()
  