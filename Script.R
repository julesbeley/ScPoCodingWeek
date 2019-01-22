library(tidyverse)
rne <- read_csv("Repertoire-national-des-elus.csv")
mean(rne$Age[rne$Député == TRUE], na.rm = T)

#the pipe (ctrl+shift+m) to create pipelines
# functions for tidying data:
# filter(), slice(), arrange(), select(), rename(), distinct(), mutate(), group_by(), summarise(), sample_n / sample_frac()

rne %>%
  filter(`Code sexe` %in% "F") # only elected women %in% instead of == to get rid of NAs
# pipe is particularly useful when using several filters:
rne_female <- rne %>%
  filter(`Code sexe` %in% "F") %>%
  mutate(`Code sexe` = recode(`Code sexe`, "F" = "Female", "M" = "Male")) %>%
  # mutate because we want to modify a variable, recode to recode it
  arrange(desc(`Date de naissance`)) %>% # desc for descending otherwise arrange(`Date de naissance`)
  # select(`Code sexe`, `Date de naissance`, `Libellé de la profession`) %>% #-VARIABLE if you want to remove
  group_by(`Libellé de la profession`) %>% # creating group doesn't change anything
  summarise(n = n(), age = mean(Age)) %>% # number of rows AND average age per group
  arrange(desc(n)) %>% #we want to summarise
  View

# pivoting between long and large format (gather & spread)

rne %>%
  gather("office", "value", `Conseiller Municipal`:`Maire`) %>% # we get rows for offices not held (== FALSE)
  filter(`value` %in% TRUE) %>% # so we filter rows for which value == TRUE %>%
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01"))) %>%  # remove rows for which birth dates are "1900-01-01", using ymd function from lubridate package to convert string into date
  select(-`value`) %>% # remove 'value' column
  arrange(`Nom de l'élu`) %>% #
  group_by(`office`) %>%
  summarise(g = n(), age = mean(Age, na.rm = TRUE)) %>% # average age by type of office
  View

# average number of offices by occupation and by gender (already computed but as an example)

rne %>%
  gather("office", "value", `Conseiller Municipal`:`Maire`) %>% # we get rows for offices not held (== FALSE)
  filter(`value` %in% TRUE) %>% # so we filter rows for which value == TRUE %>%
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01"))) %>%  # remove rows for which birth dates are "1900-01-01", using ymd function from lubridate package to convert string into date
  select(-`value`) %>% # remove 'value' column
  group_by(`Identifiant`) %>% # to take care of multiple rows for same person
  summarise(
    offices = n(),
    occupation = unique(`Libellé de la profession`),
    gender = unique(`Code sexe`)
  ) %>% # unique function singles out unique values for a variable
  ungroup() %>% # remove grouping
  group_by(occupation, gender) %>% # regroup by other variables
  summarise(offices = mean(offices)) %>%
  arrange(desc(offices)) %>%
  View

# second youngest woman in each occupation

rne %>%
  filter(`Code sexe` %in% "F") %>% # only keep women
  group_by(`Libellé de la profession`) %>% # group by profession
  arrange(`Age`) %>% # order by age
  slice(2) %>% # get second youngest woman
  View

# transform numbers into written numbers using case_when()

rne %>%
  mutate(
    number = case_when(
      `Nombre de mandats` %in% 1 ~ "one",
      `Nombre de mandats` %in% 2 ~ "two",
      `Nombre de mandats` %in% 3 ~ "three",
      `Nombre de mandats` %in% 4 ~ "four"
    )
  ) %>%
  View

# MERGING DATASETS: primary keys (unique identifier - each observation has one primary key);
# foreign key allows to merge - it relates to another dataset
# foreign key doesn't have to be unique
# primary key in one table is foreign key in another
# inner join: only keep observations in both datasets and all variables
# outer join: left join (left dataset wins, unique right observations disappear)
#             right join (right dataset wins, unique left observations disappear)
#             full join (keep all observations and variables, create NAs for every missing)
# semi join (keep values on the left for matching on the right)
# anti join (only keep values on the left which are not present on the right)
# if names of the keys are the same, no nead to specify them in by =

# BINDING
# bind_rows() / bind_columns()

rne_f <- rne %>%
  filter(`Code sexe` %in% "F") # filter females
rne_m <- rne %>%
  filter(`Code sexe` %in% "M") # filter males
rne_binded <- bind_rows(rne_f, rne_m) # bind rows

# avoiding binding columns, better to merge (order can be different from one variable to the next)
# fuzzy join in case of almost identical keys - define maximum distance for similarlity

library(fuzzyjoin)

# tips and tricks
# group_by(), summarize(), arrange() = count(sort = TRUE)

# VISUALIZATION: ggplot2 (grammar of graphics)
# elements: data, aesthetic mapping, layers, scales, coordinate system, small multiples, theme (only part that isn't data-driven)

# mapping distribution of women and men

library(ggplot2)
library(scales)
rne %>%
  ggplot(aes(x = `Code sexe`)) +
  geom_bar() + # default stat argument for geom_bar() is count, so we don't need to add anything
  scale_y_continuous(labels = scales::comma) # get rid of engineer notation

# distribution by profession

pdf(file = "./plot2.pdf", width = 7, height = 9) # create file to save plot as a pdf (vector format)
rne %>%
  mutate(gender = recode(`Code sexe`, "M" = "Male", "F" = "Female")) %>% 
  count(`Libellé de la profession`, gender, sort = TRUE) %>%
  filter(!is.na(`Libellé de la profession`)) %>% 
  ungroup %>% 
  arrange(gender, n) %>% 
  filter(n > 1000) %>% # get rid of all values for which n > 1000
  mutate(order = row_number()) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% # factor in order
  mutate(coord = if_else(n > 22000, n - 2000, n + 1000), # create new coordinate variable to put labels over the bars on the top
         color = if_else(n > 22000, "white", "black ")) %>%  # create new color variable for the text
  ggplot(aes(x = order, y = n)) +
  scale_fill_discrete(guide = FALSE) +
  geom_bar(stat = "identity", width = 0.8, fill = "darkblue") + # change width and color of bars
  scale_y_continuous(labels = scales::comma) + # get rid of engineer notation with comma from scales package
  coord_flip() + # flip y and x axis
  geom_text(aes(label = occupation, y = coord, colour = color), 
            hjust = "inward", vjust = "center", size = 2.3) + # add labels on the right with inward horizontal justification and text size 2.5
  scale_color_manual(values = c("darkblue", "white"), guide = FALSE) + # set up manual color scale with no guide (legend)
  xlab("") + # because coord_flip, xlab is actually for y axis label
  ylab("") +
  ylim(0, NA) +  
  scale_x_discrete(labels = NULL) + #remove labels
  theme(axis.ticks.y = element_blank()) + # remove ticks on y axis
  facet_wrap(facets = vars(gender), scales = "free_y") + # separate female and male
  labs(title = "Most elected officials are employees, farmers or retired",
       subtitle = "Number of elected officials in France in 2018 by occupation",
       caption = "Source: RNE, computation by Sciences Po students") +
  theme_minimal()
dev.off() # device off after writing file

# spatial data
# features are dots, lines or polygons
# two formats: vector and raster (vector is always scalable without loss because we have information about the geometry)

library(sf) # package for tidy spatial data
library(mapview) 
library(leaflet) # leaflet is a javascript package originally, used a lot on the web
toilets <- read_sf("./data/sanisettesparis2011.geojson") # .geojson is becoming a standard for spatial data
mapview(toilets) # to view the data quickly
toilets %>% 
  leaflet() %>% # to create interactive maps for the web
  addTiles() %>% # add background
  addCircleMarkers(radius = 2, color = "red")

# using tmap for elaborate maps, e.g. toilets and trees on street background with compass and scale

library(tmap)
streets <- read_sf("./data/voie.geojson") # streets of Paris
trees <- read_sf("./data/arbresremarquablesparis.geojson") # remarkable trees
streets %>% 
  tm_shape() + # all tmap functions start with tm_
  tm_lines(alpha = 0.2) +
tm_shape(toilets) +
  tm_dots(col = "red") +
tm_shape(trees) +
  tm_dots(col = "darkgreen") +
tm_scale_bar(position = c("left", "bottom")) +
tm_compass(position = c("left", "top"))

# overpass Turbo + tags on Openstreetmap wiki
# search for tags on Openstreetmap wiki, send request on overpass Turbo and download as .geojson

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.2/") #(JAVA_HOME PROBLEM)
library(OpenStreetMap) 
library(rJava)
devtools::install_github("mtennekes/tmaptools")
library(tmaptools)
churches <- read_sf("./data/churchesLyon.geojson")
basemap <- read_osm(churches, type = "stamen-toner") # create basemap with tmaptools package
tm_shape(basemap) +
  tm_rgb() +
tm_shape(churches) +
  tm_dots(shape = 3, col = "blue", size = 0.2) +
  tm_scale_bar(position = c("left", "bottom")) 

# showing trends in the data (e.g. heat maps)

churches <- st_transform(churches, crs = 2154) # re-project (crs is code for new projection)
pdf(file = "./plot3.pdf", width = 10, height = 10) # NULL?
churches_density <- smooth_map(churches, bandwidth = 0.5) # be careful to extract only points from overpass turbo (not points AND polygons), 
# i.e. comment out 'ways' lines in Overpass turbo with //, keep only nodes (some data is loss because some churches only exist as polygons, i.e. buildings, and not as nodes)
dev.off()

# APIs v. SDK (toolbox, much broader)
# using httr for example to geolocalize adresses

library(httr) # allows us to query APIs
library(sf)
url <- "https://api-adresse.data.gouv.fr/search" # point d'entrée pour le géocodage
query <- GET(url, query = list(q = "5 rue Rosa")) # GET function, query must be wrapped in a list and we need q for simple text research
status_code(query) # 200 means OK, cf status codes
geojson <- content(query, as = "text") # content function transforms the answer, ask not to parse
adresses <- read_sf(geojson) # and then we can transform it with read_sf
mapview::mapview(adresses) # view with mapview()

# zapier: way to connect APIs without using any code
# in R, it is not always necessary to interrogate APIs, many APIs are already integrated in specific packages
