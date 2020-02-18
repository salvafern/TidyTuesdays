library(tidyverse)
library(threejs)

# Get the Data
food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Get total emissions
food_consumption <- food_consumption %>% group_by(country) %>% summarise(
  co2_total_emmission = sum(co2_emmission)
) %>% ungroup() %>% mutate(
  colors = case_when(
    co2_total_emmission < 500 ~ "green",
    co2_total_emmission >= 500 & co2_total_emmission < 1000 ~ "yellow",
    co2_total_emmission >= 1000 & co2_total_emmission < 1500 ~ "orange",
    co2_total_emmission >= 1500  ~ "red",
  ),
  size = case_when(
    colors == "green" ~ 50, 
    colors == "yellow" ~ 100, 
    colors == "orange" ~ 150, 
    colors == "red" ~ 200
  )
)

# Get centroids from world map
centroids <- rworldmap::getMap(resolution="low"
       ) %>% rgeos::gCentroid(byid = TRUE
       ) %>% as.data.frame(
       ) %>% rownames_to_column("country")

# Fix country names to match food_consumption
centroids <- centroids %>% mutate(
  country = case_when(
    country == 'United States of America' ~ 'USA',
    country == 'Hong Kong S.A.R.' ~ 'Hong Kong SAR. China',
    country == 'The Bahamas' ~ 'Bahamas',
    country == 'Taiwan' ~ 'Taiwan. ROC',
    country == 'United Republic of Tanzania' ~ 'Tanzania',
    country == 'Republic of Serbia' ~ 'Serbia',
    country == 'Republic of the Congo' ~ 'Congo',
    TRUE ~ country
  )
)

# Join centroids
food_consumption <- food_consumption %>% left_join(centroids, by = 'country')

# Plot total emissions on globe
globejs(
  lat = food_consumption$y, emissive = '#46514e', pointsize = 2,
  lon = food_consumption$x,
  value = food_consumption$size,
  atmosphere = TRUE,
  bg = "black",
  color = food_consumption$colors #randomcoloR::distinctColorPalette(nrow(food_consumption))
)

