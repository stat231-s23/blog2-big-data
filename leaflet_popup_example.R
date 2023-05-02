library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(leaflet.extras)

# Load data
data <- read_rds("data/wrangledData.rds")

visual1data <- data %>%
  pivot_longer(location, names_to = "name", values_to = "location") %>% 
  group_by(location, geometry) %>%
  dplyr::summarize(N = n()) %>%
  st_as_sf() %>% 
  st_zm(drop = T, what = "ZM") %>%
  arrange(N)

# Define color palette
colors <- colorBin("viridis", visual1data$N, 6, pretty = TRUE)

# create map
leaflet(data = visual1data) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~colors(N)[as.numeric(factor(N))],
    stroke = FALSE,
    fillOpacity = 0.5,
    layerId = ~location
    , popup = paste0("Location: ", str_to_title(visual1data$location), "<br>"
                     , "Number of cases: ", visual1data$N)
  ) %>%
  # add legend explaining fill color
  addLegend(pal = colors, 
            values = visual1data$N,
            position = "bottomright", 
            title = "ACEMS Response Cases at Amherst College")
