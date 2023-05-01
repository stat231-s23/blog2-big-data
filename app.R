library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(sf)
library(leaflet.extras)

# Load data
visual1data <- data %>%
  pivot_longer(location, names_to = "name", values_to = "location") %>% 
  group_by(location, geometry) %>%
  dplyr::summarize(N = n()) %>%
  st_as_sf() %>% 
  st_zm(drop = T, what = "ZM")

# Define color palette

colors <- colorBin("viridis", visual1data$N, 6, pretty = TRUE)

# Define UI
ui <- fluidPage(
  
  title = "ACEMS Response Cases at Amherst College",
  
  tabPanel(
    
    title = "Map",
    
    sidebarLayout(
      sidebarPanel(),
      mainPanel(
        leafletOutput(outputId = "map")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet(data = visual1data) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~colors(N)[as.numeric(factor(N))],
        stroke = FALSE,
        fillOpacity = 0.5,
        layerId = ~location
      )
  })
  
  # Display data for clicked polygon
  observeEvent(input$map_shape_click, {
    shape_id <- input$map_shape_click$id
    clicked_data <- visual1data %>% filter(location == shape_id)
    if(nrow(clicked_data) > 0) {
      coords <- st_coordinates(clicked_data$geometry)
      leafletProxy("map", data = visual1data) %>% 
        clearPopups() %>% 
        addPopups(
          coords[1, 1], coords[1, 2],
          ~paste0("Location: ", clicked_data$location, "<br>",
                  "Number of cases: ", clicked_data$N),
          options = popupOptions(closeButton = TRUE)
        )
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)
