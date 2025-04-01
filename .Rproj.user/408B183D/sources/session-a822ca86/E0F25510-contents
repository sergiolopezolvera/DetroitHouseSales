# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(lwgeom)
library(scales)
library(ggspatial)
library(htmlwidgets)


# Visualización
# Old blocks
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = old_blocks, linewidth = 0.05, aes(fill = mean_price)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price per Block in Detroit",
    subtitle = "From 2015 to 2017",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/old_blocks.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)



# old blocks alpha
midpoint_value <- mean(old_blocks$mean_price, na.rm = TRUE)

ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = old_blocks, linewidth = 0.02, aes(fill = mean_price, alpha = n)) +
  scale_fill_gradient2(
    low = "green",
    mid = "yellow",
    high = "red",
    midpoint = midpoint_value, # Valor central ajustado
    na.value = "lightgrey",
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.5, 1)) +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price and Number of Properties per Block",
    subtitle = "From 2015 to 2017",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/old_blocks_2.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)

# New blocks
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = new_blocks, linewidth = 0.05, aes(fill = mean_price)) +
  scale_fill_distiller(
    palette = "Greens", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price of Single Family Home per Block in Detroit",
    subtitle = "From 2022 to Present",
    fill = "Mean Price"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/new_blocks.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)


# Interactive Leaflet
# Hover mouse and show data
library(leaflet)

interactive_map <- leaflet(data = new_blocks) %>%
  # Add OpenStreetMap basemap (Carto Light theme)
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Agregar el controlador de zoom en la parte inferior
  addControl(
    html = "<div></div>",  # Placeholder para el título
    position = "bottomright" # Cambiar posición a la parte inferior derecha
  ) %>%
  # Add polygons for the blocks
  addPolygons(
    color = "lightgrey",
    fillColor = ~ifelse(
      is.na(mean_price), 
      "lightgrey", # Color gris tenue para valores NA
      colorNumeric("Greens", domain = new_blocks$mean_price)(mean_price)
    ),
    fillOpacity = 0.7,      # Adjust transparency
    weight = 0.05,          # Line thickness
    popup = ~paste0(
      "<b>Mean Price:</b> $", format(mean_price, big.mark = ",", nsmall = 2), "<br>",
      "<b>Number of Sales:</b> ", format(n, big.mark = ",")
    ) # Customize popup content with block and mean price
  ) %>%
  # Add a legend
  addLegend(
    pal = colorNumeric("Greens", domain = new_blocks$mean_price),
    values = ~mean_price,
    title = "Mean Price",
    labFormat = labelFormat(prefix = "$", digits = 2),
    opacity = 0.7,
    position = "bottomright"
  ) %>%
  # Agregar título y subtítulo
  addControl(
    html = "<h2>Average Price of Single Family Homes</h2><h4>From 2022 to Present in Detroit</h4>",
    position = "bottomleft"
  )

# Guardar como archivo HTML
saveWidget(interactive_map, "interactive_map.html", selfcontained = TRUE)

# <iframe src="path/to/interactive_map.html" width="100%" height="600px"></iframe>



# New blocks alpha
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = new_blocks, linewidth = 0.02, aes(fill = mean_price, alpha = n)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1)) +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price and Number of Properties per Block",
    subtitle = "From 2020 to present date",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/new_blocks_2.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)

# Difference
blocks_diff <- st_join(new_blocks, old_blocks, suffix = c("_new", "_old")) %>%
  filter(!is.na(mean_price_new) & !is.na(mean_price_old)) %>%
  mutate(mean_price_diff = mean_price_new - mean_price_old)


ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = blocks_diff, linewidth = 0.05, aes(fill = mean_price_diff)) +
  scale_fill_gradient2(
    low = "blue",      # Color para valores negativos
    mid = "white",     # Color para el valor 0
    high = "red",      # Color para valores positivos
    midpoint = 0,      # Punto central de la escala
    name = "Differeence",
    labels = label_currency(accuracy = 1)
  ) +
  labs(
    title = "Difference in Average Price per Block in Detroit",
    subtitle = "2020 to present date vs 2015 to 2017",
    fill = "Difference"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))


ggsave(
  filename = "figs/Difference.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)
