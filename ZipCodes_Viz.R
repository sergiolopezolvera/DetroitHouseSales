# Visualización
# Old zip
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = old_zip, linewidth = 0.05, aes(fill = mean_price)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price per Zip Code in Detroit",
    subtitle = "From 2015 to 2017",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/old_zip.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)

# old zip alpha
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = old_zip, linewidth = 0.02, aes(fill = mean_price, alpha = n)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1)) +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price and Number of Properties per Zip Code",
    subtitle = "From 2015 to 2017",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/old_zip_2.png",
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
  geom_sf(data = new_zip, linewidth = 0.05, aes(fill = mean_price)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1), guide = "none") +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price per Zip Code in Detroit",
    subtitle = "From 2015 to present date",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/new_zip.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)

# New blocks alpha
ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = new_zip, linewidth = 0.02, aes(fill = mean_price, alpha = n)) +
  scale_fill_distiller(
    palette = "Reds", 
    direction = 1, # Para que vaya de claro a oscuro
    na.value = "lightgrey", 
    name = "Mean Price",
    labels = label_currency(accuracy = 1)
  ) +
  scale_alpha(range = c(0.2, 1)) +              # Escala de transparencia para `alpha`
  labs(
    title = "Average Price and Number of Properties per Zip Code",
    subtitle = "From 2020 to present date",
    fill = "Mean Price",
    alpha = "Number of Sales"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))

ggsave(
  filename = "figs/new_zip_2.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)

# Difference
zip_diff <- st_join(new_zip, old_zip, suffix = c("_new", "_old")) %>%
  filter(!is.na(mean_price_new) & !is.na(mean_price_old)) %>%
  mutate(mean_price_diff = mean_price_new - mean_price_old)


ggplot() +
  # Add basemap from OpenStreetMap
  annotation_map_tile(
    type = "cartolight", # Basemap type: "osm", "cartolight", "cartodark", etc.
    zoom = 12     # Adjust zoom level for your area
  ) +
  geom_sf(data = zip_diff, linewidth = 0.05, aes(fill = mean_price_diff)) +
  scale_fill_gradient2(
    low = "blue",      # Color para valores negativos
    mid = "white",     # Color para el valor 0
    high = "red",      # Color para valores positivos
    midpoint = 0,      # Punto central de la escala
    name = "Difference",
    labels = label_currency(accuracy = 1)
  ) +
  labs(
    title = "Difference in Average Price per Zip Code in Detroit",
    subtitle = "2020 to present date vs 2015 to 2017",
    fill = "Difference"
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = "white",
                                       color = NA))


ggsave(
  filename = "figs/Difference_zip.png",
  dpi = 300,           # Resolución en DPI
  width = 10,          # Ancho en pulgadas
  height = 8           # Alto en pulgadas
)
