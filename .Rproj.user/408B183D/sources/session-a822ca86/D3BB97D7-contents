# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(lwgeom)
library(scales)
library(ggspatial)

# Cargar shapefiles de ventas
geocoded_old_data_sf <- st_read("geocoded_old_data_shapefile.shp")

geocoded_new_data_sf <- st_read("geocoded_new_data_shapefile.shp")

# Cargar el shapefile de las manzanas
blocks <- st_read("data/Detroit_Census_Blocks,_2010-shp/37c46bdd-5913-4eb2-aa0d-081e0d8195322020330-1-1jg9q6w.egvs.shp")

# Verificar la validez de las geometrías
validity_check <- st_is_valid(blocks)
table(validity_check)

# Reparar las geometrías no válidas
blocks <- st_make_valid(blocks)


# Verificar nuevamente la validez
validity_check <- st_is_valid(blocks)
table(validity_check)

# Verificar la proyección (CRS) y transformarla si es necesario
if (st_crs(blocks) != st_crs(geocoded_old_data_sf)) {
  blocks <- st_transform(blocks, st_crs(geocoded_old_data_sf))
}

# Encontrar las manzanas donde los puntos están dentro
# Old
old_points_with_blocks <- st_join(geocoded_old_data_sf, blocks, join = st_within)

#New
new_points_with_blocks <- st_join(geocoded_new_data_sf, blocks, join = st_within)


# Determinar precio promedio y número de valores para cada manzana
# Old
old_blocks_stats <- old_points_with_blocks %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price))) %>%
  group_by(GEOID10) %>%
  reframe(n = n(), mean_price = mean(Price))

# New
new_blocks_stats <- new_points_with_blocks %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price))) %>%
  group_by(GEOID10) %>%
  reframe(n = n(), mean_price = mean(Price))

# Unir estadísticos de manzanas a los shapefiles
# Old
old_blocks <- left_join(blocks, old_blocks_stats)
st_write(old_blocks, "old_blocks_stats.shp")

# New
new_blocks <- left_join(blocks, new_blocks_stats)
st_write(new_blocks, "new_blocks_stats.shp")


# Difference
blocks_diff <- st_join(new_blocks, old_blocks, suffix = c("_new", "_old")) %>%
  filter(!is.na(mean_price_new) & !is.na(mean_price_old)) %>%
  mutate(mean_price_diff = mean_price_new - mean_price_old)

blocks_diff %>%
  select(GEOID10_new, mean_price_diff) %>%
  st_write("diff_blocks_stats.shp")
