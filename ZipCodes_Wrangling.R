# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(lwgeom)
library(scales)
library(ggspatial)

# Cargar el shapefile de los zip codes
zip <- st_read("data/City_of_Detroit_Zip_Code_Tabulation_Areas_2970852309938760228/zip_codes.shp")

# Cargar shapefiles de ventas
geocoded_old_data_sf <- st_read("geocoded_old_data_shapefile.shp")

geocoded_new_data_sf <- st_read("geocoded_new_data_shapefile.shp")

# Verificar la validez de las geometrías
validity_check <- st_is_valid(zip)
table(validity_check)

# Reparar las geometrías no válidas
zip <- st_make_valid(zip)

# Verificar nuevamente la validez
validity_check <- st_is_valid(zip)
table(validity_check)

# Verificar la proyección (CRS) y transformarla si es necesario
if (st_crs(zip) != st_crs(geocoded_old_data_sf)) {
  zip <- st_transform(zip, st_crs(geocoded_old_data_sf))
}

# Encontrar las manzanas donde los puntos están dentro
# Old
old_points_with_zip <- st_join(geocoded_old_data_sf, zip, join = st_within)

#New
new_points_with_zip <- st_join(geocoded_new_data_sf, zip, join = st_within)


# Determinar precio promedio y número de valores para cada manzana
# Old
old_zip_stats <- old_points_with_zip %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price))) %>%
  group_by(zipcode) %>%
  reframe(n = n(), mean_price = mean(Price))

# New
new_zip_stats <- new_points_with_zip %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price))) %>%
  group_by(zipcode) %>%
  reframe(n = n(), mean_price = mean(Price))

# Unir estadísticos de manzanas a los shapefiles
# Old
old_zip <- left_join(zip, old_zip_stats)
st_write(old_zip, "old_zip_stats.shp")

# New
new_zip <- left_join(zip, new_zip_stats)
st_write(new_blocks, "new_zip_stats.shp")


# Difference
zip_diff <- st_join(new_zip, old_zip, suffix = c("_new", "_old")) %>%
  filter(!is.na(mean_price_new) & !is.na(mean_price_old)) %>%
  mutate(mean_price_diff = mean_price_new - mean_price_old)

st_write(zip_diff, "diff_zip_stats.shp")

