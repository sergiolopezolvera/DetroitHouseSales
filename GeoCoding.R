# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(lwgeom)
library(scales)
library(ggspatial)

# Set your Google API key
api_key <- "AIzaSyD1NBj-92c2IqiBeXrY1lRI7EEWxUTdbJc"


geocode_address <- function(address, api_key) {
  # Create the request URL
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json"
  query <- list(
    address = address,
    key = api_key
  )
  
  # Perform the request
  response <- GET(url = base_url, query = query)
  
  # Parse the JSON as raw text
  response_text <- content(response, "text", encoding = "UTF-8")
  content <- fromJSON(response_text, simplifyVector = FALSE)
  
  # Check if results are available
  if (!is.null(content$results) && length(content$results) > 0) {
    # Extract latitude and longitude
    lat <- content$results[[1]]$geometry$location$lat
    lng <- content$results[[1]]$geometry$location$lng
    
    # Return the coordinates
    return(c(lat, lng))
  } else {
    # No results found
    print(paste("No results for address:", address))
    return(c(NA, NA))
  }
}


# Load old data
old_data <- read_csv("data/2015_2017_Sales.csv")

# Create a new column for the full address
old_data <- old_data %>%
  mutate(
    full_address = paste(Address, City, County, sep = ", ")
  )


# Geocode addresses in old data
geocoded_old_data <- old_data %>%
  rowwise() %>%
  mutate(
    geocode = list(geocode_address(full_address, api_key)),
    latitude = geocode[[1]],
    longitude = geocode[[2]]
  ) %>%
  ungroup() %>%
  select(-geocode) # Remover la lista intermedia

# Save the geocoded old data
write_csv(geocoded_old_data, "geocoded_old_data.csv")


# Load new data
new_data <- read_csv("data/2022_Present_Sales.csv")

# Create a new column for the full address
new_data <- new_data %>%
  mutate(
    full_address = paste(Address, City, County, sep = ", ")
  )

# Geocode addresses in new data
geocoded_new_data <- new_data %>%
  rowwise() %>%
  mutate(
    geocode = list(geocode_address(full_address, api_key)),
    latitude = geocode[[1]],
    longitude = geocode[[2]]
  ) %>%
  ungroup() %>%
  select(-geocode) # Remove intermediate list column

# Save the geocoded new data
write_csv(geocoded_new_data, "geocoded_new_data.csv")

# Confirm completion
print("Geocoding completed for both datasets and files saved.")

# Convertir geocoded_old_data a un objeto espacial sf
geocoded_old_data_sf <- st_as_sf(
  geocoded_old_data,
  coords = c("longitude", "latitude"), # Especificar las columnas de coordenadas
  crs = 4326                          # Sistema de referencia espacial (WGS84)
)

geocoded_old_data_sf <- geocoded_old_data_sf %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price)))

# Convertir geocoded_new_data a un objeto espacial sf
geocoded_new_data_sf <- st_as_sf(
  geocoded_new_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

geocoded_new_data_sf <- geocoded_new_data_sf %>%
  mutate(Price = as.numeric(gsub("[$,]", "", Price)))

# Verificar los objetos espaciales
head(geocoded_old_data_sf)
head(geocoded_new_data_sf)

# Exportar geocoded_old_data_sf a un shapefile
st_write(geocoded_old_data_sf, "geocoded_old_data_shapefile.shp")

# Exportar geocoded_new_data_sf a un shapefile
st_write(geocoded_new_data_sf, "geocoded_new_data_shapefile.shp")
