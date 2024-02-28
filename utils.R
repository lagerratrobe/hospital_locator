# utils.R

library(sf)
library(dplyr)

# Utility functions for R Shiny Hospital Locator app

getData <- function() {
  df <- readRDS(
    url("https://github.com/lagerratrobe/nearest_neighbor/raw/main/Data/usa_hospitals.RDS")) |>  
    filter(STATUS == "OPEN") |>
    filter(!TRAUMA == "NOT AVAILABLE") |>
    filter(!TYPE == "REHABILITATION") 
  
  return(st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"),  crs = 4326))
}

closestFive <- function(
    latitude,
    longitude,
    hosp_data) {
  home_point <- st_sfc(st_point(c(as.numeric(longitude), as.numeric(latitude))), crs=4326)
  # Takes in sf features and returns ID and distance of 5 closest hospitals to home_point
  hosp_data$distance <- st_distance(home_point, hosp_data)[1,]
  units(hosp_data$distance) <- NULL
  hosp_data$dist_miles <- hosp_data$distance / 1609.344 # convert to miles
  
  # Pull the 5 closest hospitals out
  closest_five <- hosp_data |>
    arrange(distance) |>
    slice_head(n=5) |>
    select(-distance)
  
  return(closest_five)
}


getGeocodeCoords <- function (
    street = NULL,
    city = NULL,
    state = NULL
) {
  library(httr)
  library(stringr)
  
  baseURL <- "https://geocoding.geo.census.gov/geocoder/locations/address?"
  
  # Function to replace spaces with "+" symbol
  spaceReplace <- function(df) {stringr::str_replace_all(df, "[ ]", "+")}
  
  # Build the request string
  request <- spaceReplace(paste0("street=", street, 
                                 "&city=", city, 
                                 "&state=", state, 
                                 "&benchmark=2020", 
                                 "&format=json"))
  
  # Tack the base address and request string together 
  requestURL <- paste0(baseURL, request)
  print(requestURL)
  
  # Make the actual request as a "GET" command
  response <- httr::GET(requestURL)
  
  # Status tells us if the request was successful, etc
  status <- httr::http_status(response)
  
  # Check that the request succeeded before proceeding further
  if (status$message == "Success: (200) OK") {
    # Parse the response
    response_data <- httr::content(response)
    
    # Extract the coordinates from the response
    coordinates <- response_data$result$addressMatches[[1]]$coordinates
    
    return(coordinates)
  } else {
    # Do something meaningful 
    sprintf("There was an error getting this request: \n", requestURL )
  }
}
