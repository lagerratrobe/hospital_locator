# base_code.R

# R code outside of Shiny framework for testing

library(shiny)
library(leaflet)
library(dplyr)
source("utils.R")

hosp_data <- readRDS("usa_hospitals.RDS")
hosp_data <- (st_as_sf(hosp_data, coords = c("LONGITUDE", "LATITUDE"),  crs = 4326))

coords <- getGeocodeCoords(street = "7757 30th Ave SW" , 
                           city = "Seattle",
                           state = "WA")

closest_five <- closestFive(coords$y, coords$x, hosp_data)
View(closest_five)

library(data.table)
new_data <- fread("hospitals_hifld_data.csv", colClasses=c("ID" = "character", "ZIP"="character"))
new_hosp_data <- new_data |>
  select(ID,NAME,ADDRESS,CITY,STATE,COUNTY,ZIP,TYPE,STATUS,BEDS,TRAUMA,HELIPAD,LATITUDE,LONGITUDE)
glimpse(new_hosp_data)

filter(hosp_data, grepl("BREMERTON", CITY)) |> glimpse()
filter(new_hosp_data, grepl("BREMERTON", CITY)) |> glimpse()
