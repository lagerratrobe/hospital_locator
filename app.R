library(shiny)
library(leaflet)
library(dplyr)
source("utils.R")

# Read in the dataset of hospitals
hosp_data <- getData()


# Pull in an icon to use as map marker for hospital locations 
hospIcon <- makeIcon(
  iconUrl = "https://icons.iconarchive.com/icons/fa-team/fontawesome/48/FontAwesome-House-Medical-icon.png",
  iconWidth = 16, iconHeight = 16
)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel( h1("Local Hospitals", align = "left"),
              windowTitle = "Local Hospitals"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Street input box
      textInput(inputId = "street",
                label = h4("Street Number"),
                value = "7717 30th Ave SW",
                placeholder = "7717 30th Ave SW",
                width = '50%'),
      # City input box
      textInput(inputId = "city",
                label = h4("City"),
                value = "Seattle",
                placeholder = "Seattle",
                width = '50%'),
      # State input box
      textInput(inputId = "state",
                label = h4("State"),
                value = "WA",
                placeholder = "WA",
                width = '50%'),
      # Submit button
      submitButton("Submit", icon("refresh"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Leaflet Map centered on input coords
      leafletOutput("leafletMap")
    )
  )
  
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Get the coordinates from imput boxes
  coords <- reactive({
    getGeocodeCoords(street = input$street , city = input$city, state = input$state)
  })
  
  latitude <- reactive({coords()[2]})
  longitude <- reactive({coords()[1]})
  
  # Do the distance calcs and get closest 5
  closest_five <- reactive({
    closestFive(latitude(), longitude(), hosp_data)
  })
  
  bbox <- reactive({
    st_bbox(closest_five()) |> as.vector()
  })
  
  # Define what the map looks like and has in it  
  output$leafletMap <- renderLeaflet({
    leaflet() |>
      fitBounds(bbox()[1], bbox()[2], bbox()[3], bbox()[4],
                options = list(padding = c(10,10))) |>
      addTiles() |>
      addMarkers( data = closest_five(),
                 icon = hospIcon,
                 popup = ~NAME, 
                 label = ~NAME ) |> 
      addMarkers( lng = as.numeric(longitude()), 
                  lat = as.numeric(latitude()) ) |>
      addProviderTiles("CartoDB.Positron")
  })
  
}


# Create Shiny app ----
shinyApp(ui=ui, server=server)  
