library(shiny)
library(tidyverse)
library(plotly)
source("data_processing.R")

anti_by_boro <- read_csv("../data/anti-by-boro.csv")
row.names(anti_by_boro) <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(

          selectInput("count", 
                      label = "Antibody by Borough in Bar Plot",
                      choices = c("Number of Positive People" = 1,
                                     "Number of People Tested" = 2,  
                                     "Percentage of Positive Cases" = 3,
                                     "Test Rate" = 4), 
                      selected = "Number of Positive People")),
          mainPanel(
            plotlyOutput("bar_plt")

          )),
  
  tabPanel("Map Test",
           leafletOutput("mymap"),
           
           absolutePanel(id= "controls",
                         draggable = FALSE, height = "auto",
                         h3("NYC MAP", align = "left"),
                         
                         h4("NYC Parks", align = "left"),
                         checkboxInput("adultexerciseequip", label = "Exercise Equipments", value = FALSE),
                         checkboxInput("playgrounds", label = "Playgrounds", value = FALSE),
                         checkboxInput("atheleticfac", label = "Atheletic Facilities", value = FALSE),
                         checkboxInput("dogruns", label = "Dog Runs", value = FALSE),
                         checkboxInput("skateparks", label = "Skate Parks", value = FALSE)
                         )
  )

)




server <- function(input, output){
  
  output$bar_plt <- renderPlotly({
    if (input$count == 1){
    plotly::plot_ly(anti_by_boro,
                    x= row.names(anti_by_boro),
                    y = ~ NUM_PEOP_POS,
                    type = 'bar')}
    else if (input$count == 2){
      plotly::plot_ly(anti_by_boro,
                      x= row.names(anti_by_boro),
                      y = ~ NUM_PEOP_TEST,
                      type = 'bar')}
    else if (input$count == 3){
      plotly::plot_ly(anti_by_boro,
                      x= row.names(anti_by_boro),
                      y = ~ PERCENT_POSITIVE,
                      type = 'bar')}
    else {
      plotly::plot_ly(anti_by_boro,
                      x= row.names(anti_by_boro),
                      y = ~ TEST_RATE,
                      type = 'bar')}
    
  })
  
  output$mymap <- renderLeaflet({ 
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender(
        "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
      ) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  
  FilterPoints <- function(ds) {
    result <- ds %>% dplyr::filter(borough == input$plot_borough)
    return(result)
  }

  AdultExerciseEquip_reactive <- reactive({FilterPoints(adult_exer_equip)})
  Playgrounds_reactive <- reactive({FilterPoints(playgrounds)})

  FilterPolygons <- function(geo) {
    result <- subset(geo, borough == input$plot_borough)
    return(result)
  }
  
  AthFacilities_reactive <- reactive({FilterPolygons(ath_facilities_geo)})
  DogRuns_reactive <- reactive({FilterPolygons(dog_runs_geo)})
  SkateParks_reactive <- reactive({FilterPolygons(skate_parks_geo)})

  observe({
    proxy <- leafletProxy("mymap", data = df)
    
    proxy %>% clearControls()
    
    
    # clear the map
    leafletProxy("mymap", data = df) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addProviderTiles("CartoDB.Voyager") %>%
      fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    
    # output data related to covid testing sites
    if (input$covid_test){
      leafletProxy("mymap", data = df) %>%
        clearShapes() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        addCircleMarkers(~lon, ~lat, radius=10,
                         color = ~color1(open_now),
                         label = paste(df$formatted_address, ', ', df$name),
                         popup = paste(
                           "<b>Name:</b>", df$name, "<br>",
                           "<b>Address:</b>", df$formatted_address, "<br>",
                           "<b>Rating:</b>", df$rating, "<br>",
                           "<b>Open now:</b>", df$open_now, "<br>")) %>%
        addLegend("bottomright",
                  pal = color1,
                  values = df$open_now,
                  title = "Status",
                  opacity = 1)
    }
    
    if (input$adultexerciseequip){
      leafletProxy("mymap", data = AdultExerciseEquip_reactive()) %>% 
        addCircleMarkers(~longitude, ~latitude, radius = 1,
                         color = "green",
                         label = ~name, popup = ~content)
    }
    
    if (input$playgrounds) {
      leafletProxy("mymap", data = Playgrounds_reactive()) %>%
        addCircleMarkers(~longitude, ~latitude, radius = 1,
                         color = "yellow",
                          label=~name, popup=~content)
    }
    
    if (input$dogruns) {
      leafletProxy("mymap",data=DogRuns_reactive()) %>% 
        addCircleMarkers(~longitude, ~latitude, 
                          radius=1, label=~name, popup=~content) %>%
        addPolygons(color="purple",stroke = TRUE, weight = 8,
                    highlight = highlightOptions(weight = 15,
                                                 color = "lightblue",
                                                 bringToFront = TRUE))
    }
    
    if (input$skateparks) {
      leafletProxy("mymap",data=SkateParks_reactive()) %>% 
        addCircleMarkers(~longitude, ~latitude, 
                          radius = 1, label=~name, popup=~content) %>%
        addPolygons(color="cornflower",stroke = TRUE, weight = 8,
                    highlight = highlightOptions(weight = 15,
                                                 color = "yellow",
                                                 bringToFront = TRUE))
    }
    
    if (input$atheleticfac) {
      leafletProxy("mymap",data=AthFacilities_reactive()) %>% 
        addAwesomeMarkers(~longitude, ~latitude, 
                          icon=atheleticfac_icons, label=~name, popup=~content) %>%
        addPolygons(color="yellow",stroke = TRUE, weight = 8,
                    highlight = highlightOptions(weight = 15,
                                                 color = "red",
                                                 bringToFront = TRUE))
    }
    
  })
}




#library(mapview)
#library(sf)
#register_google(key = "AIzaSyChyjtcHL2bjFPmOaxqXFcmEcSOJCzSmqo", write = TRUE)
#locations_sf <- st_as_sf(ath_facilities@data, coords = c("longitude", "latitude"), crs = 4326)
#mapview(locations_sf)


shinyApp(ui = ui, server = server)


