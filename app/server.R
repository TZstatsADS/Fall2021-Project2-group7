if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("dplyr")) { install.packages("dplyr")}
library(dplyr)
if (!require("tidyverse")) { install.packages("tidyverse")}
library(tidyverse)
if (!require("DT")) { install.packages("DT")}
library(DT)
if (!require("ggplot2")) { install.packages("ggplot2")}
library(ggplot2)
if (!require("lubridate")) { install.packages("lubridate")}
library(lubridate)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if (!require("hrbrthemes")) { install.packages("hrbrthemes")}
library(hrbrthemes)
if (!require("highcharter")) { install.packages("highcharter")}
library(highcharter)
if (!require("RColorBrewer")) { install.packages("RColorBrewer")}
library(RColorBrewer)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if (!require("geojsonio")) { install.packages("geojsonio")}
library(geojsonio)
if (!require("readr")) { install.packages("readr")}
library(readr)
if (!require("leaflet")) { install.packages("leaflet")}
library(leaflet)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Tab1 Home (Newest Covid Cases Visualization) 
    data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
    nyc_latest <- data_by_day %>% tail(1)
    
    output$total <- renderValueBox({
        valueBox(
            h4("Cumulative Case Confirmed in NYC"),
            h3(sum(data_by_day$CASE_COUNT)),
            icon = icon("head-side-mask", lib = "font-awesome"),
            color = "aqua"
        )
    })
    
    output$death <- renderValueBox({
        valueBox(
            h4("Total Death Count in NYC"),
            h3(sum(data_by_day$DEATH_COUNT)),
            icon = icon("frown", lib = "font-awesome"),
            color = "aqua"
        )
    })
    
    output$hospital_case <- renderValueBox({
        valueBox(
            h4("Total Hospitalized Cases"),
            h3(sum(data_by_day$HOSPITALIZED_COUNT)),
            icon = icon("syringe", lib = "font-awesome"),
            color = "aqua"
        )
    })
    
    output$new_case <- renderValueBox({
        valueBox(
            h4("New Cases Confirmed On ", nyc_latest$date_of_interest ),
            h3(nyc_latest$CASE_COUNT[1]),
            icon = icon("user"),
            color = "olive"
        )
    })
    
    output$newH_case <- renderValueBox({
        valueBox(
            h4("New Hospitalized Cases On ", nyc_latest$date_of_interest ),
            h3(nyc_latest$HOSPITALIZED_COUNT[1]),
            icon = icon("hospital", lib = "font-awesome"),
            color = "olive"
        )
    })
    
    output$newD_case <- renderValueBox({
        valueBox(
            h4("New Deaths Cases On ", nyc_latest$date_of_interest ),
            h3(nyc_latest$DEATH_COUNT[1]),
            icon = icon("heart-broken", lib = "font-awesome"),
            color = "olive"
        )
    })
    
# Interactive Plot
    
    # countType <- input$count
    
    # by_boro = read.csv("../data/anti-by-boro.csv")
    output$bar_plt <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")
        if (input$count == 1){
            plotly::plot_ly(by_boro,
                            x= c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                            y = ~ NUM_PEOP_POS,
                            type = 'bar')}
        else if (input$count == 2){
            plotly::plot_ly(by_boro,
                            x= c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                            y = ~ NUM_PEOP_TEST,
                            type = 'bar')}
        else if (input$count == 3){
            plotly::plot_ly(by_boro,
                            x= c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                            y = ~ PERCENT_POSITIVE,
                            type = 'bar')}
        else {
            plotly::plot_ly(by_boro,
                            x= c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                            y = ~ TEST_RATE,
                            type = 'bar')}
        
    })
    
    


    output$plot1 <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")

        fig <- plot_ly(by_boro, labels = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                       values = ~NUM_PEOP_POS, type = 'pie')
        fig %>% layout(title = 'number of people test positive by borough',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    })
    
    output$plot2 <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")
        
        fig <- plot_ly(by_boro, labels = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'), 
                       values = ~PERCENT_POSITIVE*100, type = 'pie')
        fig %>% layout(title = 'percentage of people test positive by borough',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })


    
    
#Analysis Tab1 (Time Series Plot of Case Count,Death Count, and Cumulative Vaccine Count)     
    data_vaccine <- read.csv("https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/doses/doses-by-day.csv", stringsAsFactors = FALSE)
    data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
    death_rate <-read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/deathrate-by-modzcta.csv", stringsAsFactors = FALSE)
    
    data_vaccine$DATE <-as.Date(data_vaccine$DATE)
    data_by_day$date_of_interest <-as.Date(data_by_day$date_of_interest,format="%m/%d/%Y")
    colnames(data_by_day)[1]="DATE"
    colnames(death_rate)[1]="DATE"
    
    output$case <-renderHighchart({
        data_by_day = data_by_day[,c(1,2,4,5)]%>%
            tidyr::pivot_longer(
                cols = -DATE, 
                names_to = "line_var", 
                values_to = "value")
        hchart(data_by_day, "line", hcaes(x = DATE, y = value, group = line_var))%>%
        hc_chart(zoomType = "x") %>%
            hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
            hc_xAxis(title = list(text = "Date"),
                     labels = list(format = '{value:%b %d %y}')) %>%
            hc_yAxis(title = list(text = "Count"),
                     tickInterval = 400,
                     max = max(data_by_day$value)) %>%
            hc_title(text = paste0("<b>Covid-19' impact on NYC </b>")) %>%
            hc_subtitle(text = "Click and drag in the plot area to zoom in on a time span") %>%
            hc_plotOptions(area = list(lineWidth = 0.5)) %>% 
            hc_exporting(enabled = TRUE)
    })
    
    output$vaccine <-renderHighchart({
        data_vaccine= data_vaccine[,c(1,3,5,7,10)]%>%
            tidyr::pivot_longer(
                cols = -DATE, 
                names_to = "vaccine_status", 
                values_to = "value")
        hchart(data_vaccine, "line", hcaes(x = DATE, y = value, group = vaccine_status))%>%
            hc_chart(zoomType = "x") %>%
            hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
            hc_xAxis(title = list(text = "Date"),
                     labels = list(format = '{value:%b %d %y}')) %>%
            hc_yAxis(title = list(text = "Count"),
                     tickInterval = 400,
                     max = max(data_vaccine$value)) %>%
            hc_title(text = paste0("<b>Vaccine Status in NYC </b>")) %>%
            hc_subtitle(text = "Click and drag in the plot area to zoom in on a time span") %>%
            hc_plotOptions(area = list(lineWidth = 0.5)) %>% 
            hc_exporting(enabled = TRUE)
    })    
    
    output$death_rate <-renderHighchart({
        death_rate= death_rate[,c(1,2,3,4,5,6,7)]%>%
            tidyr::pivot_longer(
                cols = -DATE, 
                names_to = "Borough", 
                values_to = "value")
        hchart(death_rate, "line", hcaes(x = DATE, y = value, group = Borough))%>%
            hc_chart(zoomType = "x") %>%
            hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
            hc_xAxis(title = list(text = "Date"),
                     labels = list(format = '{value:%b %d %y}')) %>%
            hc_yAxis(title = list(text = "Percentage")) %>%
            hc_title(text = paste0("<b>Death Rate In Different Borough </b>")) %>%
            hc_subtitle(text = "Click and drag in the plot area to zoom in on a time span") %>%
            hc_plotOptions(area = list(lineWidth = 0.5)) %>% 
            hc_exporting(enabled = TRUE)
    })    
    
    

    output$plot3 <- renderPlot({
        df_vac <- read.csv("https://raw.githubusercontent.com/nychealth/covid-vaccine-data/main/people/coverage-by-boro-age.csv", stringsAsFactors = FALSE)
        df_vac = na.omit(df_vac)
        ggplot(data = df_vac,aes(x = AGE_GROUP,y = PERC_FULLY, fill=BOROUGH))+
            geom_col(position = 'dodge',
                     width = 0.5)+labs(title="Percent of Fully Vaccine Between Each Age Group", yaxis="Percent of Vaccine")
        
    })
    
    
#####tab2 Map####
    output$mymap <- renderLeaflet({ 
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender(
                "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
            ) %>%
            addProviderTiles("CartoDB.Voyager") %>%
            setView(lng = -73.935242, lat = 40.730610, zoom = 11)})
    
    
    #Covid Test data#
    df <- readr::read_rds(file="./data/processed_data.rda")
    df = df[df$category == 'covid testing site', ]
    df = df[df$open_now == 'Open'|df$open_now == 'Closed', ]

    #Free Meals data#
    free_meals = read.csv("./data/free_meals_locations.csv")    
    
    
    #Seasonal Flu Vaccine data#
    df_flu = read.csv("./data/vaccine_locations.csv")
    
    
    #Covid Test sites Button#

    observeEvent(input$covid_test, {
        proxy <- leafletProxy("mymap", data = df)
        palette_testing = c("red", "green")

        color1 <- colorFactor(palette=palette_testing, df$open_now)
        proxy %>% clearControls()


        # clear the map
        leafletProxy("mymap", data = df) %>%
            clearShapes() %>%
               clearMarkers() %>%
                 addProviderTiles("CartoDB.Voyager") %>%
                     fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)

        # output data related to covid testing sites
            leafletProxy("mymap", data = df) %>%
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
        })
        
    #Free meal Button
    observeEvent(input$free_meal, {
        proxy <- leafletProxy("mymap", data = free_meals)
        palette_fm = c("red","grey", "green")
        color2 <- colorFactor(palette =palette_fm, free_meals$Accessibility)
        proxy %>% clearControls()
            
        # clear the map
        leafletProxy("mymap", data = free_meals) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
            
        leafletProxy("mymap", data = free_meals)%>%
                clearShapes() %>%
                addProviderTiles("CartoDB.Voyager") %>%    
                addCircleMarkers(~Longitude, ~Latitude, radius=10,
                                 color = ~color2(Accessibility),
                                 label = paste(free_meals$SiteAddress, ', ', free_meals$City,', ', free_meals$Zip)
                )%>%
                addLegend("bottomright",
                          pal = color2,
                          values = free_meals$Accessibility,
                          title = "Status",
                          opacity = 1)
        })
        
    #Clear Button    
    observeEvent(input$reset,{
        proxy <- leafletProxy("mymap", data = df)
        proxy %>% clearControls()
        leafletProxy("mymap", data = df) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    })
        
    #Flu Button
    observeEvent(input$flu, {
        proxy <- leafletProxy("mymap", data = df_flu)
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = df_flu) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        leafletProxy("mymap", data = df_flu) %>%
            addAwesomeMarkers(~Longitude, ~Latitude, 
                              icon = awesomeIcons(markerColor= "red",
                                                  text = fa("syringe")), label= ~Facility.Name,                                  
                              popup = paste(
                                  "<b>Address:</b>", df_flu$Address,", ", df_flu$ZIP.Code,  "<br>",
                                  "<b>Phone:</b>", df_flu$Phone, "<br>",
                                  "<b>Website:</b>", df_flu$Website, "<br>"))
    })        
        
  
    # parks data
    adult_exer_equip <- readr::read_csv("./data/adultexerciseequip_clean.csv")
    playgrounds <- readr::read_csv("./data/playgrounds_clean.csv")

    
    ath_facilities_geo <- geojsonio::geojson_read("./data/atheleticfac_geo_clean.geojson", what ="sp")
    dog_runs_geo <- geojsonio::geojson_read("./data/dogruns_geo_clean.geojson", what ="sp")
    skate_parks_geo <- geojsonio::geojson_read("./data/skateparks_geo_clean.geojson", what ="sp")
    
    
    #adult exercise button
    observeEvent(input$adultexerciseequip, {
        proxy <- leafletProxy("mymap", data = adult_exer_equip)
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = adult_exer_equip) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        leafletProxy("mymap", data = adult_exer_equip) %>%
            addAwesomeMarkers(~longitude, ~latitude,
                              icon = awesomeIcons(markerColor = "green",
                                                  text = fa("dumbbell")),
                              label = ~SiteName, popup = ~content)
        })
    
    # playgrounds button
    playgrounds <- playgrounds %>% 
        dplyr::filter(Status == "Reopened" | Status == "COVID-19 Closure")
 
    observeEvent(input$playgrounds, {
        proxy <- leafletProxy("mymap", data = playgrounds)
        palette_testing = c("red", "green")
        color1 <- colorFactor(palette=palette_testing, playgrounds$Status)
        
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = playgrounds) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        leafletProxy("mymap", data = playgrounds) %>%
            addProviderTiles("CartoDB.Voyager") %>%
            addCircleMarkers(~longitude, ~latitude,
                              color = ~color1(Status), radius = 3,
                              label = ~name, popup = ~content) %>% 
            addLegend("bottomright",
                      pal = color1,
                      values = playgrounds$Status,
                      title = "Status", opacity =1)
    })
    
    # athletic facilities button
    tennis <- subset(ath_facilities_geo, primarysport %in% c("Tennis"))
    football <- subset(ath_facilities_geo, primarysport %in% c("Football"))
    soccer <- subset(ath_facilities_geo, primarysport %in% c("Soccer"))
    baseball <- subset(ath_facilities_geo, primarysport %in% c("Baseball"))
    basketball <- subset(ath_facilities_geo, primarysport %in% c("Basketball"))
    volleyball <- subset(ath_facilities_geo, primarysport %in% c("Volleyball"))
    track <- subset(ath_facilities_geo, primarysport %in% c("Track"))

    observe({
        proxy <- leafletProxy("mymap", data = tennis)
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = tennis) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        if (input$atheleticfac ==1){
            leafletProxy("mymap", data = tennis) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                              icon = awesomeIcons(markerColor = "lightgreen",
                                                  text = fa("users")),
                              label = ~name, popup = ~content)
        }
        
        if(input$atheleticfac ==2){
            leafletProxy("mymap", data = football) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "lightgray",
                                                      text = fa("football-ball")),
                                  label = ~name, popup = ~content)
        }
        if(input$atheleticfac ==3){
            leafletProxy("mymap", data = soccer) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "gray",
                                                      text = fa("futbol")),
                                  label = ~name, popup = ~content)
        }
        if(input$atheleticfac ==4){
            leafletProxy("mymap", data = baseball) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "blue",
                                                      text = fa("baseball-ball")),
                                  label = ~name, popup = ~content)
        }
        if(input$atheleticfac ==5){
            leafletProxy("mymap", data = basketball) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "orange",
                                                      text = fa("basketball-ball")),
                                  label = ~name, popup = ~content)
        }
        if(input$atheleticfac ==6){
            leafletProxy("mymap", data = volleyball) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "purple",
                                                      text = fa("volleyball-ball")),
                                  label = ~name, popup = ~content)
        }
        if(input$atheleticfac ==7){
            leafletProxy("mymap", data = track) %>%
                addAwesomeMarkers(~longitude, ~latitude,
                                  icon = awesomeIcons(markerColor = "green",
                                                      text = fa("running")),
                                  label = ~name, popup = ~content)
        }
    })
    
    #dog run  button
    observeEvent(input$dogruns, {
        proxy <- leafletProxy("mymap", data = dog_runs_geo)
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = dog_runs_geo) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        leafletProxy("mymap", data = dog_runs_geo) %>%
            addAwesomeMarkers(~longitude, ~latitude,
                              icon = awesomeIcons(markerColor = "beige",
                                                  text = fa("dog")),
                              label = ~name, popup = ~content)
    })
    
    #skate park button
    observeEvent(input$skateparks, {
        proxy <- leafletProxy("mymap", data = skate_parks_geo)
        proxy %>% clearControls()
        
        # clear the map
        leafletProxy("mymap", data = skate_parks_geo) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
        
        leafletProxy("mymap", data = skate_parks_geo) %>%
            addAwesomeMarkers(~longitude, ~latitude,
                              icon = awesomeIcons(markerColor = "darkred",
                                                  text = fa("snowboarding")),
                              label = ~name, popup = ~content)
    })
})

# Run the application 
#shinyApp(ui = ui, server = server)
