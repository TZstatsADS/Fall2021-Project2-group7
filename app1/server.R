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

# Define server logic required to draw a histogram
server <- function(input, output) {
    data_by_modzcta=read.csv(file="~/Documents/GitHub/Fall2021-Project2-group7/data/data-by-modzcta.csv")
    by_borough=read.csv("~/Documents/GitHub/Fall2021-Project2-group7/data/by-boro.csv")
    output$total <- renderValueBox({
        valueBox(
            h4("Total Case Count"),
            h3(sum(data_by_modzcta$COVID_CASE_COUNT)),
            icon = icon("list"),
            color = "aqua"
        )
    })
    
    output$death <- renderValueBox({
        valueBox(
            h4("Total Death Count"),
            h3(sum(data_by_modzcta$COVID_DEATH_COUNT)),
            icon = icon("user"),
            color = "olive"
        )
    })
    
    output$max_case <- renderValueBox({
        valueBox(
            h4("Max Case Count: Queens"),
            h3(max(by_borough$CASE_COUNT)),
            icon = icon("list"),
            color = "aqua"
        )
    })
    
    output$max_death <- renderValueBox({
        valueBox(
            h4("Max Death Count: Queens"),
            h3(max(by_borough$DEATH_COUNT)),
            icon = icon("user"),
            color = "olive"
        )
    })

    
}

# Run the application 
#shinyApp(ui = ui, server = server)
