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
    
# Interactive Plot
    
    # countType <- input$count
    
    # by_boro = read.csv("../data/anti-by-boro.csv")
    output$bar_plt <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")
        if (input$count == 1){
            plotly::plot_ly(by_boro,
                            x= row.names(by_boro),
                            y = ~ NUM_PEOP_POS,
                            type = 'bar')}
        else if (input$count == 2){
            plotly::plot_ly(by_boro,
                            x= row.names(by_boro),
                            y = ~ NUM_PEOP_TEST,
                            type = 'bar')}
        else if (input$count == 3){
            plotly::plot_ly(by_boro,
                            x= row.names(by_boro),
                            y = ~ PERCENT_POSITIVE,
                            type = 'bar')}
        else {
            plotly::plot_ly(by_boro,
                            x= row.names(by_boro),
                            y = ~ TEST_RATE,
                            type = 'bar')}
        
    })
    
    
    # save(by_boro, file="../output/by_boro.RData")
    
    # if (countType == "case count"){
    #     # barplot(by_boro$NUM_PEOP_POS, main='case count by borough',
    #     #         names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
    #     #         xlab="borough", ylab="case count", col=brewer.pal(6,"Set3"))
    #     # 
    #     plot_ly(by_boro,
    #         x = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
    #         y = ~NUM_PEOP_POS,
    #         name = "case count by borough",
    #         type = "bar"
    #     )
    #     
    #     
    # }
    # 
    # 
    # # save(df1, file="../output/df1.RData")
    # 
    # if (countType == "test count"){
    #     # barplot(by_boro$NUM_PEOP_TEST, main='number of tests by borough', 
    #     #         names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'), 
    #     #         xlab="borough", ylab="test count", 
    #     #         col=c("lightblue","lavender"))
    #     
    #     plot_ly(by_boro,
    #             x = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
    #             y = ~NUM_PEOP_TEST,
    #             name = "case count by borough",
    #             type = "bar")
    # }
    # 
    # if (countType == "percent positive"){
    #     # barplot(by_boro$PERCENT_POSITIVE, main='percent positive by borough', 
    #     #         names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'), 
    #     #         xlab="borough", ylab="percent",
    #     #         col=brewer.pal(4,"Set3"))
    #     
    #     plot_ly(by_boro,
    #             x = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
    #             y = ~PERCENT_POSITIVE,
    #             name = "case count by borough",
    #             type = "bar")
    # }
    # 
    # if (countType == "test rate"){
    #     # barplot(by_boro$TEST_RATE, main='test rate by borough', 
    #     #         names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'), 
    #     #         xlab="borough", ylab="test rate",
    #     #         col=brewer.pal(10,"Set3"))
    #     
    #     plot_ly(by_boro,
    #             x = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
    #             y = ~TEST_RATE,
    #             name = "case count by borough",
    #             type = "bar")
    # }
    # 
    # })

    output$plot1 <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")

        fig <- plot_ly(by_boro, labels = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'),
                       values = ~NUM_PEOP_POS, type = 'pie')
        fig %>% layout(title = 'number of people test positive by borough',
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



    #    # if (input$tpanel == "number of test positive"){
    #     #    slices <- by_boro$NUM_PEOP_POS
    #      #   lbls <- c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland')
    #       #  pct <- slices
    #        # lbls <- paste(lbls, pct)
    #         #lbls <- paste(lbls, sep="")
    #     #    pie(slices,labels = lbls, col=brewer.pal(5,"Set2"),
    #      #       main="number of people test positive by borough")
    #   #  }
    })
    
    output$plot2 <- renderPlotly({
        by_boro = read.csv("../data/anti-by-boro.csv")
        
        fig <- plot_ly(by_boro, labels = c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland'), 
                       values = ~PERCENT_POSITIVE*100, type = 'pie')
        fig %>% layout(title = 'percentage of people test positive by borough',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
       # if (input$tpanel == "percent of test positive"){
        #    slices <- by_boro$PERCENT_POSITIVE
         #   lbls <- c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland')
         #   pct <- slices*100
         #   lbls <- paste(lbls, pct) 
         #   lbls <- paste(lbls,"%",sep="") 
          #  pie(slices,labels = lbls, col=brewer.pal(5,"Set2"),
          #      main="percentage of people test positive by borough")
       # }
    })

    
}

# Run the application 
#shinyApp(ui = ui, server = server)
