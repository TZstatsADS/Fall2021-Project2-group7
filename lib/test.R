library(shiny)
library(tidyverse)
library(plotly)


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
          ))
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
}





shinyApp(ui = ui, server = server)
