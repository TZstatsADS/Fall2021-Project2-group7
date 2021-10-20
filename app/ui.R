if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if(!require(highcharter)) devtools::install_github("jbkunst/highcharter")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")

data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
nyc_latest <- data_by_day %>% tail(1)



# Define UI for application that draws a histogram
shinyUI(dashboardPage(

    # Application title
    skin = "blue",
    dashboardHeader(title ="How Covid-19 Has Impacted NYC",titleWidth = 350),
    
    # Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Interactive Plot", tabName = "Interac_plot", icon = icon("dashboard")),
            menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar"),
                     startExpanded = TRUE,
                     menuSubItem("How Vaccine Slows Covid-19",tabName = "a"),
                     menuSubItem("Vaccine Status of All Ages",tabName = "b")),
            menuItem("About", tabName="About", icon = icon("list-alt"))
        )
    ),
    
    # Body content
    dashboardBody(
        tabItems(
            
            # Home tab content
            tabItem(tabName = "Home",
                    fluidPage(
                        h2(strong("How Covid-19 Impacted New York City and People's Lives"),align = "center"),
                        h3("Wanxin Wang, Ziyi Wang, Mingyuan Xia, Jee sun Yun",align = "center",style="color:gray"),
                        h4("2021Fall GR5243 Project2 Group7 - M.A. Statistics at Columbia University",align = "center",style="color:gray"),
                        
                        fluidRow(width = 20, 
                                 br(),
                                 h3("Covid-19 waves of destruction have inflicted their own kind of despair on humanity in the 21st century, leaving many to wonder when the pandemic will end. 
                                 Our lives have changed drastically since New York City reported the first COVID-19 cases on March 1,2020. There will be a lot of changes that are substantial and persistent. We won't look back and say, That was a terrible time, but it's over.
                                 We will be dealing with many of the ramifications of Covid-19 for decades. 
                                 New York State has the highest numbers of confirmed cases in the United States till mid-July and most cases were in New York City where half of the population lives.", style="color:black", align = "left"),
                                 br(),
                                 h3("In effort to stop the pandemic, the U.S. Department of Health & Human Services expedited the vaccine development program with Operation Warp Speed.
                                    However, we are living in the Covid-19 era, not the Covid-19 crisis. Especially in the months before the Delta variant became dominant, the pandemic seemed like it should be nearly over. It may not be over even when physical disease, measured in illness and mortality, has greatly subsided. 
                                    It may continue as the economy recovers and life returns to a semblance of normality.", style="color:black",align = "Left"),
                                 br(),
                                 style = "background-image: url('https://cdn2.newsok.biz/cache/large960_blur-bf4f0d8c2bad316af28263a71c41e742.jpg');
                                    background-repeat:no-repeat;background-size:cover;
                                      opacity: 0.8;
                                    background-position:center;"
                        ),
                        
                        fluidRow(hr(),
                                 column(width = 5,img(src="https://b6a1x2ll0cv47ypevpg0pcak-wpengine.netdna-ssl.com/wp-content/uploads/2020/12/covid-vaccine.png",width = "100%", height = "35%"),align = "center",
                                        p(strong("Current Vaccine Eligibility:")," As of August 16 2021, New Yorkers with compromised immune systems can now receive their third COVID-19 vaccine dose.",
                                          br(),
                                          a(href="https://www1.nyc.gov/site/doh/covid/covid-19-vaccine-eligibility.page", "Check Eligibility",target="_blank"),style="text-align:center;color:black")),
                                 
                                 box(width = 5, height = "10%", h2(strong("NYC Covid Updates, Resources, and Vaccine Situation App"),align = "center"),
                                     background = "blue",
                                     h4("Welcome to our App,
                            We want to provide New Yorkers with updated COVID-19 information and analyze how it has impacted people's lives and social environments"),
                                     br(),
                                     tags$div(
                                         "1. Click the Map tab to understand the precautions in your target area, including testing sites, available vaccine location, and other resources", 
                                         tags$br(),
                                         tags$br(),
                                         "2. Click the Interactive Plot tab to see how Covid cases change with respect to locatoin in NYC",
                                         tags$br(),
                                         tags$br(),
                                         "3. Click the analysis tab to deep dive into the data by our different residents."
                                     ) ),
                        ),
                        
                        fluidRow(
                            width = 12,
                            h2("Overall COVID-19 Case & Death",align = "center"),
                            p("Last Updated:  ", nyc_latest$date_of_interest),
                            valueBoxOutput("total"),
                            valueBoxOutput("death"),
                            valueBoxOutput("hospital_case")),
                        fluidRow(
                            valueBoxOutput("new_case"),
                            valueBoxOutput("newH_case"),
                            valueBoxOutput("newD_case"),
                        )
                    )),
            
            # About Tab content 
            tabItem(tabName = "About",
                    box(width=12, h1(strong("ABOUT"), align="center")),     
                    mainPanel(width=12, h2(strong("Contributors"), align="center")),
                    h4("Wanxin Wang, ww2581@columbia.edu", align="center"),
                    h4("Ziyi Wang, zw2732@columbia.edu", align="center"),
                    h4("Mingyuan Xia, mx2204@columbia.edu", align="center"),
                    h4("Jeesun Yun, jy3112@columbia.edu", align="center"),
                    br(),
                    
                    mainPanel(width=12, h2(strong("Data Source"), align="center"),
                              h4("The data source used is from", a("NYC Coronavirus Disease 2019 Data", 
                                                                   href="https://github.com/nychealth/coronavirus-data"), 
                                 " and ", a("NYC Open Data By Agency",
                                            href="https://opendata.cityofnewyork.us/data/"),
                                 ". ",align="center")
                    ),
                    br(),
                    mainPanel(width=12, h2(strong("Code"), align="center"),
                              h4("More detailed codes are shared at", a("Github", 
                                                                        href="https://github.com/TZstatsADS/Fall2021-Project2-group7"), ". ", align="center"))
            ),
            
            #Interactive plot Tab content
            tabItem(tabName = "Interac_plot",
                    box(width=6,
                        h4("The bar chart below shows that Brooklyn and Queens have the most positive antibody cases."),
                        h4("Please click on the select box to see more specific distribution by borough."),
                        br(),
                        

                        fluidPage(
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
                        
                        ),
                    
                    box(
                        width=6,
                        h4("Please click on the select box to see more specific distribution of positive antibody tests number & rate."),
                        br(),
                        
                        mainPanel(
                            fluidRow(
                                tabsetPanel(id = "tpanel",
                                            type = "tabs",
                                            tabPanel("number of test positive", plotlyOutput("plot1")),
                                            tabPanel("percent of test positive", plotlyOutput("plot2")))
                            )
                        ))
                    ),
            
            
            
            #Analysis Submenu 1
            tabItem(tabName = "a", 
                    mainPanel(
                        highchartOutput("case",width = "80%",height = "300px"),
                        highchartOutput("vaccine",width = "80%",height = "300px"),
                        highchartOutput("death_rate",width = "80%",height = "300px")
                    )),
            
            #Analysis Submenu 2
            tabItem(tabName = "b", 
                    mainPanel(
                        plotOutput(outputId = "plot3")
                    )),
            
            # Tab2 Map  
            tabItem(tabName = "map", 
                    fluidPage(
                        actionButton("free_meal","Free Meals",icon=icon("utensils",  lib = "font-awesome")),
                        actionButton("covid_test", "Covid Testing",icon=icon("vial", lib = "font-awesome")),
                        actionButton("flu", "Seasonal Flu Vaccinations",icon=icon("map-marked-alt", lib = "font-awesome")),
                        actionButton("reset", "Clear",icon=icon("fast-backward", lib = "font-awesome")),
                        leafletOutput("mymap", width="100%", height=800))
            )
            
)
)
))
 