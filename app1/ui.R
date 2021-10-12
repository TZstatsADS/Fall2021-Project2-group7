if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    skin = "blue",
    dashboardHeader(title ="How Covid-19 Has Impacted NYC",titleWidth = 350),
    
    # Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Interactive Plot", tabName = "Time_series", icon = icon("dashboard")),
            menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar"),
                     startExpanded = TRUE,
                     menuSubItem("By a",tabName = "a"),
                     menuSubItem("By b",tabName = "b")),
            menuItem("About", tabName="About", icon = icon("list-alt"))
        )
    ),
    
    # Body content
    dashboardBody(
        tabItems(
            # Home tab content
            tabItem(tabName = "Home",
                    fluidPage(
                        h2(strong("How Covid-19 Has Impacted New York City"),align = "center"),
                        h3("Wanxin Wang, Ziyi Wang, Mingyuan Xia, Jee sun Yun",align = "center",style="color:gray"),
                        h4("2021Fall GR5243 Project2 Group7 - M.A. Statistics at Columbia University",align = "center",style="color:gray"),
                        
                    fluidRow(width = 20, 
                                 br(),
                                 h3("Covid-19’s waves of destruction have inflicted their own kind of despair on humanity in the 21st century, leaving many to wonder when the pandemic will end. 
                                 Our lives have changed drastically since New York City reported the first COVID-19 cases on March 1,2020. There will be a lot of changes that are substantial and persistent. We won’t look back and say, ‘That was a terrible time, but it’s over.’ 
                                 We will be dealing with many of the ramifications of Covid-19 for decades. 
                                 New York State has the highest numbers of confirmed cases in the United States till mid-July and most cases were in New York City where half of the population lives.",align = "left"),
                                 br(),
                                 h3("In effort to stop the pandemic, the U.S. Department of Health & Human Services expedited the vaccine development program with Operation Warp Speed.
                                    However, we are living in the Covid-19 era, not the Covid-19 crisis. Especially in the months before the Delta variant became dominant, the pandemic seemed like it should be nearly over. It may not be over even when physical disease, measured in illness and mortality, has greatly subsided. 
                                    It may continue as the economy recovers and life returns to a semblance of normality.", style="color:black",align = "Left"),
                                 br(),
                                 style = "background-image: url('https://www.beneschlaw.com/images/content/1/4/v1/14735/NewYork-1100x900.jpg');
                                    background-repeat:no-repeat;background-size:cover;
                                      opacity: 0.9;
                                    background-position:center;"
                        ),
                        
                        fluidRow(hr(),
                                 column(width = 5,img(src="https://b6a1x2ll0cv47ypevpg0pcak-wpengine.netdna-ssl.com/wp-content/uploads/2020/12/covid-vaccine.png",width = "100%", height = "35%"),align = "center",
                                        p(strong("Current Vaccine Eligibility:")," As of August 16 2021, New Yorkers with compromised immune systems can now receive their third COVID-19 vaccine dose.",
                                          br(),
                                          a(href="https://www1.nyc.gov/site/doh/covid/covid-19-vaccine-eligibility.page", "Check Eligibility",target="_blank"),style="text-align:center;color:black")),
                                 
                                 box(width = 5, height = "10%", h2(strong("NYC xxx App"),align = "center"),
                                     background = "blue",
                                     h4("Welcome to our xxx App,
                            We want to provide New Yorkers with updated COVID-19 information and analyze how it has impacted xxx"),
                                     br(),
                                     tags$div(
                                         "1. Click the Map tab to understand the COVID situation in your target area", 
                                         tags$br(),
                                         tags$br(),
                                         "2. Click the Interactive Plot tab to see how Covid cases change with respect to time and locatoin in NYC",
                                         tags$br(),
                                         tags$br(),
                                         "3. Click the analysis tab to deep dive into the data by xxxxx"
                                     ) ),
                        ),
                        
                        fluidRow(
                            width = 12,
                            h2("Overall COVID-19 Case & Death",align = "center"),
                            p("Last Updated: Aug, 2021"),
                            valueBoxOutput("total"),
                            valueBoxOutput("death")),
                        fluidRow(
                            valueBoxOutput("max_case"),
                            valueBoxOutput("max_death"),
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
            )
            

)
)
)
 