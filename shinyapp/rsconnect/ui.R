# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("leaflet")
# install.packages("wordcloud2")
# install.packages("wordcloud")

library(shiny)
library(shinydashboard)
library(leaflet)
library(wordcloud2)
library(wordcloud)

ui<-dashboardPage(skin = "blue",
  dashboardHeader(title = "My dashboard"),
  dashboardSidebar(
    #create menu items 
    sidebarMenu(
      menuItem("Number of arrests over years", tabName = "numberOfArrestsOverYears", icon = icon("calendar-alt")),
      menuItem("What are people arrested for", tabName = "whatArePeopleArrestedFor", icon = icon("gavel")),
      menuItem("Diversity", tabName = "diversity", icon = icon("user-friends")),
      menuItem("Time of the year", tabName = "timeFfTheYear", icon = icon("calendar-minus")),
      menuItem("Dangerous neighbourhoods", tabName = "dangerousNeighbourhoods", icon = icon("search-location")),
      menuItem("Jurisdiction responsible", tabName = "jurisdictionResponsible", icon = icon("user-graduate"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #create a page of number of arrests 
      tabItem("numberOfArrestsOverYears",fluidPage(h1("Number of arrests over years")),
              box(plotOutput("numberOfArrestsOverYears_plot"), width = 8),
              box(
                selectInput("from", "From:",
                            c("2006","2007","2008","2009","2010","2011","2012",
                              "2013","2014","2015","2016","2017","2018")
                ),
                selectInput("to", "To:",
                            c("2018","2017","2016","2015","2014","2013","2012",
                              "2011","2010","2009","2008","2007","2006")
                ), width = 4
              )
          ),
      
      #create a page that presents what people are arrested for
      tabItem("whatArePeopleArrestedFor",fluidPage(h1("What are people arrested for")),
              box(plotOutput("whatArePeopleArrestedFor_plot"), width = 8, height=500),
              box(
                selectInput("whatArePeopleArrestedForfeatures", "Features:",
                            c("5", "6", "7","8")
                ), width = 4
              )
            ),
      
      #show diversity page
      tabItem("diversity",fluidPage(h1("Diversity")),
              box(plotOutput("diversity_plot"), width = 8, height=500),
              box(
                selectInput("diversityfeatures", "Features:",
                            c("countsSex","countsRace","countsAge")
                ), width = 4
              )
            ),
      
      #show the months 
      tabItem("timeFfTheYear",fluidPage(h1("Time of the year")),
              box(plotOutput("timeFfTheYear_plot"), width = 8)
            ),
      
      #display map
      tabItem("dangerousNeighbourhoods",
              fluidPage(
                  h1("Dangerous neighbourhoods"),
                  leafletOutput("mymap", height=600),
                  p()
              )),
      
      #display word cloud 
      tabItem("jurisdictionResponsible",fluidPage(h1("Jurisdiction responsible")),
              bootstrapPage(
                wordcloud2Output('wordcloud2')
              ),
              box(
                selectInput("jurisdictionResponsibleFeatures", "Features:",
                            c("LAW_CODE", "ARREST_PRECINCT")
                ), width = 4
              )
              
            )
    )
  )
)
