#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"), collapsible = T,
                   title = "LA County COVID-19 Dashboard", id = "nav",
                   
                   ## Tab 1: MAP
                   tabPanel("Map",
                            leafletOutput("covidMap")
                            ),
                   
                   ## Tab 2: COMPARE (USE RAW LADPH REPORTING HIERARCHY)
                   tabPanel("Compare"),
                   
                   ## Tab 3: TABLE
                   tabPanel("Table")


    
))
