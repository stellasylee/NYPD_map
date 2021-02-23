##install packages
library(maptools)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(dplyr)
library(rgeos)
library(sf)

path = "C:\\Users\\stella\\Documents\\GitHub\\NYPD_map\\"

if(!exists("arrestData")) arrestData <- read.csv(paste0(path, "ArrestDat.csv"), header=TRUE)
if(!exists("precincts1")) precincts1 <- sf::st_read(paste0(path, "\\precincts1\\nypp.shp"))

pop_data <- arrestData[arrestData$Year == 2010,]
pop_data <- select(pop_data, "Precinct", "Population", "Area", "AsPac", "Black", "Hisp", "Native", "White")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
body <- dashboardBody(
  tags$head(tags$style(HTML('
                         /* Sidebar font size */
                              .sidebar-menu>li>a {
                                   font-size:16px;
                              }
                         /* Box title font size */
                              .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
                                     font-size: 20px;
                              }
                         /* Overall font size */
                              body {
                              font-size: 16px;
                              }
                              small {
                              font-size: 14px;
                              }
                         /* Table properties */
                              td {
                                  padding-left: 15px;
                                  padding-right: 15px;
                                   vertical-align: middle;
                              }
                         /* Expand and center title */
                              .main-header .logo {
                                   float:inherit;
                                   width:inherit;
                              }
                              .main-header .navbar {
                                   display: none;
                              }

                             '))),
  
  title = "NYPD Precincts", skin="blue",
  
  fluidPage(fluidRow(
    column(8,
           box(title = "Map of Police Precincts", solidHeader=T, status="primary", width = '100%',
               div(leafletOutput("nycMap", height = 450)),
               div(htmlOutput("footer"), align = "right")
           ),
           
           box(title="Map Options", status = "primary", solidHeader=T, collapsible=T, width = '100%',
               
               div(style = 'display: flex',
                   div(style = 'flex: 2',
                       selectInput("colorby","Color Precincts by:",choices=c(
                         "Total number of arrests, weighted by precinct population" = "arrests_weighted",
                         "Total number of arrests" = "arrests_raw",
                         "Total number of arrests for each race" = "race_arr",
                         "Number of arrests by race, weighted by precinct population" = "race_weighted",
                         "Racial distribution for each precinct" = "race_dist"))),
                   
                   div(style = 'flex: 1',
                       conditionalPanel(condition = "input.colorby == 'race_dist' ||
                                                                         input.colorby == 'race_arr' || input.colorby == 'race_weighted'",
                                        selectInput("race", "Race", choices = c(
                                          "White" = "W",
                                          "Black" = "B",
                                          "Hispanic" = "H"
                                          #"Asian/Pacific islander" = "A",
                                          #"Native American" = "N"
                                        ))))
               ),
               
               div(style = 'display: flex',
                   div(style = 'flex: 1',
                       selectizeInput('removePrecincts', "Remove precincts", multiple = TRUE,
                                      choices=arrestData$Precinct, selected = c(22, 50))),
                   div(style = 'flex: 1',
                       selectizeInput('filterPrecincts', "Filter precincts", multiple = TRUE,
                                      choices = c("Show all", arrestData$Precinct),
                                      selected = "Show all",
                                      options = list(maxItems = 5)))),
               
               radioButtons('scale', "Scale", choices = c('Linear', 'Logarithmic'), inline = TRUE,
                            selected = "Logarithmic"),
               
               sliderInput('year', "Year Range", min = 2005, max = 2018, value = c(2005, 2018), sep = "", animate = TRUE)
               
           ))
    )))

ui <- dashboardPage(dashboardHeader(title = "Stop-and-Frisk in New York City"),
                    dashboardSidebar(disable = TRUE),
                    body)
#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#

server <- function(input, output) {
  #----------------------------------------------------------------------------------------------------------------#
  #                                                 CREATE MAP                                                     #
  #----------------------------------------------------------------------------------------------------------------#
  
  updateYear <- function (){
    map_data = arrestData[arrestData$Year >= input$year[1] & arrestData$Year <= input$year[2], ]
    map_data = aggregate(map_data, by = list(Precinct = map_data$Precinct), FUN = sum)
    map_data = select(map_data, "Precinct", "AsPacA", "BlackA", "HispA", "NativeA", "WhiteA", "TotalA")
    map_data = left_join(map_data, pop_data, by = "Precinct")
  }
  
  updateMarkers() <- function (){
    
  }
  
  #    Render base map ----
  output$nycMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.004, 40.705, zoom = 10) %>%
      setMaxBounds(-73.000, 40.200, -75.000, 41.100)
  })
  
  #    Create instance of base map where polygons can be added
  MapProxy <- leafletProxy('nycMap')
  
  # Update map when new year range is selected
  observeEvent({input$year}, {
    updateYear()
    updateMarkers()
  })
  
  # Update map when new color is selected
  observeEvent({input$year}, {
    updateMarkers()
  })
  # Update map when new filter is selected
  observeEvent({input$year}, {
    updateMarkers()
  })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)