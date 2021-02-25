##install packages
library(maptools)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(dplyr)
library(rgeos)
library(sf)
library(htmltools)

# Load dataset  ----
path = "H:\\NYPD\\"

arrestData <- read.csv(paste0(path, "ArrestDat.csv"), header=TRUE)
precincts1 <- sf::st_read(paste0(path, "\\precincts1\\nypp.shp"))
map_data = arrestData[arrestData$Year == 2017, ]

exp_minus_one <- function(x) { round( exp(x)-1 ) }