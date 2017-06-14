#devtools::install_github(repo = "maksimhorowitz/nflscrapR") Only run this line if need to install

# Required Packages and functions
library(shiny)
library(shinyjs)
library(shinyFiles)
library(rsconnect)
library(reshape2)
require(plyr)
library(dplyr)
library(DT)
library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(googleVis)
library(rvest)
library(xml2)
library(XML)
library(plotly)
library(DT)
library(tidyr)
library(stringr)
library(nflscrapR)

# CSS switch
mycss = 'Flatly.css'

m <- list(l = 50,
          r = 50,
          b = 100,
          t = 100,
          pad = 4)

load(file = "data/all.RData")