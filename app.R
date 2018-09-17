# Project #1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# Upload Philadelphia property assessment data from Opendataphilly
# Many fields were removed to decrease data upload
# Data can be found here: https://www.phila.gov/property/data/
property <- read.csv ("projectdata.csv")

