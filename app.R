# Project #1

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

#upload Philly shooting victim data from Opendataphilly
property <- read.csv ("projectdata.csv")

