# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("shinydashboard")
# install.packages("shinyWidgets")
# install.packages("shinyTime")
# install.packages("colourpicker")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("DT")

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinyTime)
library(colourpicker)
library(plyr)
library(dplyr)
library(tidyverse)
library(DT)

styles <- list(submit = "color: #fff; background-color: #337ab7; border-color: #2e6da4")

thickness <- c("liquid",1:9,"brick")
pieces <- c("none",1:9,"a lot and big!")

food_default <- c("Bread"="Bread1","tomato"="tomato2","banana"="banana3")


# load previous session
files <- list.files("./www/",full.names=TRUE)

loadable_files <- c("./www/foods.RData",
                    "./www/shitlist.RData",
                    "./www/templates.RData")
for(f in files){
  if(f %in% loadable_files){
    load(f)
  }
}

shit_template_names_default <- "--- none ---"
if (exists("templates")){
  shit_template_names <- c(shit_template_names_default,names(templates))
} else {
  shit_template_names <- shit_template_names_default
}