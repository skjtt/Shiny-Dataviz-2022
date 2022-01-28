
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

passat <- read_excel("K2 - passat.xlsx")
RT_data <- read.csv2("Rt_cases_2021_04_13.csv")

ggplot(data = RT_data, aes(x = as.Date(SampleDate), y = estimate)) +
  geom_line()

install.packages('rsconnect')


textInput("fødeby", "fødeby"
          placeholder = "vælg din fødeby"),

radioButtons("køn", "køn",
             choices = c("mand", "kvinde")),

numericInput("tid" "timer brugt på studiet om ugen",
             value = NULL),

sliderTextInput("glad", label = "hvor glad er du i dag?",
                choices = c("Ked", "Lidt nede", "middel", "Glad", "Meget glad"),
                selected = c("middel"))
#library(shinywidgets)

actionButton("tilføj", "Tilføj observation"),


#library(googlesheets4)