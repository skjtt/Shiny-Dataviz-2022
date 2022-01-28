library(shiny)
library(tidyverse)
library(shinyWidgets)
library(googlesheets4)

gs4_auth(cache = ".secrets",
         email = "sustainaweardata@gmail.com")

#?shinyWidgetsGallery


# UI ------------------------------------------------------------------------------------------


ui <- fluidPage(
  
  #liste <- 

  #selectizeInput("firma", label = "firma",
                 #choices = input$dataset)
  
  textInput("firma", "Firma",
            placeholder = "Skriv firma"),
  
  dateInput("dato", "Dato for udførelse",
            value = Sys.Date(),
            min = NULL,
            max = NULL,
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 1,
            width = NULL),
  
  radioButtons("eks", "Ekstern / Intern",
               choices = c("Ekstern", "Intern")),
  
  numericInput("t_tim", "Timer brugt",
               value = NULL),
  
  numericInput("t_min", "Minutter brugt",
               value = NULL),
  
  selectizeInput("bruger", label = "Navn",
                 choices = c("Anja", "Jeppe", "Caroline", "Sidse", "Mette", "Anne", "Kamma"),
                   options = list(
                     placeholder = "Vælg en bruger",
                     onInitialize = I('function() {this.setValue(""); }'))),
                  
  
  actionButton("tilføj", "Tilføj observation"),
  
  
)
table_data <- data.frame( firma = as.character(),
                          dato = as.Date(as.character()),
                          eks = as.character(),
                          t_tim = as.numeric(),
                          t_min = as.numeric(),
                          bruger = as.character(),
                          check.names = FALSE)
  
  
# Define server logic required to draw a histogram
server <- function(input, output) {

  tableValues <- reactiveValues(
    
    df = data.frame(firma = as.character(),
                    dato = as.Date(as.character()),
                    eks = as.character(),
                    t_tim = as.numeric(),
                    t_min = as.numeric(),
                    bruger = as.character(),
                    check.names = FALSE))
  
  observeEvent(input$tilføj, {
    
      show_alert(title = "tilføjet",
                 type = "succes")
    
    data <- tableValues$ny
    
    newRow <- data.frame(firma = input$firma,
                         dato = input$dato,
                         eks = input$eks,
                         t_tim = input$t_tim,
                         t_min = input$t_min,
                         bruger = input$bruger,
                         check.names = FALSE)
    
    data <- rbind(data, newRow)
    
    tableValues$ny <- data
    
    sheet_append("1Zr6aq2k4S2bX8fRCsB3kbKKGcCasGTQNmW9QMm1xXEE", newRow, sheet = 1)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
