

library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

passat <- read_excel("K2 - passat.xlsx")
theme_set(theme_minimal())
options(scipen = 999)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dataviz first attempt 2022", title = div(img(height = 100,
                                                             width =125, 
                                                             src = "dania_logo.png"))),
    theme = shinytheme("cosmo"),
    
    headerPanel("Informationer angående passat handel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            sliderInput("alpha", "vælg transparenthed",
                        min = 0.1,
                        max = 1.0,
                        value = 0.5),
            
            sliderInput("size", "Vælg størrelse på punkter",
                        min = 1,
                        max = 20, 
                        value = 5),
            
            #eventuel tilføj brugers egen titel = textInput("title", "")
            
            submitButton("Lav ændringer")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
           "Her viser jeg mine plots", 
           
           tabsetPanel(id = "tabs", 
                       br(),
                       selectInput("xaxis", "Vælg x-akse", choices = names(passat),
                                   selected = "km_per_liter", TRUE, multiple = FALSE),
                       
                       selectInput("yaxis", "Vælg y-akse", choices = names(passat), 
                                   multiple = FALSE), 
                       br(),
                       tabPanel("plot", plotOutput("plot")),
                       tabPanel("Tabel", tableOutput("tabel")))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderPlot({
        
        
        
        ggplot(data = passat, aes_string(x = as.name(input$xaxis), y = as.name(input$yaxis))) +
            geom_point(size = input$size, alpha = input$alpha) +
            labs(title = paste0("her er sammenhængen mellem ", as.name(input$xaxis), " og ", as.name(input$yaxis)))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
