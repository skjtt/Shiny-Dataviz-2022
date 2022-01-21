

library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

passat <- read_excel("K2 - passat.xlsx")
RT_data <- read_csv2("Rt_cases_2021_04_13.csv")
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
            sliderInput("price_a", "vælg max pris",
                        min = min(passat$price),
                        max = max(passat$price),
                        value = mean(passat$price),
                        step = 10000),
            
          
            #eventuel tilføj brugers egen titel = textInput("title", "")
            
            submitButton("Lav ændringer")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
           "Her viser jeg mine plots", 
           
           br(),
           br(),
           
           tabsetPanel(id = "tabs", 
                       tabPanel("plot",
                                br(),
                                selectInput("xaxis", "Vælg x-akse", choices = names(passat),
                                    selected = "km_per_liter", TRUE, multiple = FALSE),
                       
                                selectInput("yaxis", "Vælg y-akse", choices = names(passat), 
                                    multiple = FALSE), 
                                br(), 
                                plotOutput("plot")),
                       tabPanel("Tabel", tableOutput("tabel")),
                       tabPanel("RT_plot", plotOutput("RT_cases")),
                       br(),
                       dateRangeInput(inputId = "RT_data", "vælg datoer",
                                     start = min(RT_data$SampleDate),
                                     end = Sys.Date(),
                                    max = Sys.Date(),
                                    startview = "month",
                                    weekstart = 1),
                       plotOutput("covid_plot")
                       )
        )
                                      
                                      
                                      
                                      
                                       #strong("Date Range"),
                                      #start = min(RT_data$SampleDate), end = max(RT_data$SampleDate),
                                     # min = min(RT_data$SampleDate), max = max(RT_data$SampleDate)),
                       #plotOutput("Graph"),)
           
       


# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$plot <- renderPlot({
        
        pas_a <- passat %>% 
            filter(price <= input$price_a)
        
        ggplot(data = pas_a, aes_string(x = as.name(input$xaxis), y = as.name(input$yaxis))) +
            geom_point(size = input$size, alpha = input$alpha) +  
            labs(title = paste0("her er sammenhængen mellem ", as.name(input$xaxis), " og ", as.name(input$yaxis)),
                 subtitle = paste0("her vises biler der er koster mindre end ", input$price_a, " kroner.")) +
            theme(plot.title = element_text(size = 30), 
                  plot.subtitle = element_text(size = 20),
                  axis.title = element_text(size = 15))
        
    })
    
    output$tabel <- renderTable({
        
        pas <- passat %>% 
            filter(price <= input$price_a)
      
        print(pas)  
      
    })
 
    output$RT_cases <- renderPlot({
        
        rt2 <- RT_data %>% 
            filter(SampleDate >= as.Date(input$rt_date[1]) & SampleDate <= as.Date(input$rt_date[2]))
        
       p <- ggplot(data = RT_data, aes(x = as.Date(SampleDate), y = estimate)) +
                   geom_line()+
                  goem_hline(yintercept = 1, color = "red", size = 2, alpha = 0.5) +
          labs(x = "Dato",
               y = "Kontakttal")
       
       print(p) 
       
    })
       
}

# Run the application 
shinyApp(ui = ui, server = server)
