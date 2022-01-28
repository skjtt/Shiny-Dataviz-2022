library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)

rt <- read_csv2("Rt_cases_2022_01_18.csv")
na <- read_csv2("Newly_admitted_over_time.csv")
tp <- read_csv2("Test_pos_over_time.csv")
Dt <- read_csv2("Deaths_over_time.csv")


# UI ------------------------------------------------------------------------------------------



ui <- dashboardPage(
    title = "Covid-19 dashboard - Dataanalyse 2022", skin = "purple",

    dashboardHeader(title = div(img(height = 50, width = 50, src = "covid19.png"))),
    
    dashboardSidebar(
        
        collapsed = TRUE,
        
        sidebarMenu(id = "menu",
                    menuItem("Danmarks nøgletal", tabName = "nøgletal", icon = icon("viruses"),
                             menuSubItem("København", tabName = "kbh", icon = icon("city")),
                             menuSubItem("Nordjylland", tabName = "njy")),
                    menuItem("Sverige nøgletal", tabName = "sv_nøgletal", icon = icon("viruses")))
        
    ),
    
    dashboardBody(
        
        tabItem(tabName = "nøgletal", 
                
                    fluidRow(valueBoxOutput(width = 3, "rt"), 
                             (valueBoxOutput(width = 3, "na")), 
                             (valueBoxOutput(width = 3, "tp")), 
                             (valueBoxOutput(width = 3, "Dt"))),
                fluidRow(
                  
                    tabBox(title = "tendens", id = "tendens", width = 12,
                           
                           tabPanel("kontakttal", 
                                    fluidRow(column(width = 4, box("vælg datoer", status = "primary", width = NULL,
                                                                   solidHeader = TRUE,
                                                                   
                                                                   dateRangeInput(inputId = "rt_Date", "vælg datoer",
                                                                                  start = Sys.Date(),
                                                                                  end = Sys.Date(),
                                                                                  max = Sys.Date(),
                                                                                  startview = "month",
                                                                                  weekstart = 1),
                                                                                  
                                                                    checkboxInput("trend_line", label = "plot en tendenslinje", value = FALSE),
                                                                    submitButton("lav ændringer")))),
                                    fluidRow(column(width = 12, box(title = "kontakttal over tid", status = "primary", width = NULL)))))
                )
                ),
        
      
    )
    
)


# Server --------------------------------------------------------------------------------------



server <- function(input, output) {

    output$rt <- renderValueBox({
        
        last_row <- tail(rt, n = 1)
        
        valueBox(value = last_row$estimate, "kontakttal", 
                 subtitle = paste0("kontakttal pr. ", last_row$SampleDate) ,icon = icon("people-arrows"), color = "yellow")
        
    })
    
    output$na <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Total, "kontakttal",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "blue")
    })
    
    output$tp <- renderValueBox({
        
        right_data <- slice_tail(tp, n = 9) %>% 
            slice_head(n = 7)
        
        right_cell <- mean(right_data$PosPct)
        
        right_cell <- round(right_cell, digits = 2)
        
        valueBox(value = paste0(right_cell, " %"),
                 subtitle = paste0("positiv % løbende uge "), icon = icon("syringe"), color = "red")
        
    })
    
    output$Dt <- renderValueBox({
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
