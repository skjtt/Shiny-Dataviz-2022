library(shiny)
library(shinydashboard)
library(tidyverse)


na <- read_csv2("Newly_admitted_over_time.csv")

# UI ------------------------------------------------------------------------------------------

ui <- dashboardPage(
    title = "indlagte med covid-19 i Danmark", skin = "purple",
    
    
    dashboardHeader(title = div(img(height = 50, width = 50, src = "covid19.png"))),
    
    
    dashboardSidebar(
        
        collapsed = TRUE,
        
        sidebarMenu(id = "menu",
                    menuItem("Forside", tabName = "forside", icon = icon("archway")),
                    menuItem("Nye indlæggelser pr region", tabName = "nøgletal", icon = icon("bed"),
                             menuSubItem("Hovedstaden", tabName = "kbh", icon = icon("city")),
                             menuSubItem("Nordjylland", tabName = "njy", icon = icon("city")),
                             menuSubItem("Sjælland", tabName = "sjl", icon = icon("city")),
                             menuSubItem("Syddanmark", tabName = "sdm", icon = icon("city")),
                             menuSubItem("Turister", tabName = "tur", icon = icon("walking"))))),
    
    
    dashboardBody(
        tabItems(
        tabItem(tabName = "kbh",
                fluidRow(valueBoxOutput(width = 4, "kbh")),
                fluidRow(
                    tabBox(title = "Tendens", id = "tendens1", width = 12,
                           tabPanel("Indlæggelser",
                                    fluidRow(column(width = 4, box("Vælg datoer", status = "primary", width = NULL,
                                                                   solidHeader = TRUE,
                                                                   dateRangeInput(inputId = "rt_Date", "",
                                                                                  start = Sys.Date()-30,
                                                                                  end = Sys.Date(),
                                                                                  max = Sys.Date(),
                                                                                  startview = "month",
                                                                                  weekstart = 1),
                                                                   checkboxInput("trend_line", label = "Plot en tendenslinje", value = TRUE),
                                                                   submitButton("Lav ændringer")))),
                                    fluidRow(column(width = 12, box(title = "Indlæggelser over tid", status = "primary", width = NULL,
                                                                    plotOutput("na_tendens"))))))
                )),
        tabItem(tabName = "njy",
                fluidRow(valueBoxOutput(width = 4, "njy")),
                fluidRow(
                    tabBox(title = "tendens", id = "tendens2", width = 12,
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
                )),
        tabItem(tabName = "sjl",
                fluidRow(valueBoxOutput(width = 4, "sjl")),
                fluidRow(
                    tabBox(title = "tendens", id = "tendens3", width = 12,
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
                )),
        tabItem(tabName = "sdm",
                fluidRow(valueBoxOutput(width = 4, "sdm")),
                fluidRow(
                    tabBox(title = "tendens", id = "tendens4", width = 12,
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
                )),
        tabItem(tabName = "tur",
                fluidRow(valueBoxOutput(width = 4, "tur")),
                fluidRow(
                    tabBox(title = "tendens", id = "tendens5", width = 12,
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
                )))
    )
    
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$kbh <- renderValueBox({
        
        last_row <- tail(na, n = 1)
            
        valueBox(value = last_row$Hovedstaden, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "yellow")
    })
    
    output$na_tendens <- renderPlot({
        
        Na <- na %>%
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(Na, aes(as.Date(Dato), Hovedstaden)) +
            geom_line() +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
        
    })
    
    output$njy <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Nordjylland, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "blue")
    })
    
    output$sjl <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Sjælland, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "green")
    })
    
    output$sdm <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Syddanmark, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "red")
    })
    
    output$tur <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$`Ukendt Region`, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "black")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
