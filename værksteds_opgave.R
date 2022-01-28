library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinymanager)

na <- read_csv2("Newly_admitted_over_time.csv")
dot <- read_csv2("Deaths_over_time.csv")

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
                fluidRow(valueBoxOutput(width = 4, "kbh"),
                         valueBoxOutput(width = 4, "kbha"),
                         valueBoxOutput(width = 4, "kbhb")),
                br(),
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
                                                                    plotOutput("na_tendens"))))),
                           
                           tabPanel("Dødsfald",
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
                                    fluidRow(column(width = 12, box(title = "Dødsfald over tid", status = "primary", width = NULL,
                                                                    plotOutput("dot_tendens"))))))
                )),
        
        tabItem(tabName = "njy",
                fluidRow(valueBoxOutput(width = 4, "njy"),
                         valueBoxOutput(width = 4, "nja"),
                         valueBoxOutput(width = 4, "njb")),
                fluidRow(
                    tabBox(title = "Tendens", id = "tendens2", width = 12,
                           tabPanel("Indlæggelser",
                                    fluidRow(column(width = 4, box("Vælg datoer", status = "primary", width = NULL,
                                                                   solidHeader = TRUE,
                                                                   dateRangeInput(inputId = "rt_Date", "",
                                                                                  start = Sys.Date()-30,
                                                                                  end = Sys.Date(),
                                                                                  max = Sys.Date(),
                                                                                  startview = "month",
                                                                                  weekstart = 1),
                                                                   checkboxInput("trend_line", label = "Plot en tendenslinje", value = FALSE),
                                                                   submitButton("Lav ændringer")))),
                                    fluidRow(column(width = 12, box(title = "Indlæggelser over tid", status = "primary", width = NULL,
                                                                    plotOutput("aa_tendens"))))))
                )),
        
        tabItem(tabName = "sjl",
                fluidRow(valueBoxOutput(width = 4, "sjl"),
                         valueBoxOutput(width = 4, "sja"),
                         valueBoxOutput(width = 4, "sjb")),
                fluidRow(
                    tabBox(title = "Tendens", id = "tendens3", width = 12,
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
                                                                    plotOutput("ab_tendens"))))))
                )),
        
        tabItem(tabName = "sdm",
                fluidRow(valueBoxOutput(width = 4, "sdm"),
                         valueBoxOutput(width = 4, "sda"),
                         valueBoxOutput(width = 4, "sdb")),
                fluidRow(
                    tabBox(title = "Tendens", id = "tendens4", width = 12,
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
                                                                    plotOutput("bb_tendens"))))))
                )),
        
        tabItem(tabName = "tur",
                fluidRow(valueBoxOutput(width = 4, "tur"),
                         valueBoxOutput(width = 4, "tua"),
                         valueBoxOutput(width = 4, "tub")),
                fluidRow(
                    tabBox(title = "Tendens", id = "tendens5", width = 12,
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
                                                                    plotOutput("bc_tendens"))))))
                )))
    )
)


# Server --------------------------------------------------------------------------------------


# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$kbh <- renderValueBox({
        
        last_row <- tail(na, n = 1)
            
        valueBox(value = last_row$Hovedstaden, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "yellow")
    })
    output$kbha <- renderValueBox({
        
        last_row <- tail(dot, n = 1)
        
        valueBox(value = last_row$Antal_døde, "",
                 subtitle = paste0("Antal døde ", last_row$Dato), icon = icon("skull"), color = "blue")
    })
    output$kbhb <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Hovedstaden, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "red")
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
    
    output$dot_tendens <- renderPlot({
        
        aa <- slice(dot, 1:(n() - 1)) %>% 
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(aa, aes(as.Date(Dato), Antal_døde)) +
            geom_line() +
            labs(x = "Dato",
                 Y = "Antal_døde")
        
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
    output$nja <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Nordjylland, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "yellow")
    })
    output$njb <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Nordjylland, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "red")
    })
    
    output$aa_tendens <- renderPlot({
        
        Na <- na %>%
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(Na, aes(as.Date(Dato), Nordjylland)) +
            geom_line() +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
        
    })
    
    output$sjl <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Sjælland, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "green")
    })
    output$sja <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Sjælland, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "blue")
    })
    output$sjb <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Sjælland, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "red")
    })
    
    output$ab_tendens <- renderPlot({
        
        Na <- na %>%
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(Na, aes(as.Date(Dato), Sjælland)) +
            geom_line() +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
        
    })
    
    output$sdm <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Syddanmark, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "red")
    })
    output$sda <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Syddanmark, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("car"), color = "blue")
    })
    output$sdb <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$Syddanmark, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("syringe"), color = "green")
    })
    
    output$bb_tendens <- renderPlot({
        
        Na <- na %>%
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(Na, aes(as.Date(Dato), Syddanmark)) +
            geom_line() +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
        
    })
    
    output$tur <- renderValueBox({
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$`Ukendt Region`, "",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bed"), color = "black")
    })
    output$tua <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$`Ukendt Region`, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("walking"), color = "blue")
    })
    output$tub <- renderValueBox({
        
        last_row <- tail(na, n = 1)
        
        valueBox(value = last_row$`Ukendt Region`, "nye",
                 subtitle = paste0("nye indlagte pr. ", last_row$Dato), icon = icon("bus"), color = "red")
    })
    
    output$bc_tendens <- renderPlot({
        
        Na <- na %>%
            filter(Dato >= as.Date(input$rt_Date[1]) & Dato <= as.Date(input$rt_Date[2]))
        
        p <- ggplot(Na, aes(as.Date(Dato), `Ukendt Region`)) +
            geom_line() +
            labs(x = "Dato",
                 y = "Kontakttal")
        
        if(input$trend_line) {
            
            p <- p + geom_smooth(se = FALSE)
        }
        
        print(p)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
