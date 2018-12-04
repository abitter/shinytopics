#
# Hot Topics Shiny App
######################
# André Bittermann, ZPID, Trier
#
# This app displays research topics in psychology
# identified using topic modeling of PSYNDEX data
#    
# Reference: Bittermann & Fischer (2018), doi: https://doi.org/10.1027/2151-2604/a000318


# packages ----
library(shiny)
library(lattice)

# data ----
theta_year <- readRDS("data/theta_year.rds")

# sources ----
source("trends.R")


# Define UI ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Shiny Topics v0.2"),
   
   # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
       
       #helpText("Wählen Sie das Jahr oder den Zeitraum (1980–2016)"),
       
       numericInput("year", 
                    label = h4("Jahr:"),
                    value = 2016, 
                    min = 1980, 
                    max = 2016),
       
       sliderInput("range",
                    label = h4("Zeitraum:"),
                    min = 1980,
                    max = 2016,
                    value = c(1980, 2016),
                    sep = "",
                    ticks = FALSE),
       
       helpText(br(),
                p("Die Forschungsthemen der Psychologie aus dem deutschsprachigen Raum wurden mit Hilfe von",
                a("Topic Modeling", href = "https://doi.org/10.1027/2151-2604/a000318", target="_blank"),
                "identifiziert, basierend auf der Referenzdatenbank",
                a("PSYNDEX.", href = "https://www.psyndex.de", target="_blank")
                )),
       
       br(),
       #a(img(src = "logo.png", height = 328/7, width = 1445/7), href = "https://www.leibniz-psychology.org", target="_blank")
       a(img(src = "logo.png", height = "75%", width = "75%"), href = "https://www.leibniz-psychology.org", target="_blank")      ),
      
      # Main Panel
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel("Themen der Jahre", 
                   br(),
                   plotOutput("topicchart"),
                   br(),
                   plotOutput("topicchart2")
                   ), 
          tabPanel("Hot Topics", 
                   br(),
                   plotOutput("hot"),
                   br(),
                   verbatimTextOutput("hotterms")
                   ),
          tabPanel("Cold Topics", 
                   br(),
                   plotOutput("cold"),
                   br(),
                   verbatimTextOutput("coldterms")
                   ),
        type = "tabs")
        
      )
   )
)

# Define server logic ----
server <- function(input, output) {
  
  # transform invalid year input
  finalInput <- reactive({
    if (input$year < 1980) return(1980)
    if (input$year > 2016) return(2016)
    input$year
  })
  
  terms_hot <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979)[[1]]
  })
  
  terms_cold <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979)[[2]]
  })
  
  hot <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979)[[3]]
  })
  
  cold <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979)[[4]]
  })

  
  #output$value <- renderPrint({finalInput()})
  
  output$topicchart <- renderPlot({
    barchart(head(sort(theta_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1], 
             col = "#230d46ff", 
             main = list(paste("Populäre Forschungsthemen im Jahr", finalInput()), cex = 2),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.1))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    barchart(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[10:1],
             col = "#230d46ff", 
             main = list(paste0("Populäre Forschungsthemen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 2),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.1))) # label font size
  })
  
  output$hot <- renderPlot({
    xyplot(hot(),
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.015),
           ylab = expression(paste("Mean ",theta)),
           xlab = "Year",
           type = c("l", "g", "r"),
           scales = list(x = list(alternating = FALSE)),
           main = "Hot Topics")
  })
  
  output$cold <- renderPlot({
    xyplot(cold(),
           layout = c(5,2),
           col = "black",
           ylim = c(0,0.015),
           ylab = expression(paste("Mean ",theta)),
           xlab = "Year",
           type = c("l", "g", "r"),
           scales = list(x = list(alternating = FALSE)),
           main = "Cold Topics")
  })
  
  output$hotterms <- renderPrint({terms_hot()})
  output$coldterms <- renderPrint({terms_cold()})

}

# Run the application ----
shinyApp(ui = ui, server = server)

