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

# Define UI ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Shiny Topics v0.1"),
   
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
                    value = c(2010, 2016),
                    sep="",
                    ticks = FALSE),
       
       helpText(br(),
                p("Die Forschungsthemen der Psychologie aus dem deutschsprachigen Raum wurden mit Hilfe von",
                a("Topic Modeling", href = "https://doi.org/10.1027/2151-2604/a000318", target="_blank"),
                "identifiziert, basierend auf der Referenzdatenbank",
                a("PSYNDEX.", href = "https://www.psyndex.de", target="_blank")
                )),
       
       br(),
       a(img(src = "logo.png", height = 328/7, width = 1445/7), href = "https://www.leibniz-psychology.org", target="_blank")
       
      ),
      
      # Main Panel
      mainPanel(#width = 12,
        #verbatimTextOutput("value"),
        plotOutput("topicchart"),
        br(),
        plotOutput("topicchart2")
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
  
  #output$value <- renderPrint({finalInput()})
  
  output$topicchart <- renderPlot({
    barchart(head(sort(theta_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1], 
             col = "#230d46ff", 
             main = list(paste("Populäre Forschungsthemen im Jahr", finalInput()), cex = 2),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.2))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    barchart(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[10:1],
             col = "#230d46ff", 
             main = list(paste0("Populäre Forschungsthemen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 2),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.2))) # label font size
  })
   
}

# Run the application ----
shinyApp(ui = ui, server = server)

