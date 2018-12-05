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
theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds")
theta_mean_by_year_time <- readRDS("data/theta_time.rds")
theta_mean_by_year_ts <- readRDS("data/theta_ts.rds")
years <- readRDS("data/years.rds")

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
       
       # slider color
       #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #a2b21e}")),
       
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
                   dataTableOutput("hotterms")
                   ),
          tabPanel("Cold Topics", 
                   br(),
                   plotOutput("cold"),
                   br(),
                   dataTableOutput("coldterms")
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
  
  trends <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979, 
              theta_year, theta_mean_by_year, theta_mean_by_year_time, theta_mean_by_year_ts, years)
  })
  
  #output$value <- renderPrint({finalInput()})
  
  output$topicchart <- renderPlot({
    barchart(head(sort(theta_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1], 
             col = "steelblue3", 
             main = list(paste("Populäre Forschungsthemen im Jahr", finalInput()), cex = 1.75),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.1))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    barchart(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[10:1],
             col = "steelblue3", 
             main = list(paste0("Populäre Forschungsthemen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 1.75),
             xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.1))) # label font size
  })
  
  output$hot <- renderPlot({
    xyplot(trends()[[3]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.015),
           ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           xlab = "",
           type = c("l", "g", "r"),
           scales = list(x = list(alternating = FALSE), tck=c(1,0), x=list(cex=1), y=list(cex=0.6)),
           main = list(paste0("Hot Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background = list(col="steelblue3")))
  }, res=125)
  
  output$cold <- renderPlot({
    xyplot(trends()[[4]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.015),
           ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           xlab = "",
           type = c("l", "g", "r"),
           scales = list(x = list(alternating = FALSE), tck=c(1,0), x=list(cex=1), y=list(cex=0.6)),
           main = list(paste0("Cold Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background=list(col="steelblue3")))
  }, res=125)
  
  #output$hotterms <- renderPrint({trends()[[1]]})
  output$hotterms <- renderDataTable({trends()[[1]]}, options = list(pageLength = 10, lengthChange = FALSE, info = FALSE))
  output$coldterms <- renderDataTable({trends()[[2]]}, options = list(pageLength = 10, lengthChange = FALSE, info = FALSE))

}

# Run the application ----
shinyApp(ui = ui, server = server)

