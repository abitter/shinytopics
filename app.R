#
# Shiny Topics
##############
# André Bittermann, ZPID, Trier
#
# This app displays research topics in psychology
# identified using topic modeling of PSYNDEX data
#    
# Reference: Bittermann & Fischer (2018), doi: https://doi.org/10.1027/2151-2604/a000318


# packages ----
library(shiny)
library(lattice)
library(DT)

# data ----
theta_year <- readRDS("data/theta_year.rds")
theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds")
theta_mean_by_year_time <- readRDS("data/theta_mean_by_year_time.rds")
theta_mean_by_year_ts <- readRDS("data/theta_mean_by_year_ts.rds")
years <- readRDS("data/years.rds")
topic <- readRDS("data/topic.rds")
#topdocs_string <- readRDS("data/topdocs_string.rds")


# sources ----
source("trends.R")

# Helper function ----
# https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
createLink <- function(val) {
  val <- gsub(', ','" OR "' , val)
  val <- paste0('"', val, '"')
  paste0("<a href='https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28CT%3D%28", 
         val,"%29%29+DB%3DPSYNDEX&stats=TOP' target='_blank' class='btn btn-primary'>Suche in PSYNDEX</a>")
}


# Define UI ----
ui <- fluidPage(
   
  # Application title
   titlePanel("Shiny Topics v0.3.3"),
   
   # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
       
       #helpText("Wählen Sie das Jahr oder den Zeitraum (1980–2017)"),
       
       numericInput("year", 
                    label = h4("Jahr:"),
                    value = as.numeric(years[length(years)]), 
                    min = 1980, 
                    max = as.numeric(years[length(years)])),
       
       # slider color
       #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #a2b21e}")),
       
       sliderInput("range",
                    label = h4("Zeitraum:"),
                    min = 1980,
                    max = as.numeric(years[length(years)]),
                    value = c(1980, as.numeric(years[length(years)])),
                    sep = "",
                    ticks = FALSE),
       
       actionButton("reset", "Reset"),
       br(),
       br(),
       helpText(br(),
                p("Die Themen der psychologischen Fachliteratur aus dem deutschsprachigen Raum wurden mit Hilfe von",
                a("Topic Modeling", href = "https://doi.org/10.1027/2151-2604/a000318", target="_blank"),
                "identifiziert, basierend auf der Referenzdatenbank",
                a("PSYNDEX.", href = "https://www.psyndex.de", target="_blank")),
                br(),
                p("Die Themen basieren auf den",  em("PSYNDEX Terms,"), 
                  "welche", a("hier", href = "https://www.psyndex.de/index.php?wahl=products&uwahl=printed&uuwahl=psyndexterms", 
                              target="_blank"), "eingesehen werden können.")
                ),
       
       br(),
       helpText(a("Feedback", href = "https://leibniz-psychology.org/mitarbeiter/profil-andre-bittermann/", target="_blank")),
       br(),
       br(),
       a(img(src = "logo.png", height = "75%", width = "75%"), href = "https://www.leibniz-psychology.org", target="_blank")
       ),
      
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
                   DT::dataTableOutput("hotterms")
                   ),
          tabPanel("Cold Topics", 
                   br(),
                   plotOutput("cold"),
                   br(),
                   DT::dataTableOutput("coldterms")
                   ),
          tabPanel("Alle Themen", 
                   br(),
                   plotOutput("topicplot"),
                   #h3(strong("Liste aller Themen:")),
                   #br(),
                   br(),
                   DT::dataTableOutput("topiclist")
          ),
        type = "tabs")
        
      )
   )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # slider reset
  observeEvent(input$reset,{
    updateSliderInput(session,'range', 
                      value = c(1980, as.numeric(years[length(years)])))
  })
  
  # transform invalid year input
  finalInput <- reactive({
    if (input$year < 1980) return(1980)
    if (input$year > 2017) return(2017)
    input$year
  })
  
  # clickable selection of data table rows
  select <- reactive({
    if (is.null(input$topiclist_rows_selected) == TRUE) return(1)
    input$topiclist_rows_selected
  })
  
  # trends function
  trends <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979, 
              theta_year, theta_mean_by_year, theta_mean_by_year_time, theta_mean_by_year_ts, years, topic)
  })

  ranger <- reactive({
    paste0(input$range[1]-1979,":",input$range[2]-1979)
  })
  
  ### plots ###
  
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
           ylim = c(0,0.05),
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
           ylim = c(0,0.05),
           ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           xlab = "",
           type = c("l", "g", "r"),
           scales = list(x = list(alternating = FALSE), tck=c(1,0), x=list(cex=1), y=list(cex=0.6)),
           main = list(paste0("Cold Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background=list(col="steelblue3")))
  }, res=125)
  
  output$topicplot <- renderPlot({
    #xyplot(theta_mean_by_year_ts[,select()], # fixed total time interval
    xyplot(window(theta_mean_by_year_ts, input$range[1], c(input$range[1], input$range[2]-input$range[1]+1))[,select()],
           col = c("black"),
           ylim = c(0,0.05),
           ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck=c(1,0), x=list(cex=1), y=list(cex=0.6), tick.number = 10),
           main = list(paste("Zeitlicher Verlauf von Thema", select()), cex = 1),
           par.settings = list(strip.background=list(col="steelblue3")))
  }, res=125, width = 600)
  
  
  ### data tables ##
  
  output$hotterms <- DT::renderDataTable({
    table_hot <- trends()[[1]]
    table_hot$Recherche <- createLink(table_hot$Thema)
    return(table_hot)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "none"),
    options = list(pageLength = 10, lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  output$coldterms <- DT::renderDataTable({
    table_cold <- trends()[[1]]
    table_cold$Recherche <- createLink(table_cold$Thema)
    return(table_cold)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "none"),
    options = list(pageLength = 10, lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  output$topiclist <- DT::renderDataTable({
    topic$Recherche <- createLink(topic$Thema)
    topic[,3] <- round(topic[,3], 4)
    return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE,
    options = list(pageLength = 10, sDom = '<"top">flrt<"bottom">ip', searchHighlight = TRUE))
  
  
  ### Dynamic Search in PubPsych ###
  #output$link <- renderUI({
  #  a(paste("Literatur zu Thema", select(), "in PSYNDEX"), 
  #    href=paste0("https://pubpsych.zpid.de/pubpsych/Search.action?search=&q=%28DFK%3D%28", 
  #                topdocs_string[[select()]],
  #                "%29%29+DB%3DPSYNDEX&stats=TOP"), target="_blank")
  #  })

}

# Run the application ----
shinyApp(ui = ui, server = server)

