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
suppressPackageStartupMessages( if (!require("pacman")) install.packages("pacman") )
pacman::p_load(shiny, lattice, DT, plotrix)


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
source("links.R")


# Define UI ----
ui <- fluidPage(
  
  # color of selected row; https://www.w3schools.com/colors/colors_names.asp
  tags$style(HTML('table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:gold !important;}')),
  
  # move position of output objects
  #tags$style(type='text/css', "#circleplot { width:100%; margin-top: 0px;}"),
  tags$style(type='text/css', "#circleplot { width:100%; margin-top: 0px;}"),
  
  # color of clear search button
  #tags$style(HTML('#reset2{background-color:lightgrey}')),
  
  # Application title
   titlePanel("Shiny Topics v0.4.2"),
   
   # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
                     
       numericInput("year", #width = "50%",
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
          tabPanel("Themen eines Jahres", 
                   br(),
                   plotOutput("topicchart"),
                   br(),
                   DT::dataTableOutput("popular"),
                   br()
                   ), 
          tabPanel("Themen eines Zeitraums", 
                   br(),
                   plotOutput("topicchart2"),
                   br(),
                   DT::dataTableOutput("popularrange"),
                   br()
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
                   column(6, 
                          br(),
                          plotOutput("topicplot")),
                   column(6,
                          #br(),
                          plotOutput("circleplot")),
                   #br(),
                   #plotOutput("topicplot"),
                   p(actionButton("reset2", strong("Suche löschen")), align = "right"),
                   DT::dataTableOutput("topiclist")
          ),
        type = "tabs")
        
      )
   )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # reset buttons
  # slider reset
  observeEvent(input$reset,{
    updateSliderInput(session,'range', 
                      value = c(1980, as.numeric(years[length(years)])))
  })
  # search reset
  observeEvent(input$reset2,{
    clearSearch(proxy)
  })
  
  # transform invalid year input
  finalInput <- reactive({
    if (input$year < 1980) return(1980)
    if (input$year > 2017) return(2017)
    input$year
  })
  
  # clickable selection of data table rows
  select <- reactive({
    if (is.null(input$topiclist_rows_selected) == TRUE) return(1) # show plot for Topic 1 in case no row is selected
    input$topiclist_rows_selected
  })
  select_popular <- reactive({
    if (is.null(input$popular_rows_selected) == TRUE) return(0) # 0, so no plot is highlighted when row in data table is unselected
    input$popular_rows_selected
  })
  select_popular_range <- reactive({
    if (is.null(input$popularrange_rows_selected) == TRUE) return(0)
    input$popularrange_rows_selected
  })
  select_hot <- reactive({
    if (is.null(input$hotterms_rows_selected) == TRUE) return(0)
    input$hotterms_rows_selected
  })
  select_cold <- reactive({
    if (is.null(input$coldterms_rows_selected) == TRUE) return(0)
    input$coldterms_rows_selected
  })
 
  
  # trends function
  trends <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979, 
              theta_year, theta_mean_by_year, theta_mean_by_year_time, theta_mean_by_year_ts, years, topic)
  })

  
  ### plots ###
  
  # default color for barchart and background of xyplots headers
  colors <- rep("steelblue3", 10)
  
  output$topicchart <- renderPlot({
    colors[(11-select_popular())] <- "gold"
    barchart(head(sort(theta_mean_by_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1], 
             col = colors, 
             main = list(paste("Populäre Themen im Jahr", finalInput()), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    colors[(11-select_popular_range())] <- "gold"
    barchart(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[10:1],
             col = colors, 
             main = list(paste0("Populäre Themen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$hot <- renderPlot({
    colors[select_hot()] <- "gold"
    xyplot(trends()[[3]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.05),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex = 0.6),
           ylab = list("Prävalenz", cex = 0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste0("Hot Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background = list(col = colors)),
           strip = function(..., bg) { # http://r.789695.n4.nabble.com/lattice-change-background-strip-color-in-one-panel-td3554612.html
             strip.default(..., 
                           bg = trellis.par.get("strip.background")$col[which.packet()]
                           )}) 
  }, res=125)
  
  output$cold <- renderPlot({
    colors[select_cold()] <- "gold"
    #ticks <- ifelse((input$range[2]-input$range[1])<3, 1, 6)
    xyplot(trends()[[4]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.05),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex = 0.6),
           ylab = list("Prävalenz", cex = 0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste0("Cold Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background = list(col = colors)),
           strip = function(..., bg) {
             strip.default(...,
                           bg = trellis.par.get("strip.background")$col[which.packet()]
                           )}) 
  }, res=125)
  
  output$topicplot <- renderPlot({
    #xyplot(theta_mean_by_year_ts[,select()], # fixed total time interval
    xyplot(window(theta_mean_by_year_ts, input$range[1], c(input$range[1], input$range[2]-input$range[1]+1))[,select()],
           col = "steelblue3",
           ylim = c(0,0.05),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           ylab = list("Prävalenz", cex=0.6),
           xlab = "",
           type = c("l", "g"),
           lwd = 3,
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste("Zeitlicher Verlauf von Thema", select()), cex = 1),
           par.settings = list(strip.background = list(col = "steelblue3")))
  }, res=125)
  
  output$circleplot <- renderPlot({
    plot(1, xlab="", ylab="", xaxt='n', yaxt='n', asp = 1, xlim = c(0.6, 1.4), ylim = c(0.6, 1.4),
         main = list(paste0("Prävalenz von Thema ", select(), " im Vegleich"), #": ", round(topic[select(),3], 4)), 
                     par(cex.main = 1)), type="n")
    plotrix::draw.circle(1, 1, topic[select(),3]*10, col="gold", border="gold") # current topic
    plotrix::draw.circle(1, 1, (1/(dim(topic)[1]))*10, border="steelblue3", col="white", lty="solid", density=0) # average
    plotrix::draw.circle(1, 1, max(topic[,3])*10, border="black", col="white", lty="dashed", density=0) # max
    #plotrix::draw.circle(1, 1, min(topic[,3])*10, border="black", col="white", lty="solid", density=0) # min
    legend("bottomright",
           legend=c("Maximum", "Durchschnitt"), 
           col=c("black", "steelblue3"), 
           lty=c("dashed", "solid"), 
           cex=0.6)
    
  }, res=125)
  
  
  ### data tables ##
  
  # options applied to all data tables
  options(DT.options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/9dcbecd42ad/i18n/German.json')))
  
  # popular by year #
  output$popular <- DT::renderDataTable({
    table_popular <- as.data.frame(head(sort(theta_year[as.character(finalInput()), ], decreasing = TRUE), 10)[1:10])
    table_popular$Thema <- rownames(table_popular)
    table_popular$Rang <- 1:10
    table_popular$NR <- as.numeric(names(head(sort(theta_mean_by_year[as.character(finalInput()), ], decreasing = TRUE), 10)[1:10]))
    rownames(table_popular) <- NULL
    table_popular[ ,c(1,2,3,4)] <- table_popular[ ,c(3,4,2,1)]
    names(table_popular) <- c("Rang", "Nr.", "Thema", "Prävalenz")
    table_popular[,4] <- round(table_popular[,4], 4)
    table_popular$Recherche <- createLink(table_popular$Thema)
    return(table_popular)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # popular in range of years #
  output$popularrange <- DT::renderDataTable({
    table_popular_range <- as.data.frame(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10])
    table_popular_range$Thema <- rownames(table_popular_range)
    table_popular_range$Rang <- 1:10
    table_popular_range$NR <- as.numeric(names(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10]))
    rownames(table_popular_range) <- NULL
    table_popular_range[ ,c(1,2,3,4)] <- table_popular_range[ ,c(3,4,2,1)]
    names(table_popular_range) <- c("Rang", "Nr.", "Thema", "Prävalenz")
    table_popular_range[,4] <- round(table_popular_range[,4], 4) 
    table_popular_range$Recherche <- createLink(table_popular_range$Thema)
    return(table_popular_range)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # hot topics #
  output$hotterms <- DT::renderDataTable({
    table_hot <- trends()[[1]]
    table_hot$Recherche <- createLink(table_hot$Thema)
    names(table_hot)[2] <- ("Nr.")
    return(table_hot)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # cold topic #
  output$coldterms <- DT::renderDataTable({
    table_cold <- trends()[[2]]
    table_cold$Recherche <- createLink(table_cold$Thema)
    names(table_cold)[2] <- ("Nr.")
    return(table_cold)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # all topics #
  output$topiclist <- DT::renderDataTable({
    topic$Recherche <- createLink(topic$Thema)
    topic[,3] <- round(topic[,3], 4)
    names(topic)[1] <- ("Nr.")
    return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe',
    options = list(sDom = '<"top">flrt<"bottom">ip', searchHighlight = TRUE))
  # for clear search button:
  proxy <- dataTableProxy("topiclist")
  
  
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

