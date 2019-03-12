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
#suppressPackageStartupMessages( if (!require("pacman")) install.packages("pacman") )
#pacman::p_load(shiny, shinyWidgets, forecast, nnet, lattice, DT, plotrix)

library(shiny)
library(shinyWidgets)
library(forecast)
library(nnet)
library(lattice)
library(DT)
library(plotrix)


# data ----
theta_year <- readRDS("data/theta_year.rds")
theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds")
theta_mean_by_year_time <- readRDS("data/theta_mean_by_year_time.rds")
theta_mean_by_year_ts <- readRDS("data/theta_mean_by_year_ts.rds")
years <- readRDS("data/years.rds")
topic <- readRDS("data/topic.rds")
booster <- readRDS("data/booster.rds")


# sources ----
source("trends.R")
source("links.R")
source("quantqual.R")


# function for aligning slider
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el, style="margin-left:auto;margin-right:auto;")
}


# colors ----

col_bars <- "#0094c5"
col_highlight <- "gold"
# colors in tags$style have to be set manually in respective lines

  
# Define UI ----
ui <- fluidPage(
  
  # color of selected row; https://www.w3schools.com/colors/colors_names.asp
  tags$style(HTML('table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:gold !important;}')),
  
  # move position of output objects
  #tags$style(type='text/css', "#circleplot { width:100%; margin-top: 15px;}"),
  
  # color of clear search button
  #tags$style(HTML('#reset2{background-color:lightgrey}')),
  
  # Application title
   titlePanel("Shiny Topics v0.6"),
  
     # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
       
       # slider colors (add line for every slider)
       tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0094c5}")),
       tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #0094c5}")),
       
       #numericInput("year", #width = "50%",
       #            label = h4("Jahr:"),
       #           value = as.numeric(years[length(years)]), 
       #          min = 1980, 
       #         max = as.numeric(years[length(years)])),
       
       sliderInput("range",
                    label = h4("Zeitraum festlegen:"),
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
          # slider color
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
                          plotOutput("circleplot")),
                   br(),
                   searchInput(
                     inputId = "searchbox", label = "Suche nach Themen",
                     placeholder = "Bitte Suchbegriff eingeben",
                     btnSearch = icon("search"),
                     btnReset = icon("remove"),
                     width = "450px"),
                   #p(actionButton("reset2", strong("Suche löschen")), align = "right"),
                   DT::dataTableOutput("topiclist")
                   ),
          tabPanel("Erwartete Verläufe", 
                   column(6, 
                          br(),
                          br(),
                          h4("Vergleichen Sie den beobachteten mit dem erwarteten Verlauf ab einem gewünschten Zeitpunkt."),
                          br(),
                          br(),
                          searchInput(
                            inputId = "searchbox2", label = "Suche nach Themen",
                            placeholder = "Bitte Suchbegriff eingeben",
                            btnSearch = icon("search"),
                            btnReset = icon("remove"),
                            width = "450px"),
                          br(),
                          DT::dataTableOutput("eventtable")),
                   column(6,
                          br(),
                          plotOutput("eventplot"),
                          #br(),
                          #verbatimTextOutput(outputId = "info"),
                          alignCenter(sliderInput("year",
                                      label = "Zeitpunkt festlegen:",
                                      min = 1981,
                                      max = as.numeric(years[length(years)]) - 2,
                                      value = 2000,
                                      sep = "",
                                      width = "80%",
                                      ticks = FALSE)))
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
  
  # search box input to lower case
  search_lower <- reactive({
    tolower(input$searchbox)
  })
  search_lower2 <- reactive({
    tolower(input$searchbox2)
  })
  
  # info box for development
  output$info <- renderPrint({
   "test"
  })
  
  # transform invalid year input
  finalInput <- reactive({
    if (input$year < 1981) return(1981)
    if (input$year > 2015) return(2015)
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
  select_event <- reactive({
    if (is.null(input$eventtable_rows_selected) == TRUE) return(1) # show plot for Topic 1 in case no row is selected
    input$eventtable_rows_selected
  })
 
  
  # trends function
  trends <- reactive({
    trends.ab(input$range[1]-1979, input$range[2]-1979, 
              theta_year, theta_mean_by_year, theta_mean_by_year_time, theta_mean_by_year_ts, years, topic)
  })

  
  ### plots ###
  
  # default color for barchart and background of xyplots headers
  colors <- rep(col_bars, 10)
  
  output$topicchart <- renderPlot({
    colors[(11-select_popular())] <- col_highlight
    #barchart(head(sort(theta_mean_by_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1],
    barchart(head(sort(theta_mean_by_year[as.character(input$range[2]), ], decreasing = TRUE), 10)[10:1], 
             col = colors, 
             main = list(paste("Populäre Themen im Jahr", input$range[2]), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    colors[(11-select_popular_range())] <- col_highlight
    barchart(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[10:1],
             col = colors, 
             main = list(paste0("Populäre Themen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$hot <- renderPlot({
    colors[select_hot()] <- col_highlight
    xyplot(trends()[[3]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.04),
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
    colors[select_cold()] <- col_highlight
    xyplot(trends()[[4]],
           layout = c(5,2),
           col = c("black"),
           ylim = c(0,0.04),
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
           col = col_bars,
           ylim = c(0,0.04),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           ylab = list("Prävalenz", cex=0.6),
           xlab = "",
           type = c("l", "g"),
           lwd = 3,
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste("Zeitlicher Verlauf von Thema", select()), cex = 1),
           par.settings = list(strip.background = list(col = col_bars)))
  }, res=125)
  
  output$circleplot <- renderPlot({
    plot(1, xlab="", ylab="", xaxt='n', yaxt='n', asp = 1, xlim = c(0.6, 1.4), ylim = c(0.6, 1.4),
         #main = list(paste0("Prävalenz von Thema ", select(), ": ", round(topic[select(),3], 4)), par(cex.main = 1)), type="n")
         main = list(paste0("Prävalenz von Thema ", select(), " im Vergleich "), par(cex.main = 1)), type="n")
    plotrix::draw.circle(1, 1, topic[select(),3]*16, col=col_highlight, border=col_highlight) # current topic
    plotrix::draw.circle(1, 1, (1/(dim(topic)[1]))*16, border=col_bars, col="white", lty="solid", density=0) # average
    plotrix::draw.circle(1, 1, max(topic[,3])*16, border="black", col="white", lty="dotted", density=0) # max
    #plotrix::draw.circle(1, 1, min(topic[,3])*10, border="black", col="white", lty="solid", density=0) # min
    legend("bottomright",
           legend=c("Maximum", "Durchschnitt"), 
           col=c("black", col_bars), 
           lty=c("dotted", "solid"), 
           cex=0.6)
  }, res=125)
  
  output$eventplot <- renderPlot({
    inp <- topic[(grepl(search_lower2(), topic$Thema)),][select_event(), 1]
    # linear regression
    #years2 <- 1980:finalInput()
    #lm1 <- lm(theta_mean_by_year[1:(finalInput()-1980+1), inp] ~ years2)
    #linreg <- list()
    #linreg[[1]] <- lm1$fitted.values
    #linreg[[2]] <- lm1$fitted.values[length(lm1$fitted.values)] + lm1$coefficients[2]
    #for (i in 3:(length(theta_mean_by_year[,1]) - length(lm1$fitted.values) + 1)){
    #  linreg[[i]] <- linreg[[i-1]] + lm1$coefficients[2]
    #}
    #linreg <- unlist(linreg)
    #names(linreg) <- years
    #
    #plot(window(theta_mean_by_year_ts, input$range[1], c(input$range[1], input$range[2]-input$range[1]+1))[, inp], 
    #     type = "l", col = col_bars, ylab = list("Prävalenz", cex = 0.8), xlab = "", lwd = 3, #ylim = c(0, 0.04),
    #     main = list(paste("Zeitlicher Verlauf von Thema", inp), cex = 1.25))
    #lines(x = years2, y = lm1$fitted.values, col = "red", lwd = 2)
    #lines(x = years, y = linreg, col = "red", lwd = 2, lty = "dashed")
    #
    # forecast with MLP #
    window <- window(theta_mean_by_year_ts[, inp], start = 1980, end = finalInput())
    mlp <- plotXY(1:length(window), window, complexity = 2)
    mlp_ts <- ts(mlp$prediction, start = 1980)
    forecast <- forecast(mlp_ts, h = length(theta_mean_by_year_ts[, inp]) - length(window))
    plot(forecast, ylim = c(0, max(theta_mean_by_year_ts[,inp])), showgap = FALSE, PI = TRUE,
         main = list(paste("Beobachteter und erwarteter Verlauf von Thema", inp), cex = 1.25), # remove main to see method
         col = col_bars, fcol = "#83227a")
    lines(theta_mean_by_year_ts[,inp])
    grid(NULL, NULL, lty = "solid", col = "lightgrey")
    abline(v = finalInput(), lty = "dashed", col = col_bars, lwd = 2)
  }, res = 100)
  
  
  ### data tables ##
  
  # options applied to all data tables
  options(DT.options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/9dcbecd42ad/i18n/German.json')))
  
  # popular by year #
  output$popular <- DT::renderDataTable({
    table_popular <- as.data.frame(head(sort(theta_year[as.character(input$range[2]), ], decreasing = TRUE), 10)[1:10])
    table_popular$Thema <- rownames(table_popular)
    table_popular$Rang <- 1:10
    table_popular$NR <- as.numeric(names(head(sort(theta_mean_by_year[as.character(input$range[2]), ], decreasing = TRUE), 10)[1:10]))
    rownames(table_popular) <- NULL
    table_popular[ ,c(1,2,3,4)] <- table_popular[ ,c(3,4,2,1)]
    names(table_popular) <- c("Rang", "Nr.", "Thema", "Prävalenz")
    table_popular[,4] <- round(table_popular[,4], 4)
    table_popular$Recherche <- createLink(table_popular$Thema, booster)
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
    table_popular_range$Recherche <- createLink(table_popular_range$Thema, booster)
    return(table_popular_range)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # hot topics #
  output$hotterms <- DT::renderDataTable({
    table_hot <- trends()[[1]]
    table_hot$Recherche <- createLink(table_hot$Thema, booster)
    names(table_hot)[2] <- ("Nr.")
    return(table_hot)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # cold topic #
  output$coldterms <- DT::renderDataTable({
    table_cold <- trends()[[2]]
    table_cold$Recherche <- createLink(table_cold$Thema, booster)
    names(table_cold)[2] <- ("Nr.")
    return(table_cold)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # all topics #
  output$topiclist <- DT::renderDataTable({
    topic <- topic[(grepl(search_lower(), topic$Thema)),]
    topic$Recherche <- createLink(topic$Thema, booster)
    topic[,3] <- round(topic[,3], 4)
    names(topic)[1] <- ("Nr.")
    return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe',
    #options = list(sDom = '<"top">flrt<"bottom">ip', searchHighlight = TRUE))
    options = list(lengthChange = TRUE, info = TRUE, paging = TRUE, searching = FALSE))
  # for clear search button:
  #proxy <- dataTableProxy("topiclist")
  
  # event selection #
  output$eventtable <- DT::renderDataTable({
    list <- topic[(grepl(search_lower2(), topic$Thema)),][,-3]
    names(list)[1] <- ("Nr.")
    return(list)
  }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))


}


# Run the application ----
shinyApp(ui = ui, server = server)

