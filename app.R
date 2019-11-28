#
# Shiny Topics
##############
# André Bittermann, ZPID, Trier
# abi@leibniz-psychology.org
#
# This app displays research topics in psychology
# identified using topic modeling of PSYNDEX data
#    
# Reference: Bittermann & Fischer (2018), doi: https://doi.org/10.1027/2151-2604/a000318


# packages ----
library(shiny)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(forecast)
library(nnet)
library(lattice)
library(DT)
library(plotrix)


# data ----
# this data is the result of topic modeling
# please contact the author if you need more information
theta_year <- readRDS("data/theta_year.rds") # theta_mean_by_year with labels instead of topic numbers
theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds") # mean theta of topic by year
theta_mean_by_year_time <- readRDS("data/theta_mean_by_year_time.rds") # for trend analysis
theta_mean_by_year_ts <- readRDS("data/theta_mean_by_year_ts.rds") # for trend analysis
years <- readRDS("data/years.rds") # a list of publication years
topic <- readRDS("data/topic.rds") # a list of topics and top terms
booster <- readRDS("data/booster.rds") # a table with factors for term boosting in PubPsych
k <- 325 # set number of topics in the model (all topics, not only the reliable ones)


# sources ----
source("trends.R")
source("links.R")
source("quantqual.R")


# function for aligning slider
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el, style = "margin-left:auto;margin-right:auto;")
}


# colors ----

col_bars <- "#0094c5"
col_highlight <- "gold"
# colors in tags$style have to be set manually in respective lines

  
# Define UI ----
ui <- fluidPage(
  
  # color of selected row; https://www.w3schools.com/colors/colors_names.asp
  tags$style(HTML('table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:gold !important;}')),
  
    # Tab color; https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel/43201952#43201952
  tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: #0094c5; color:white}")),
  
  # move position of output objects
  #tags$style(type='text/css', "#circleplot { width:100%; margin-top: 15px;}"),
  
  # color of clear search button
  #tags$style(HTML('#reset2{background-color:lightgrey}')),
  
  # Application title
   titlePanel("Shiny Topics v1.0.0"),
  
     # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
       
       # get screen size: https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny/37060206
       #tags$head(tags$script('
        #                        var dimension = [0, 0];
         #                       $(document).on("shiny:connected", function(e) {
          #                          dimension[0] = window.innerWidth;
           #                         dimension[1] = window.innerHeight;
            #                        Shiny.onInputChange("dimension", dimension);
             #                   });
              #                  $(window).resize(function(e) {
               #                     dimension[0] = window.innerWidth;
                #                    dimension[1] = window.innerHeight;
                 #                   Shiny.onInputChange("dimension", dimension);
                  #              });
                   #         ')),
                  
       # slider colors (add line for every slider)
       tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0094c5}")),
       tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #0094c5}")),
       tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #0094c5}")),

       
       # color of PSYNDEX search buttom
       # https://stackoverflow.com/questions/46232856/applying-2-different-css-styles-on-shiny-button
       tags$style(HTML(".btn {
                       color: #fff;
                       background-color: #0094c5;
                       border: 2px #0094c5 solid;
                       }
                       .btn:hover {
                       color: #fff;
                       background-color: #241b3e;
                       }
                       .btn-default.active, .btn-default:active, .open > .dropdown-toggle.btn-default {
                       color: #fff;
                       background-color: #0094c5;
                       border-color: #0094c5;
                       }
                       ")),
       
       useShinyalert(),
       
      # numericInput("yearpop", #width = "50%",
       #           label = h4("Jahr anzeigen:"),
        #          value = as.numeric(years[length(years)]), 
         #         min = 1980, 
          #        max = as.numeric(years[length(years)])),
   
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
                p("Die Themen der psychologischen Fachliteratur aus dem deutschsprachigen Raum wurden mit",
                a("Topic Modeling", href = "https://doi.org/10.1027/2151-2604/a000318", target="_blank"),
                "automatisiert aus",
                a("PSYNDEX.", href = "https://www.psyndex.de", target="_blank"), "erstellt."),
                #br(),
                p("Die Themen enthalten die", a(em("PSYNDEX Terms."), href = "https://www.psyndex.de/index.php?wahl=products&uwahl=printed&uuwahl=psyndexterms", 
                              target="_blank"))
                ),
       
       helpText("Die", strong(em("Prävalenz")), "beschreibt die durchschnittliche Wahrscheinlichkeit dieses Themas in den Publikationen in Prozent."),
       br(),
       helpText(a("Feedback", href = "https://forms.gle/bzsC6AJdTTBY3RDH8", target="_blank")),
       br(),
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
                   #br(),
                   sliderInput("yearpop",
                               label = "Jahr wählen",
                               min = 1980,
                               max = as.numeric(years[length(years)]),
                               value = years[length(years)],
                               sep = "",
                               #width = "80%",
                               ticks = FALSE),
                   #numericInput("yearpop", #width = "50%",
                    #                   label = strong("Jahr wählen:"),
                     #                  value = as.numeric(years[length(years)]), 
                      #                 min = 1980, 
                       #                max = as.numeric(years[length(years)])),
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
                          plotOutput("circleplot"),
                          p("Das Kreisdiagramm zeigt Ihnen an, ob ein Thema über- oder unterdurschnittlich 
                            stark in der psychologischen Fachliteratur aufgegriffen wird.", align = "center")
                          ),
                   br(),
                   searchInput(
                     inputId = "searchbox", label = "Suche nach Themen",
                     placeholder = "Bitte Suchbegriff eingeben",
                     btnSearch = icon("search"),
                     btnReset = icon("remove"),
                     width = "450px"),
                   p("Mögliche Suchbegriffe finden Sie", 
                     a("hier", href = "https://fremdauswerter.zpid.de/thesaurus.php", target="_blank")),
                   br(),
                   #p(actionButton("reset2", strong("Suche löschen")), align = "right"),
                   DT::dataTableOutput("topiclist")
                   ),
          tabPanel("Erwartete Verläufe", 
                   column(6, 
                          br(),
                          br(),
                          h4("Vergleichen Sie den beobachteten mit dem erwarteten Verlauf", align = "center"),
                          h4("ab einem gewünschten Zeitpunkt.", align = "center"),
                          br(),
                          br(),
                          searchInput(
                            inputId = "searchbox2", label = "Suche nach Themen",
                            placeholder = "Bitte Suchbegriff eingeben",
                            btnSearch = icon("search"),
                            btnReset = icon("remove"),
                            width = "450px"),
                          p("Mögliche Suchbegriffe finden Sie", 
                            a("hier", href = "https://www.psyndex.de/pub/info/PSYNDEXterms2016.pdf", target="_blank")),
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
  
  # modal pop up: https://stackoverflow.com/questions/50326110/r-shiny-popup-window-before-app
  # shiny alert: https://deanattali.com/blog/shinyalert-package/

  popup <- shinyalert(html = TRUE, title = "<h2 style='color:#333b8f'>Erkunden Sie Themen und Trends der 
                      psychologischen Fachliteratur</h3>",
                      text = "Die Themen wurden automatisch aus PSYNDEX-Einträgen generiert.
                      <br><br>Mit dem Button
                      <font color='#333b8f'><b>Suche in PSYNDEX</b></font>
                      können Sie zu jedem Thema relevante Literatur recherchieren.
                      <br><br><br><br><font size='-1'>Ein Angebot von:</font><br>
                      <img src = 'logo.png' width = 50% height = 50%>
                      <br><br>", 
                      type = "", animation = TRUE, confirmButtonCol = "#333b8f", closeOnClickOutside = TRUE)

  #showModal(popup)  
  
  
  observeEvent(input$hilfe, {
    # Show a modal when the button is pressed
    shinyalert(title = "Themen der psychologischen Fachliteratur",
               text = "Die Themen wurden automatisch aus PSYNDEX-Einträgen generiert. \n\nSie können", 
               type = "", animation = TRUE, confirmButtonCol = "#0094c5")
  })
  
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
  
  # transform invalid year input (popular topics)
  finalInputPop <- reactive({
    if (input$yearpop < 1980) return(1980)
    if (input$yearpop > 2018) return(2018)
    input$yearpop
  })
  
  # transform invalid year input (expected)
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
  

  
  # info box for development
  # output$info <- renderText({
  #  paste(input$dimension[1], input$dimension[2])
  #})
  
  
  ### plots ###
  
  # default color for barchart and background of xyplots headers
  colors <- rep(col_bars, 10)
  
  output$topicchart <- renderPlot({
    colors[(11-select_popular())] <- col_highlight
    #barchart(head(sort(theta_mean_by_year[as.character(finalInput()), ], decreasing = TRUE), 10)[10:1],
    barchart(head(sort(theta_mean_by_year[as.character(finalInputPop()), ]*100, decreasing = TRUE), 10)[10:1], 
             col = colors, 
             main = list(paste("Populäre Themen im Jahr", finalInputPop()), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    colors[(11-select_popular_range())] <- col_highlight
    barchart(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]*100), decreasing = TRUE), 10)[10:1],
             col = colors, 
             main = list(paste0("Populäre Themen im Zeitraum ", input$range[1], "–", input$range[2]), cex = 1.75),
             #xlab = "Mittlere Dokument-Topic-Wahrscheinlichkeit",
             xlab = "Prävalenz",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$hot <- renderPlot({
    colors[select_hot()] <- col_highlight
    xyplot(trends()[[3]]*100,
           layout = c(5,2),
           col = c("black"),
           ylim = c(0, max(theta_mean_by_year)*100),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex = 0.6),
           ylab = list("Prävalenz", cex = 0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           #main = list(paste0("Hot topics for the years ", input$range[1], "–", input$range[2]), cex = 1),
           main = list(paste0("Hot Topics für den Zeitraum ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background = list(col = colors)),
           strip = function(..., bg) { # http://r.789695.n4.nabble.com/lattice-change-background-strip-color-in-one-panel-td3554612.html
             strip.default(..., 
                           bg = trellis.par.get("strip.background")$col[which.packet()]
                           )}) 
  }, res = 125)
  
  output$cold <- renderPlot({
    colors[select_cold()] <- col_highlight
    xyplot(trends()[[4]]*100,
           layout = c(5,2),
           col = c("black"),
           ylim = c(0, max(theta_mean_by_year)*100),
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
  }, res = 125)
  
  output$topicplot <- renderPlot({
    #xyplot(theta_mean_by_year_ts[,select()], # fixed total time interval
    inp <- topic[(grepl(search_lower(), topic$Thema)),][select(), 1] # get correct topic number from filtered list
    xyplot(window(theta_mean_by_year_ts*100, input$range[1], c(input$range[1], input$range[2]-input$range[1]+1))[,inp],
           col = col_bars,
           ylim = c(0, max(theta_mean_by_year)*100),
           #ylab = list("Mittlere Dokument-Topic-Wahrscheinlichkeit", cex=0.6),
           ylab = list("Prävalenz", cex=0.6),
           xlab = "",
           type = c("l", "g"),
           lwd = 3,
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste("Zeitlicher Verlauf von Thema", inp), cex = 1),
           panel = function(...) {
             panel.abline(h = (1/k)*100, lty = "dashed", col = "#83227a")
             panel.text(1987, (1/k)*100*1.25, labels = "Durchschnittsprävalenz aller Themen", col = "#83227a", cex = 0.65)
             #panel.text(1982, (1/k)*100*0.8, labels = "unter dem Durchschnitt", col = "#83227a", cex = 0.7)
             panel.xyplot(...)
           },
           par.settings = list(strip.background = list(col = col_bars)))
  }, res = 125)
  
  output$circleplot <- renderPlot({
    inp <- topic[(grepl(search_lower(), topic$Thema)),][select(), 1]
    factor <- 40 # depends on number of topics k
    lim <- 0.18 # adjust depending on number of topics k
    plot(1, xlab="", ylab="", xaxt='n', yaxt='n', asp = 1, xlim = c(1-lim, 1+lim), ylim = c(1-lim, 1+lim),
         #main = list(paste0("Prävalenz von Thema ", select(), ": ", round(topic[select(),3], 4)), par(cex.main = 1)), type="n")
         main = list(paste0("Prävalenz von Thema ", inp, " im Vergleich "), par(cex.main = 1)), type = "n")
    plotrix::draw.circle(1, 1, topic[inp, 3]*factor, col=col_highlight, border=col_highlight) # current topic
    plotrix::draw.circle(1, 1, (mean(topic[,3]))*factor, border=col_bars, col="white", lty="solid", density=0) # average
    #plotrix::draw.circle(1, 1, (1/(dim(topic)[1]))*factor, border=col_bars, col="white", lty="solid", density=0) # average if all topics are included
    plotrix::draw.circle(1, 1, max(topic[,3])*factor, border="black", col="white", lty="dotted", density=0) # max
    #plotrix::draw.circle(1, 1, min(topic[,3])*factor, border="black", col="white", lty="solid", density=0) # min
    legend("bottomright",
           legend=c("Maximum", "Durchschnitt"), 
           col=c("black", col_bars), 
           lty=c("dotted", "solid"), 
           cex=0.6)
  }, res = 125)
  
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
    forecast <- forecast(mlp_ts*100, h = length(theta_mean_by_year_ts[, inp]) - length(window))
    plot(forecast, ylim = c(0, max(theta_mean_by_year_ts[,inp]*100)), showgap = FALSE, PI = TRUE,
         main = list(paste("Beobachteter und erwarteter Verlauf von Thema", inp), cex = 1.25), # remove main to see method
         col = col_bars, fcol = "#83227a",
         ylab = "Prävalenz", cex=0.6)
    lines(theta_mean_by_year_ts[,inp]*100)
    grid(NULL, NULL, lty = "solid", col = "lightgrey")
    abline(v = finalInput(), lty = "dashed", col = col_bars, lwd = 2)
  }, res = 100)
  
  
  ### data tables ##
  
  # options applied to all data tables
  options(DT.options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/9dcbecd42ad/i18n/German.json')))
  
  # popular by year #
  output$popular <- DT::renderDataTable({
    table_popular <- as.data.frame(head(sort(theta_year[as.character(finalInputPop()), ], decreasing = TRUE), 10)[1:10])
    table_popular$Thema <- rownames(table_popular)
    table_popular$Rang <- 1:10
    table_popular$NR <- as.numeric(names(head(sort(theta_mean_by_year[as.character(finalInputPop()), ], decreasing = TRUE), 10)[1:10]))
    rownames(table_popular) <- NULL
    table_popular[ ,c(1,2,3,4)] <- table_popular[ ,c(3,4,2,1)]
    names(table_popular) <- c("Rang", "ID", "Thema", "Prävalenz")
    table_popular[,4] <- round(table_popular[,4], 4)*100
    topicnum <- table_popular[,2]
    table_popular$Recherche <- createLink(table_popular$Thema, booster, topicnum)
    return(table_popular)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # popular in range of years #
  output$popularrange <- DT::renderDataTable({
    table_popular_range <- as.data.frame(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10])
    table_popular_range$Thema <- rownames(table_popular_range)
    table_popular_range$Rang <- 1:10
    table_popular_range$NR <- as.numeric(names(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10]))
    rownames(table_popular_range) <- NULL
    table_popular_range[ ,c(1,2,3,4)] <- table_popular_range[ ,c(3,4,2,1)]
    names(table_popular_range) <- c("Rang", "ID", "Thema", "Prävalenz")
    table_popular_range[,4] <- round(table_popular_range[,4], 4)*100
    topicnum <- table_popular_range[,2]
    table_popular_range$Recherche <- createLink(table_popular_range$Thema, booster, topicnum)
    return(table_popular_range)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # hot topics #
  output$hotterms <- DT::renderDataTable({
    table_hot <- trends()[[1]]
    topicnum <- table_hot[,2]
    table_hot$Recherche <- createLink(table_hot$Thema, booster, topicnum)
    names(table_hot)[2] <- ("ID")
    #names(table_hot) <- c("Rank", "No.", "Topic", "Search")
    return(table_hot)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # cold topic #
  output$coldterms <- DT::renderDataTable({
    table_cold <- trends()[[2]]
    topicnum <- table_cold[,2]
    table_cold$Recherche <- createLink(table_cold$Thema, booster, topicnum)
    names(table_cold)[2] <- ("ID")
    return(table_cold)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # all topics #
  output$topiclist <- DT::renderDataTable({
    topic <- topic[(grepl(search_lower(), topic$Thema)),]
    topicnum <- topic[,1]
    topic$Recherche <- createLink(topic$Thema, booster, topicnum)
    topic[,3] <- round(topic[,3], 4)*100
    names(topic)[1] <- ("ID")
    return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe', extensions = 'Responsive',
    #options = list(sDom = '<"top">flrt<"bottom">ip', searchHighlight = TRUE))
    options = list(lengthChange = TRUE, info = TRUE, paging = TRUE, searching = FALSE))
  # for clear search button:
  #proxy <- dataTableProxy("topiclist")
  
  # event selection #
  output$eventtable <- DT::renderDataTable({
    list <- topic[(grepl(search_lower2(), topic$Thema)),][,-3]
    names(list)[1] <- ("ID")
    return(list)
  }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe', extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))


}


# Run the application ----
shinyApp(ui = ui, server = server)

