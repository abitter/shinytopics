#
# PsychTopics
##############
# André Bittermann, ZPID, Trier
# abi@leibniz-psychology.org
#
# This app displays research topics in psychology
# identified using topic modeling of PSYNDEX data
#    
# Reference: Bittermann (2019). Development of a user-friendly app for exploring and analyzing research topics in psychology. 
# In G. Catalano, C. Daraio, M. Gregori, H. F. Moed & G. Ruocco (Hrsg.), Proceedings of the 17th Conference of the 
# International Society for Scientometrics and Informetrics (2634–2635). Rom: Edizioni Efesto. 
# http://dx.doi.org/10.23668/psycharchives.2521




# packages ----

library(shiny)
library(shinyWidgets)
library(shinyalert)
library(forecast)
library(nnet)
library(lattice)
library(DT)
library(plotrix)


# data ----
# result of topic modeling (LDA.R)
theta_year <- readRDS("data/theta_year.rds") # theta_mean_by_year with labels instead of topic numbers
theta_mean_by_year <- readRDS("data/theta_mean_by_year.rds") # mean theta of topic by year
theta_mean_by_year_time <- readRDS("data/theta_mean_by_year_time.rds") # for trend analysis
theta_mean_by_year_ts <- readRDS("data/theta_mean_by_year_ts.rds") # for trend analysis
years <- readRDS("data/years.rds") # a list of publication years
topic <- readRDS("data/topic.rds") # a list of topics and top terms
booster <- readRDS("data/booster.rds") # a table with factors for term boosting in PubPsych.eu
k <- 325 # set number of topics in the model (all topics, not only the reliable ones)

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
  
  # call shinyalert
  useShinyalert(),
  
  # color of selected row; https://www.w3schools.com/colors/colors_names.asp
  tags$style(HTML('table.dataTable tbody tr.selected td, table.dataTable td.selected{background-color:gold !important;}')),
  
  # Tab color; https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel/43201952#43201952
  tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: #0094c5; color:white}")),
  
  # Application title
   titlePanel("PsychTopics"), #v1.0.6   30.12.2020
  
     # Sidebar
   sidebarLayout(
     sidebarPanel(width = 3,
       
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
       
       sliderInput("range",
                    label = h4("Set range of publication years:"),
                    min = 1980,
                    max = as.numeric(years[length(years)]),
                    value = c(2015, as.numeric(years[length(years)])),
                    sep = "",
                    ticks = FALSE),
       
       actionButton("reset", "Reset"),
       br(),
       br(),
       helpText(br(),
                p("The psychological research topics from the German-speaking countries were identified automatically using",
                a("Topic Modeling.", href = "http://dx.doi.org/10.23668/psycharchives.2521", target = "_blank"), 
                strong("Conclusions should be drawn carefully.")),
                #br(),
                p("The topics consist of",  a("standardized keywords", 
                                              href = "https://www.psyndex.de/ueber/inhalte-aufbau/schlagwoerter-klassifikationen/#schlagw%C3%B6rter-psyndex-terms", 
                                              target="_blank"), "of the psychological reference database",
                  a("PSYNDEX.", href = "https://www.psyndex.de/ueber/steckbrief/", target = "_blank"))
                ),
       helpText("Topic", strong(em("prevalence")), "is the mean topic probability in the corpus of publications (in percent)."),
       br(),
       #helpText(a("Feedback", href = "https://leibniz-psychology.org/en/staff/profile-andre-bittermann/", target="_blank")),
       helpText(a("Feedback", href = "https://forms.gle/bzsC6AJdTTBY3RDH8", target = "_blank")),
       br(),
       br(),
       a(img(src = "logo.png", height = "75%", width = "75%"), href = "https://leibniz-psychology.org/en/", target = "_blank")
       ),
      
      # Main Panel
      mainPanel(width = 9, 
        tabsetPanel(
          # slider color
          tabPanel("Popular by Year", 
                   br(),
                   plotOutput("topicchart"),
                   sliderInput("yearpop",
                               label = "Select year:",
                               min = 1980,
                               max = as.numeric(years[length(years)]),
                               value = years[length(years)],
                               sep = "",
                               #width = "80%",
                               ticks = FALSE),
                   br(),
                   DT::dataTableOutput("popular"),
                   br()
                   ), 
          tabPanel("Popular by Period", 
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
          tabPanel("All Topics", 
                   column(6, 
                          br(),
                          plotOutput("topicplot")),
                   column(6,
                          plotOutput("circleplot"),
                          p("The circle plot shows whether a topic is addressed above or below average 
                            in the psychological literature.", align = "center")
                          ),
                   br(),
                   searchInput(
                     inputId = "searchbox", label = "Search for topics",
                     placeholder = "Please enter search term",
                     btnSearch = icon("search"),
                     btnReset = icon("remove"),
                     width = "450px"),
                   p("Possible search terms can be found", 
                     a("here", href = "https://fremdauswerter.zpid.de/thesaurus.php", target="_blank")),
                   br(),
                   DT::dataTableOutput("topiclist")
                   ),
          tabPanel("Expected Trends", 
                   column(6, 
                          br(),
                          br(),
                          h4("Compare the observed with the expected course", align = "center"),
                          h4("at a desired point in time.", align = "center"),
                          br(),
                          br(),
                          searchInput(
                            inputId = "searchbox2", label = "Search for topics",
                            placeholder = "Please enter search term",
                            btnSearch = icon("search"),
                            btnReset = icon("remove"),
                            width = "450px"),
                          p("A list of possible search terms can be found", 
                            a("here", href = "https://www.psyndex.de/pub/info/PSYNDEXterms2016.pdf", target="_blank")),
                          br(),
                          DT::dataTableOutput("eventtable")),
                   column(6,
                          br(),
                          plotOutput("eventplot"),
                          alignCenter(sliderInput("year",
                                      label = "Select year:",
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
  
  popup <- shinyalert(html = TRUE, 
                      
                      # HTML improvements by ttr@leibniz-psychology.org
                      
                      title = '<h1 style="color: #333b8f; margin-top: 2rem; text-align:center; font-weight: bold; font-size: 24px; line-height: 1.2">
                      Explore topics and trends in psychology
                      </h1>',
                        
                        text = 
                        '<div style="margin: 2rem; color: #333333; text-align:left;">
                        <p style="font-size:18px; line-height:1.45">
                        The topics were derived <strong>automatically</strong> from PSYNDEX records using machine learning algorithms.
                        </p>
                        
                        <p style="color: #333b8f; margin-top: 1rem; font-size:18px; line-height:1.45">
                        <strong>Thus, conclusions should be drawn carefully.</strong>
                        </p>
                        <p style="margin-top: 1rem; font-size:18px; line-height:1.45">
                        Each topic is characterized by five terms. To better interpret a topic, you can check relevant literature by clicking the button <strong>Search PSYNDEX</strong>.
                        </p>
                        </div>',
                  
                      type = "", animation = TRUE, confirmButtonCol = "#333b8f", closeOnClickOutside = TRUE)
  
  showModal(popup) 
  
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
    if (input$yearpop > 2019) return(2019)
    input$yearpop
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
    barchart(head(sort(theta_mean_by_year[as.character(finalInputPop()), ]*100, decreasing = TRUE), 10)[10:1], 
             col = colors, 
             main = list(paste("Popular topics in", finalInputPop()), cex = 1.75),
             xlab = "Prevalence",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$topicchart2 <- renderPlot({
    colors[(11-select_popular_range())] <- col_highlight
    barchart(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]*100), decreasing = TRUE), 10)[10:1],
             col = colors, 
             main = list(paste0("Popular topics in years ", input$range[1], "–", input$range[2]), cex = 1.75),
             xlab = "Prevalence",
             scales=list(tck=c(1,0), x=list(cex=1), y=list(cex=1.5))) # label font size
  })
  
  output$hot <- renderPlot({
    colors[select_hot()] <- col_highlight
    xyplot(trends()[[3]]*100,
           layout = c(5,2),
           col = c("black"),
           ylim = c(0, max(theta_mean_by_year)*100),
           ylab = list("Prevalence", cex = 0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste0("Hot topics for the years ", input$range[1], "–", input$range[2]), cex = 1),
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
           ylab = list("Prevalence", cex = 0.6),
           xlab = "",
           type = c("l", "g"),
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste0("Cold topics for the years ", input$range[1], "–", input$range[2]), cex = 1),
           par.settings = list(strip.background = list(col = colors)),
           strip = function(..., bg) {
             strip.default(...,
                           bg = trellis.par.get("strip.background")$col[which.packet()]
                           )}) 
  }, res = 125)
  
  output$topicplot <- renderPlot({
    inp <- topic[(grepl(search_lower(), topic$Thema)),][select(), 1] # get correct topic number from filtered list
    xyplot(window(theta_mean_by_year_ts*100, input$range[1], c(input$range[1], input$range[2]-input$range[1]+1))[,inp],
           col = col_bars,
           ylim = c(0, max(theta_mean_by_year)*100),
           ylab = list("Prevalence", cex=0.6),
           xlab = "",
           type = c("l", "g"),
           lwd = 3,
           scales = list(x = list(alternating = FALSE), tck = c(1,0), y = list(cex = 0.6)),
           main = list(paste("Time course of Topic", inp), cex = 1),
           panel = function(...) {
             panel.abline(h = (1/k)*100, lty = "dashed", col = "#83227a")
             panel.text(1980, (1/k)*100*1.2, pos = 4, labels = "Mean prevalence of all topics", col = "#83227a", cex = 0.55)
             panel.xyplot(...)
           },
           par.settings = list(strip.background = list(col = col_bars)))
  }, res = 125)
  
  output$circleplot <- renderPlot({
    inp <- topic[(grepl(search_lower(), topic$Thema)),][select(), 1]
    factor <- 40 # depends on number of topics k
    lim <- 0.18 # adjust depending on number of topics k
    plot(1, xlab="", ylab="", xaxt='n', yaxt='n', asp = 1, xlim = c(1-lim, 1+lim), ylim = c(1-lim, 1+lim),
         main = list(paste0("Prevalence of Topic ", inp, " compared to max and average "), par(cex.main = 1)), type = "n")
    plotrix::draw.circle(1, 1, topic[inp, 3]*factor, col=col_highlight, border=col_highlight) # current topic
    plotrix::draw.circle(1, 1, (mean(topic[,3]))*factor, border=col_bars, col="white", lty="solid", density=0) # average
    plotrix::draw.circle(1, 1, max(topic[,3])*factor, border="black", col="white", lty="dotted", density=0) # max
    legend("bottomright",
           legend=c("Maximum", "Average"), 
           col=c("black", col_bars), 
           lty=c("dotted", "solid"), 
           cex=0.6)
  }, res = 125)
  
  output$eventplot <- renderPlot({
    inp <- topic[(grepl(search_lower2(), topic$Thema)),][select_event(), 1]
    window <- window(theta_mean_by_year_ts[, inp], start = 1980, end = finalInput())
    
    #mlp <- plotXY(1:length(window), window, complexity = 2)
    #mlp_ts <- ts(mlp$prediction, start = 1980)
    #forecast <- forecast(mlp_ts*100, h = length(theta_mean_by_year_ts[, inp]*100) - length(window))
    
    forecast <- forecast(auto.arima(window*100), h = length(theta_mean_by_year_ts[, inp]*100) - length(window))
    plot(forecast, ylim = c(0, max(theta_mean_by_year_ts[,inp]*100)), showgap = FALSE, PI = TRUE,
         main = list(paste("Observed and expected trend of Topic", inp), cex = 1.25), # remove main to see method
         col = col_bars, fcol = "#83227a")
    lines(theta_mean_by_year_ts[,inp]*100)
    grid(NULL, NULL, lty = "solid", col = "lightgrey")
    abline(v = finalInput(), lty = "dashed", col = col_bars, lwd = 2)
  }, res = 100)
  
  
  ### data tables ##
  
  # options applied to all data tables
  options(DT.options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json')))
  
  # popular by year #
  output$popular <- DT::renderDataTable({
    table_popular <- as.data.frame(head(sort(theta_year[as.character(finalInputPop()), ], decreasing = TRUE), 10)[1:10])
    table_popular$Thema <- rownames(table_popular)
    table_popular$Rang <- 1:10
    table_popular$NR <- as.numeric(names(head(sort(theta_mean_by_year[as.character(finalInputPop()), ], decreasing = TRUE), 10)[1:10]))
    rownames(table_popular) <- NULL
    table_popular[ ,c(1,2,3,4)] <- table_popular[ ,c(3,4,2,1)]
    names(table_popular) <- c("Rank", "ID", "Topic", "Prevalence")
    table_popular[,4] <- round(table_popular[,4], 4)*100
    topicnum <- table_popular[,2]
    table_popular$Search <- createLink(table_popular$Topic, booster, topicnum)
    # remove ranking
    table_popular <- table_popular[,-1]
    return(table_popular)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe',  extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # popular in range of years #
  output$popularrange <- DT::renderDataTable({
    table_popular_range <- as.data.frame(head(sort(colMeans(theta_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10])
    table_popular_range$Thema <- rownames(table_popular_range)
    table_popular_range$Rang <- 1:10
    table_popular_range$NR <- as.numeric(names(head(sort(colMeans(theta_mean_by_year[(input$range[1]-1979):(input$range[2]-1979), ]), decreasing = TRUE), 10)[1:10]))
    rownames(table_popular_range) <- NULL
    table_popular_range[ ,c(1,2,3,4)] <- table_popular_range[ ,c(3,4,2,1)]
    names(table_popular_range) <- c("Rank", "ID", "Topic", "Prevalence")
    table_popular_range[,4] <- round(table_popular_range[,4], 4) *100
    topicnum <- table_popular_range[,2]
    table_popular_range$Search <- createLink(table_popular_range$Topic, booster, topicnum)
    # remove ranking
    table_popular_range <- table_popular_range[,-1]
    return(table_popular_range)
  }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # hot topics #
  output$hotterms <- DT::renderDataTable({
    table_hot <- trends()[[1]]
    topicnum <- table_hot[,2]
    table_hot$Recherche <- createLink(table_hot$Thema, booster, topicnum)
    names(table_hot) <- c("Rank", "ID", "Topic", "Search")
    # remove ranking
    table_hot <- table_hot[,-1]
    return(table_hot)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # cold topic #
  output$coldterms <- DT::renderDataTable({
    table_cold <- trends()[[2]]
    topicnum <- table_cold[,2]
    table_cold$Recherche <- createLink(table_cold$Thema, booster, topicnum)
    names(table_cold) <- c("Rank", "ID", "Topic", "Search")
    # remove ranking
    table_cold <- table_cold[,-1]
    return(table_cold)
    }, escape = FALSE, rownames = FALSE, selection = list(mode = "single", selected = 1), class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))
  
  # all topics #
  output$topiclist <- DT::renderDataTable({
    topic <- topic[(grepl(search_lower(), topic$Thema)),]
    topicnum <- topic[,1]
    topic$Recherche <- createLink(topic$Thema, booster, topicnum)
    topic[,3] <- round(topic[,3], 4)*100
    names(topic) <- c("ID", "Topic", "Prevalence", "Search")
    return(topic)
    }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe', extensions = 'Responsive',
    options = list(lengthChange = TRUE, info = TRUE, paging = TRUE, searching = FALSE))
  
  # event selection #
  output$eventtable <- DT::renderDataTable({
    list <- topic[(grepl(search_lower2(), topic$Thema)),][,-3]
    names(list) <- c("ID", "Topic")
    return(list)
  }, escape = FALSE, selection = list(mode = "single", selected = 1), rownames = FALSE, class = 'stripe', extensions = 'Responsive',
  options = list(lengthChange = FALSE, info = FALSE, paging = FALSE, searching = FALSE))


}


# Run the application ----
shinyApp(ui = ui, server = server)

