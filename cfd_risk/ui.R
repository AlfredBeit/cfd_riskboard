library('shinydashboard')
library('ggplot2')
library('plotly')



convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

shinyUI( dashboardPage(
  
  skin = "green",
  dashboardHeader(title='CFD board', titleWidth = 300), 
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      convertMenuItem(menuItem("display data", tabName = "graph", icon = icon("calculator"),
                               selectInput("category", "category: ", c('P/L' = 'profit','P/L w/o commission' = 'profit_less_comm' ,
                               											'Volume' = 'volume', 'Investment' = 'bet_usd', 
                               											'Win Rate' = 'winrate')),
                               br(), 
                               selectInput("device", "By device: ", c('Total', 'By device type')),
                               br(),
                               selectInput("summary", "Summary: ", c('Sum')),
                               br(),
                               selectInput("asset", "Select asset for winrate: ", c('All' = '','EUR/USD','GBP/USD',
                               															'USD/CHF','USD/JPY',
                               															'USD/CAD','AUD/USD',
                               															'Bitcoin', 'Litecoin', 'Apple', 'Crypto IDX' ) ),
                               br(),
                               dateRangeInput(inputId="period",
                                              label = "Period:",
                                              start = Sys.time(),
                                              end = Sys.Date()+1,
                                              min = "2018-03-01",
                                              max = NULL,
                                              separator = "") ,
                               br(),
                               actionButton("goButton", "Plot"),
                               br()
      ), "graph")
    )),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "graph",
              fluidRow(box(title='graph', width=12, status='primary',
                           plotlyOutput("plot"),
                           br(),
                           textOutput('text'))
              )
      )),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico", style="background-color: black;"))
  )
) )
