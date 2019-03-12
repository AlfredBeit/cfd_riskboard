library('shinydashboard')
library('ggplot2')
library('plotly')
library('RPostgreSQL')
library('data.table')
library('rjson')



config <- fromJSON(file = "/home/binomo/config.json")
source("/home/binomo/R/rfunc/sqlQuery.R", local=TRUE)
source("/home/binomo/R/rfunc/plot_fun.R", local=TRUE)
source("/home/binomo/R/rfunc/winrate_plot.R", local=TRUE)

server <-function(input, output, session) {
	values <- reactiveValues()
  	get_data <- reactive({
    values$asset <-  as.character(input$asset)
    asset <- values$asset 
    leftb = as.character(input$period[1])
    rightb = as.character(input$period[2])
    dls_sel <- sqlQuery(leftb, rightb, config, asset)
    N<-dim(dls_sel)[1]
    dls_sel$asset<-factor(trimws(dls_sel$asset))
    dls_sel$device_type<-factor(dls_sel$device_type)
    dls_sel$bet_usd<-dls_sel$bet_usd/100
    dls_sel$revenue_usd<-dls_sel$revenue_usd/100
    dls_sel$commission<-dls_sel$commission/100
    dls_sel$profit_less_comm<- `*`(-1,(dls_sel$revenue_usd-dls_sel$bet_usd) )
    dls_sel$profit<- `+`(`*`(-1,(dls_sel$revenue_usd-dls_sel$bet_usd) ), dls_sel$commission)
    dls_sel$volume<-(dls_sel$bet_usd*dls_sel$leverage)
    dls_sel$winrate<-as.numeric(ifelse(dls_sel$profit_less_comm < 0, 1, 0 ))
    
    sessions <- c(0,7, 12, 17, 23)
    labels <- c("ASIAN", "European", "European+US")
    dls_sel$sess_grp <-
      cut(as.numeric(format(as.POSIXct(dls_sel$closed_at, tz = "GMT"), '%H')), breaks = sessions , include.lowest =  TRUE)
    
    levs <- levels(dls_sel$sess_grp)
    dls_sel$sess_lab <- factor(rep(NA, length=N), levels = labels)
    dls_sel$sess_lab[dls_sel$sess_grp %in% c(levs[1])] <- labels[1]
    dls_sel$sess_lab[dls_sel$sess_grp %in% c(levs[c(2,4)])] <-labels[2]
    dls_sel$sess_lab[dls_sel$sess_grp %in% c(levs[3])] <- labels[3]
    return(dls_sel)

  })

  
  output$plot <- renderPlotly({
    input$goButton 	
	isolate({
      summary <- input$summary
      var <- input$category
      device <- input$device
      grouping <- c('asset','sess_lab', 'device_type')
	    dls = get_data()
      if(var=='winrate'){
  	    asset <- values$asset
  	    graph <- winrate_plot(dls,var,asset)}
      else {
        graph <- plot_fun(dls, summary, var, grouping, device)}
      graph
      
      
    })
  
  })
}








