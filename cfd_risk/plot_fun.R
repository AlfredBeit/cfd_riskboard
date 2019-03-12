


plot_fun<-function(data, summary, var, grouping, device){
  list_fun <- list(
    sum = function(x) sum(x),
    mean = function(x) mean(x),
    median = function(x) median(x)
  )
  sum_map=c(1,2,3)
  names(sum_map)=c('Sum', 'Avg', 'Median')
  
  formulae<-
    as.formula(paste(var,paste(grouping, collapse=" + ") ,  sep=" ~ "))
  
  new_df_agg <- aggregate(formulae, data=data, FUN=function(x){list_fun[[sum_map[summary]]](x)})
  
  
  if (device=='Total') {
    
    p3<-plot_ly(data=new_df_agg, y=new_df_agg[[var]], x=new_df_agg[[grouping[1]]], color = new_df_agg[[grouping[2]]], type='bar')%>%
      
      layout(title =  paste0(names(list_fun)[sum_map[summary]],' of ', paste0(var), " by " , grouping[1], " and " ,grouping[2]),
             yaxis = list(title = paste0(names(list_fun)[sum_map[summary]]," of " , paste0(var)),ticksuffix='$'), 
             barmode='relative', annotations = list(
               list(x = 0.01 , y = 1.05, text = "IOS+ANDROID", showarrow = F, xref='paper', yref='paper'))) 
    
    return(p3)
    
    
  } else
  {
    
    data1=subset(new_df_agg,device_type=='ios')
    data2=subset(new_df_agg,device_type=='android')
    
    
    p1<-plot_ly(data=data1, y=data1[[var]], x=data1[[grouping[1]]], color =data1[[grouping[2]]], type='bar', legendgroup=data1[[grouping[1]]])%>%
      
      layout(title =  paste0(names(list_fun)[sum_map[summary]],' of ', paste0(var), " by " , grouping[1], " and " ,grouping[2], " and " ,grouping[3]),
             yaxis = list(title = paste0(names(list_fun)[sum_map[summary]]," of " , paste0(var)), ticksuffix='$'), 
             barmode='relative', annotations = list(
               list(x = 0.2 , y = 1.07, text='IOS',  showarrow = F, xref='paper', yref='paper')))
      
    
    
    p2<-plot_ly(data=data2, y=data2[[var]], x=data2[[grouping[1]]], color = data2[[grouping[2]]], type='bar', legendgroup=data1[[grouping[1]]], showlegend=F)%>%
      
      layout(title =  paste0(names(list_fun)[sum_map[summary]],' of ', paste0(var), " by " , grouping[1], " and " ,grouping[2], " and " ,grouping[3] ),
             yaxis = list(title = paste0(names(list_fun)[sum_map[summary]]," of " , paste0(var)),ticksuffix='$'), 
             barmode='relative', annotations = list(
               list(x = 0.8 , y = 1.07, text='ANDROID', showarrow = F, xref='paper', yref='paper')))
    
      
    return(subplot(p1,p2, shareX = TRUE, shareY = TRUE))
    
  }
}