
winrate_plot<-function(data,var,asset){
  data<-setDT(data)
  data$user_id<-factor(data$user_id)
  df1<-(data[, list(total_dls=.N, win_deals=sum(get(var))), by=user_id])
  df1$winrate<-`/`(df1$win_deals,df1$total_dls)
  df2<-df1[df1$total_dls>50, ]
  sort_df <- df2[order(df2$winrate), ]
  
  p4<-plot_ly()%>%
    add_boxplot(data=sort_df, y=sort_df[[var]]*100 , boxpoints = 'suspectedOutliers') 
  
  p5<-ggplot(sort_df,aes(x = sort_df[[var]]*100)) +
    geom_histogram(bins=21)
  p5<-ggplotly(p5)
  
  p6 <-ggplot(sort_df, (aes(x = sort_df[[var]]*100)))+
    stat_ecdf()
  p6<-ggplotly(p6)
  
  p7<-subplot(p4,p5,p6, shareY = F, shareX = F)%>%  
    layout(title =  paste0(var, " of ", asset))
  
  return(p7)
  
}  

