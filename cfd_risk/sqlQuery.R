sqlQuery <- function(leftb, rightb, config, asset) {
  con <- dbConnect(PostgreSQL(), user=config$login, password=config$password,
                   dbname="binomo_db", host="172.31.0.32", port="5433")
  
  query <- paste0("select dls.user_id, assets.name as asset, dls.trend, dls.opened_at, dls.closed_at, dls.bet_usd,", 
                  "crn.rate*dls.commission as commission, dls.revenue_usd, dls.leverage, dls.device_type from cfd_deals as dls ",
                  "left join  users as us on dls.user_id=us.id inner join assets on assets.id=dls.asset_id ",
                  "inner  join currency_rates as crn on us.currency_id=crn.currency_id where type='real' ",
                  "and closed_at >'",leftb," 'and closed_at <= '",rightb,"'and status ='closed' and crn.date = current_date", 
                  " and assets.name like '%",asset,"%'; ")
  on.exit(dbDisconnect(con))
  rs <- dbSendQuery(con, query)
  data <- dbFetch(rs)
  return(data)
}
