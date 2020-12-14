
# Class -------------------------------------------------------------------


data = '{"filter":[{"left":"market_cap_basic","operation":"nempty"}],"options":{"data_restrictions":"PREV_BAR","lang":"pt"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","sector","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,150]}'


get_data <- function(data){
  
  require(httr)
  
  headers = c(
    `authority` = 'scanner.tradingview.com',
    `accept` = 'text/plain, */*; q=0.01',
    `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36',
    `content-type` = 'application/x-www-form-urlencoded; charset=UTF-8',
    `origin` = 'https://br.tradingview.com',
    `sec-fetch-site` = 'same-site',
    `sec-fetch-mode` = 'cors',
    `sec-fetch-dest` = 'empty',
    `referer` = 'https://br.tradingview.com/',
    `accept-language` = 'pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7,hu;q=0.6',
    `cookie` = '_sp_ses.cf1a=*; _ga=GA1.2.358856770.1607445662; _gid=GA1.2.861757103.1607445662; _sp_id.cf1a=05301dcf-a5c5-4fe3-8686-7cb8eba3f1ce.1607445662.1.1607446184.1607445662.dfad3852-fcca-402f-aecf-ea63cb78ecd8; _gat_gtag_UA_24278967_1=1'
  )
  
  data = '{"filter":[{"left":"market_cap_basic","operation":"nempty"}],"options":{"data_restrictions":"PREV_BAR","lang":"pt"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","sector","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,150]}'
  t <- fromJSON(data)
  t_colnames <- t$columns
  
  res <- httr::POST(url = 'https://scanner.tradingview.com/brazil/scan', httr::add_headers(.headers=headers), body = data)
  
  tbf <- fromJSON(content(res, 'text'))
  
  final_df <- rbindlist(lapply(tbf$data$d,function(x){
    df <- data.table(t(data.frame(x)))
    names(df) <- t_colnames
    return(df)
  }))
  
  return(final_df)
  
}






# Class2 ------------------------------------------------------------------

t <- read_html('https://www.imdb.com/title/tt3501632/')
write_html(t, 't.html')
df <- fromJSON(t %>% html_node(xpath = '//script[@type="application/ld+json"]') %>% html_text())



t <- read_html('https://www.skyscanner.com.br/transporte/passagens-aereas/bsb/rioa/210107/?adults=1&adultsv2=1&cabinclass=economy&children=0&childrenv2=&destinationentityid=27541837&inboundaltsenabled=false&infants=0&originentityid=27539572&outboundaltsenabled=false&preferdirects=false&preferflexible=false&ref=home&rtn=0')
write_html(t, 't.html')
df2 <- fromJSON(t %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text())

toJSON(df2, auto_unbox = T)



t <- read_html('https://www.payscale.com/research/US/Job=Data_Scientist/Salary')
df2 <- fromJSON(t %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text())

toJSON(df2, auto_unbox = T)

