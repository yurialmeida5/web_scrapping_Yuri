rm(list=ls())

library(data.table)
library(rvest)
library(survPen)
library(tidyverse)
library(prettydoc)


########### Function Definition ##################################


get_my_url <- function(searchword, numberofpages){
  
  df_total = data.frame()
  
  for (i in 1:numberofpages){
    
    my_url <- paste0('https://www.newsinlevels.com/page/',
                     numberofpages, '/?s=',
                     searchword)
    
    t <- read_html(my_url)
    
    boxes <- t %>% 
      html_nodes('.title') %>%  html_text()
    
    boxes_links <- t %>% 
      html_nodes('.title a') %>% html_attr('href')
    
    boxes_df <- lapply(boxes, function(x){
      
      trimws(substr(x, 
                    instr(x , '\n', startpos = 2) + 2,
                    instr(x , ':', startpos = 2) + 2))
      
      t_list <- list()
      t_list[['title']] <- trimws(substr(x ,4,instr(x , '\n', startpos = 2) - 1))
      t_list[['date']] <-  trimws(substr(x, 
                                         instr(x , '\n', startpos = 2) + 2,
                                         instr(x , ':', startpos = 2) + 2))
      
      t_list[['teaser']] <- trimws(substr(x, 
                                          instr(x , ':', startpos = 2) + 3,
                                          nchar(x)))
      
      return(data.frame(t_list))  
      
    })
    
  df <- rbindlist(boxes_df)
  df$link <- boxes_links 
  
  df_total <- rbind(df_total,df)
    
  }
  
  return(df_total)
    
}


###################  Generate and Write Files #############################################

news_in_levels_corona <- get_my_url(searchword = 'corona', numberofpages =  5)
news_in_levels_trump <- get_my_url(searchword = 'Trump', numberofpages =  5)

write.csv(news_in_levels_corona,'news_in_levels_corona.csv')
write.csv(news_in_levels_corona,'news_in_levels_Trump.csv')  
