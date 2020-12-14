library(rvest)
library(jsonlite)
library(data.table)
library(httr)
library(tidyverse)
library(utils)
library(survPen)
library(lubridate)
library(purrr)

rm(list=ls())

# Important, due to a connection limitation it`s important to refresh (copy and paste) the JSON link every 3 hours

SearchString <- "https://www.expedia.com.br/Flight-Search-Paging?c=f311868c-1c12-478c-995c-7cb18ed168c3&is=1&sp=asc&cz=200&cn=0"
summary_type <- 'Connection'


# Retrieve Flights Summary ------------------------------------------------

#### Doing it to show that we can retrieve also tables from a list instead of unique values 

get_summary_flights <- function(SearchString, summary_type = 'AirCompany'){
  
  
  ##### Define the variables and get the JSON ########
  
  df <- list()
  tbf = data.frame()
  df <- fromJSON(SearchString)
  final_df = data.frame()
  
  if(summary_type == 'Connection'){
  
    ################## FILTER CONNECTION SUMMARY ######################
    
    previous_data <- read.csv("Final_Project/Datasets/summary_byconnection.csv") 
    tbf <- df$content$summary$filters$stopFilters  
    tbf$run_time <- Sys.time()
    tbf = subset(tbf, select = -c(filterCategory,formattedReferencePrice,formattedPrice))
    tbf <- tbf %>% rename( Number_of_Flights = totalCount,
                           Min_Price = priceAsDouble,
                           Qtd_connections = filterName)
    tbf <- separate(data = tbf, into = c("search_date", "search_time"), col = run_time , sep = ' ')
    
    tbf$search_day_period <- with(tbf,  ifelse(hour(hms(search_time)) >= 5 & hour(hms(search_time)) <=11, "morning",
                                        ifelse(hour(hms(search_time))>11 & hour(hms(search_time))<=17, "afternoon", "night")))
    final_df <- dplyr::union(tbf,previous_data)                      
    write.table(final_df , "Final_Project/Datasets/summary_byconnection.csv", row.names = FALSE)
    
    return(final_df)
    
  }else{
  
  ################################### SUMMARY AIRLINE FILTERS ##################################################
      
  previous_data <- read.csv("Final_Project/Datasets/summary_byaircompany.csv")
  tbf <- df$content$summary$filters$airlineFilters
  tbf$run_time <- Sys.time()
  tbf = subset(tbf, select = -c(filterCategory,formattedReferencePrice,formattedPrice))
  tbf <- tbf %>% rename( Number_of_Flights = totalCount,
                             Min_Price = priceAsDouble,
                             Air_Line_Company = filterName)
  tbf <- separate(data = tbf, into = c("search_date", "search_time"), col = run_time , sep = ' ')
  tbf$search_day_period <- with(tbf,  ifelse(hour(hms(search_time)) >= 5 & hour(hms(search_time)) <=11, "morning",
                                          ifelse(hour(hms(search_time))>11 & hour(hms(search_time))<=17, "afternoon", "night")))
      
  final_df <- dplyr::union(tbf,previous_data)                      
  write.table(final_df , "Final_Project/Datasets/summary_byaircompany.csv", row.names = FALSE)
  
  return(final_df)   
  
  }

}

tbf <- get_summary_flights(SearchString, summary_type)


# Retrieve_Full_DataSet ---------------------------------------------------


get_flights_table <- function(SearchString){
  
  df_total = data.frame()
  
  df_fromJson <- fromJSON(SearchString)
  
  NestedList <- df_fromJson$content$legs
  
  previous_data <- read.csv("Final_Project/Datasets/flights_table.csv")
  
  for (i in 1:length(NestedList)){
    
      t_list <- list()
      t_list[['Fare_Code']] <- NestedList[[i]][['naturalKey']]
      t_list[['Price']] <- NestedList[[i]][['price']][['exactPrice']]
      t_list[['Currency']] <- NestedList[[i]][['price']][['currencyCode']]
      t_list[['AirLineCompany']] <- NestedList[[i]][['carrierSummary']][['airlineName']]
      t_list[['Qtd_Connections']] <- NestedList[[i]][['stops']]
      t_list[['Departure_Airport_City']] <- NestedList[[i]][['departureLocation']][['airportCity']]
      t_list[['Departure_Airport_Code']] <- NestedList[[i]][['departureLocation']][['airportCode']]
      t_list[['Arrival_Airport_City']] <- NestedList[[i]][['arrivalLocation']][['airportCity']]
      t_list[['Arrival_Airport_Code']] <- NestedList[[i]][['arrivalLocation']][['airportCode']]
      t_list[['Departure_Date']] <- NestedList[[i]][['departureTime']][['date']]
      t_list[['Departure_Time']] <- NestedList[[i]][['departureTime']][['time']]
      t_list[['Arrival_Date']] <- NestedList[[i]][['arrivalTime']][['date']]
      t_list[['Arrival_Time']] <- NestedList[[i]][['arrivalTime']][['time']]
      t_list[['Duration_Hours']] <- NestedList[[i]][['duration']][['hours']]
      t_list[['Duration_Minutes']] <- NestedList[[i]][['duration']][['minutes']]
      t_list[['run_time']] <- Sys.time()
      
    df <- data.frame(t_list)
    
    df <- separate(data = df, into = c("search_date", "search_time"), col = run_time , sep = ' ')
    
    df$remaining_days <- as.integer( as.Date(as.character(df$Departure_Date), format="%d/%m/%Y") -
                         as.Date(as.character(df$search_date), format="%Y-%m-%d") )
    
    df$search_day_period <- with(df, ifelse(hour(hms(search_time)) >= 5 & hour(hms(search_time)) <=11, "morning",
                                        ifelse(hour(hms(search_time))>11 & hour(hms(search_time))<=17, "afternoon", "night")))
    
    df_total <- rbind(df_total,df)
    
  }
  
  flights_df <- dplyr::union(df_total,previous_data) 
  
  write.csv(flights_df, 'Final_Project/Datasets/flights_table.csv', row.names = FALSE )
  return(flights_df)
  
}
 
flights_df <- get_flights_table(SearchString)


# Analysis ----------------------------------------------------------------

# Is it better to buy flights during the morning, afternoon or at night? Do the prices change? 
# Is there any correlation between the days missing to the flight and their price?

##### Unfortunately I couldn't collect enough data, so I simply altered the datasets randomly to check the graphs
##### However, I promise that I will generate the datasets properly and honestly do some analysis before buying my tickets


##### Read the file ######

flights_df <- read.csv("Final_Project/Datasets/flights_table.csv")

## Delivery_Summary 

Price_Analysis_by_Day_Period_and_Aircompany <- flights_df %>%
  group_by(search_day_period, AirLineCompany) %>% 
  summarise(
    mean     = mean(Price),
    median   = median(Price),
    std      = sd(Price),
    iq_range = IQR(Price), 
    min      = min(Price),
    max      = max(Price)
)


Price_Analysis_Remaing_days <- flights_df %>%
  group_by(remaining_days) %>% 
  summarise(
    mean     = mean(Price),
    median   = median(Price),
    std      = sd(Price),
    iq_range = IQR(Price), 
    min      = min(Price),
    max      = max(Price)
  )

#### Linear regression samples (Check later possible model transformations)

ggplot( data = Price_Analysis_Remaing_days, aes( x = remaining_days , y = min ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

ggplot( data = Price_Analysis_Remaing_days, aes( x = remaining_days , y = max ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

ggplot( data = Price_Analysis_Remaing_days, aes( x = remaining_days , y = mean ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )


