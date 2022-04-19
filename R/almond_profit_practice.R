#' Almond profit Function
#' 
#' @param data The climate data frame including daily precipitation and minimum daily temperature data
#' @return df Returns a dataframe containing each given year and it's corresponding almond yield anomaly
#' 
# Function Definition

#according to Lobell paper, minimum temperature data is to be taken from month 2 of each year

library(here)

#source(here("R", "almond_yield.R"))

almond_profit_practice <- function(yield, year, price = 6000) {
  
  profit <- yield * price
  
  year_profit <- data.frame(year = year, yield = yield, profit = profit)
  
  return(year_profit)
  
}