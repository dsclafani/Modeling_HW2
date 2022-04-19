#' Almond Yield Function
#' 
#' @param data The climate data frame including daily precipitation and minimum daily temperature data
#' @return df Returns a dataframe containing each given year and it's corresponding almond yield anomaly
#' 
# Function Definition

#according to Lobell paper, minimum temperature data is to be taken from month 2 of each year

library(here)

#source(here("R", "almond_yield.R"))

almond_profit <- function(yield, year, price) {
  
  yield <- (coef_1 * temp) - (coef_2 * tmin_feb ^ 2) - (coef_3 * precip_jan) + (coef_4 * precip_jan ^ 2) + const
  
  profit <- yield * price
  
  year_profit <- data.frame(year = year, yield = yield, profit = profit)
  
  return(year_profit)
  
}
