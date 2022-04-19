#' Almond Profit Function
#' 
#' @param data (data frame with the daily temperature data C)
#' @param price ($/ton) ~$6,000/ton
#' @param precip (mm) 
#' @return profit ($/acre)
#' 
# Function Definition


almond_profit_DS <- function (precip, data, price) {
  
  clim_data <- data %>%
    group_by(year, month) %>%
    summarise(tmin = mean(tmin_c)) %>% 
    filter(month==2)
  
    yield = ((-0.015 *clim_data$tmin) - (0.0046 *clim_data$tmin^ 2) - (0.07 * precip) + (0.0043 * precip^ 2) + 0.28)
    
    profit = yield*price

 almond_profit <-data.frame(year = year, yield=yield, profit =profit)
 return(almond_profit)
  
}
#this returns the answers as a dataframe, lets you input the years and yield all at once and generate the profits as a dataframe
#if you just do lines 12 and 14 and make line 14 return(profit), you will get a list of answers but not in a nice dataframe


