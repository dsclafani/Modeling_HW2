#' Almond Yield Function
#' 
#' @param data The climate data frame including daily precipitation and minimum daily temperature data
#' @return df Returns a dataframe containing each given year and it's corresponding almond yield anomaly
#' 
# Function Definition

#according to Lobell paper, minimum temperature data is to be taken from month 2 of each year

almond_yield <- function(data) {

  clim_data <- data %>%
    group_by(year, month) %>%
    summarise(tmin = mean(tmin_c), precip = sum(precip))

  tmin_feb <- clim_data %>% filter(month == 2) %>% select(tmin)

  precip_jan <- clim_data %>% filter(month == 1) %>% select(precip)

  df <- left_join(tmin_feb, precip_jan) %>%
    rename(tmin_feb = tmin, precip_jan = precip)

  df <- df %>%
    mutate(yield = (-0.015 * tmin_feb) - (0.0046 * tmin_feb ^ 2) - (0.07 * precip_jan) + (0.0043 * precip_jan ^ 2) + 0.28) %>% 
    select(year, yield)

  return(df)

}



