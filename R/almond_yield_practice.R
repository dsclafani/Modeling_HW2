almond_yield_practice <- function(data,
                                  coef_1 = 0.015,
                                  coef_2 = 0.0046,
                                  coef_3 = 0.07,
                                  coef_4 = 0.0043,
                                  const = 0.28) {
  
  clim_data <- data %>%
    group_by(year, month) %>%
    summarise(tmin = mean(tmin_c), precip = sum(precip))
  
  tmin_feb <- clim_data %>% filter(month == 2) %>% select(tmin)
  
  precip_jan <- clim_data %>% filter(month == 1) %>% select(precip)
  
  df <- left_join(tmin_feb, precip_jan) %>%
    rename(tmin_feb = tmin, precip_jan = precip)
  
  df <- df %>% 
    mutate(yield = (-coef_1 * tmin_feb) - (coef_2 * tmin_feb ^ 2) - (coef_3 * precip_jan) + 
    (coef_4 * precip_jan ^ 2) + const) %>% 
    select(year, yield)
  
  return(df)
  
}