#' Almond Yield Function
#' 
#' @param tmin minimum temperature °C
#' @param p precipitation (mm)
#' @return y yield anomaly (tons almonds/acre)
#' 
# Function Definition

yield = function(tmin, tmax, p) {
  y = -0.015*tmin - 0.0046*tmin^2 -0.07*p +0.0043*p^2 + 0.28
  return(y)
}