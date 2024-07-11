#' @title Should I Wear Shorts Today?
#' @description Tells me if I should wear short in a specific location today.
#' @author Gareth Burns
#' @param location A character string representing the location for which you want
#' to retrieve the weather data. This can be a city name, coordinates (latitude and longitude),
#' or a postal code.
#' @return A answer to the question 'Is it shorts weather?" for the supplied location.
#'
#' @examples
#' \dontrun{
#' IsItShortsWeather(location = "Belfast")
#' }
#' @export

IsItShortsWeather <- function(location){
  weatherData <- GetCurrentWeather(location = location)

  currentTemp <- weatherData[["current"]][["temp_c"]]

  if (currentTemp >= 14) {
    print("Yes! Get those shorts on!")
  } else {
    print("Maybe not today")
  }

}
