#' @title Should I Dry My Washing?
#' @description Tells me should I dry my washing in a specific location today.
#' @author Gareth Burns
#' @param location A character string representing the location for which you want
#' to retrieve the weather data. This can be a city name, coordinates (latitude and longitude),
#' or a postal code.
#' @return A answer to the question 'Should I dry my washing?" for the supplied location.
#'
#' @examples
#' \dontrun{
#' ShouldIDryMyWashing(location = "Belfast")
#' }
#' @export

ShouldIDryMyWashing <- function(location) {
  weatherData <- GetCurrentWeather(location = location, days = 1)

  chanceOfRain <-
    weatherData[["forecast"]][["forecastday"]][["day"]][["daily_chance_of_rain"]]

  if (chanceOfRain >= 50) {
    print(sprintf("Risky! There's a %s % chance of rain!",
                  chanceOfRain))
  } else {
    print(sprintf("Why not risk it! There's only a %s % chance of rain",
                  chanceOfRain))
  }

}
