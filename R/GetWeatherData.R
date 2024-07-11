#' @title Get Current Weather Data
#' @description This function retrieves the current weather data for a specified
#'   location using the WeatherAPI.
#' @author Gareth Burns
#' @param location A character string representing the location for which you want
#' to retrieve the weather data. This can be a city name, coordinates (latitude and longitude),
#' or a postal code.
#' @param days Optional. Number of Days to supply forecast. If supplied returned
#'  forecasted weather for those days ahead (an positive integer up to 3)
#' @return A list containing the current weather data for the specified location.
#' If the request fails, it returns an error message.
#'
#' @examples
#' \dontrun{
#' location <- "London"
#' current_weather <- GetCurrentWeather(location)
#' print(current_weather)
#' }


GetCurrentWeather <- function(location, days = NULL, ...) {
  # Base URL for WeatherAPI current weather
 if(is.null(days)) {
   base_url <- "http://api.weatherapi.com/v1/current.json"
 } else {
   base_url <- "http://api.weatherapi.com/v1/forecast.json"
 }


  argg <- compact(list(
    "key" = Sys.getenv("WEATHER_API"),
    "q" = location,
    "days" = days
  ))
  # Create the request
  request <-
    do.call("req_url_query", args = c(list(.req = request(base_url)), argg))

  # Perform the request and check for success
  response <- req_perform(request)

  # Check if the request was successful
  if (resp_status(response) == 200) {
    # Parse the JSON response
    weather_data <- response %>%
      resp_body_json(simplifyVector = TRUE)

    # Return the relevant weather data
    return(weather_data)
  } else {
    # Return an error message if the request failed
    return(paste("Error:", resp_status(response)))
  }
}
