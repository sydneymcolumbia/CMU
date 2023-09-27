# Please note that your machine must be connected to the internet for this
# function to work as intended.
ip_address2country_code <- function(ip_addresses) {
  #############################################
  # Looks up the country code associated with an IP address using the
  # batch ip-api API.
  #
  # Parameters
  # ##########
  # ip_addresses: a vector of IP addresses.
  #
  # Returns
  # #######
  # The vector of country codes associated with the input IP addresses.
  # IP addresses for which a matching country code cannot be established
  # generate a NA. Input string that are not valid IP addresses generate a NA.
  #
  # Examples
  # ########
  # > ip_addresses <- c("191.23.45.1", "182.12.77.168")
  # > ip_address2country_code(ip_addresses)
  # [1] "BR" "ID"
  #
  # Notes
  # #####
  # Rate limit: 15 requests per minute.
  # API docs: https://ip-api.com/docs/api:batch
  #############################################
  ip_api_url <- "http://ip-api.com/batch/"
  df <- data.frame(query = ip_addresses)
  request_body <- toJSON(df)
  response <- POST(url = ip_api_url, body = request_body)
  response_content <- content(response)
  country_codes <-
    sapply(response_content, function(x)
      ifelse(is.null(x$countryCode), NA_character_, x$countryCode))
  return(country_codes)
}
