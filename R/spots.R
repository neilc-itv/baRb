#' Get a tibble of spots from the BARB API
#'
#' @param min_transmission_date Start date of the spot query
#' @param max_transmission_date End date of the spot query
#' @param advertiser_name Advertiser name. Get names from barb_get_advertisers()
#' @param additional_filters Additional filters passed to the API as URL parameters
#'
#' @return A spots tibble
#' @export
#'
#' @examples
#' barb_get_spots(
#'  min_transmission_date = "2022-01-01",
#'  max_transmission_date = "2022-12-31",
#'  advertiser_name = "PLAYMOBIL UK")
barb_get_spots <- function(min_transmission_date = NULL,
                           max_transmission_date = NULL,
                           advertiser_name = NULL,
                           additional_filters = NULL){

  raw_json <- barb_query_api(
    barb_url_spots(),
    list(
      "min_transmission_date" = min_transmission_date,
      "max_transmission_date" = max_transmission_date,
      "advertiser_name" = advertiser_name
    )
  )

spots_parsed <- raw_json$events %>%
  tidyjson::as_tbl_json() %>%
  tidyjson::spread_values(panel_region = tidyjson::jstring('panel', 'panel_region')) %>%
  tidyjson::spread_values(station_name = tidyjson::jstring('station', 'station_name')) %>%
  tidyjson::spread_values(clearcast_commercial_title = tidyjson::jstring('clearcast_information', 'clearcast_commercial_title')) %>%
  tidyjson::spread_values(standard_datetime = tidyjson::jstring('spot_start_datetime', 'standard_datetime')) %>%
  tidyjson::enter_object('audience_views') %>%
  tidyjson::gather_array() %>%
  tidyjson::spread_all() %>%
  tibble::as_tibble()

audiences <- raw_json$audience_categories %>%
  tidyjson::as_tbl_json() %>%
  tidyjson::spread_all() %>%
  tibble::as_tibble()

if(nrow(spots_parsed)==0){
  return(NULL)
}

spots_parsed_full <- spots_parsed %>%
  dplyr::left_join(audiences, by = "audience_code")

spots_parsed_full

}
