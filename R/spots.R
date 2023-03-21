#' Get a tibble of spots from the BARB API
#'
#' @param min_transmission_date Start date of the spot query
#' @param max_transmission_date End date of the spot query
#' @param advertiser_name Advertiser name. Get names from barb_get_advertisers()
#' @param additional_filters Additional filters passed to the API as URL parameters
#' @param macro_regions whether to return macro or standard region areas
#' @param consolidated whether to return consolidated or only live viewing. Defaults to TRUE (consolidated).
#' @param use_reporting_days whether to use a standard 24 hour clock or the BARB reporting clock. Defaults to FALSE (standard 24 hour clock).
#' @param standardise_audiences whether to standardise impacts by spot time length. Options are the default of no standardisation (""), "using_duration" or "using_rate_factors".
#'
#' @return A tibble of TV spots
#' @export
#'
#' @examples
#' barb_get_spots(min_transmission_date = "2022-01-01", max_transmission_date = "2022-12-31", advertiser_name = "PLAYMOBIL UK")
barb_get_spots <- function(min_transmission_date = NULL,
                           max_transmission_date = NULL,
                           advertiser_name = NULL,
                           additional_filters = NULL,
                           macro_regions = FALSE,
                           consolidated = TRUE,
                           use_reporting_days = FALSE,
                           standardise_audiences = ""){

  api_result <- barb_query_api(
    barb_url_spots(),
    list(
      "min_transmission_date" = min_transmission_date,
      "max_transmission_date" = max_transmission_date,
      "advertiser_name" = advertiser_name,
      "limit" = "5000",
      "consolidated" = consolidated,
      "use_reporting_days" = use_reporting_days,
      "standardise_audiences" = standardise_audiences
    )
  )

  if(is.null(api_result$json$events)) return(NULL)

  spots <- process_spot_json(api_result)

  #Paginate if necessary
  while(!is.null(api_result$next_url)){
    message("Paginating")
    api_result <- barb_query_api(api_result$next_url)

    api_page <- process_spot_json(api_result)

    spots <- spots %>%
      dplyr::union_all(api_page)
  }

  spots %>%
    dplyr::filter(is_macro_region==macro_regions)
}

process_spot_json <- function(spot_json){

  #Extract spot list from json
  spots_parsed <- spot_json$json$events %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_values(panel_region = tidyjson::jstring('panel', 'panel_region')) %>%
    tidyjson::spread_values(is_macro_region = tidyjson::jstring('panel', 'is_macro_region')) %>%
    tidyjson::spread_values(station_name = tidyjson::jstring('station', 'station_name')) %>%
    tidyjson::spread_values(clearcast_commercial_title = tidyjson::jstring('clearcast_information', 'clearcast_commercial_title')) %>%
    tidyjson::spread_values(standard_datetime = tidyjson::jstring('spot_start_datetime', 'standard_datetime'))

  #Get audience data for non-zero spots
  audiences_parsed <- spots_parsed %>%
    tidyjson::enter_object('audience_views') %>%
    tidyjson::gather_array() %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  #Extract audience names
  audiences <- spot_json$json$audience_categories %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_all() %>%
    tibble::as_tibble()

  #If all spots were zero rated, return result
  if(nrow(audiences_parsed)==0){
    spots_parsed <- spots_parsed %>%
      dplyr::select(
        panel_region,
        station_name,
        clearcast_commercial_title,
        standard_datetime
      )
    return(spots_parsed)
  }

  #Pivot audiences to columns and append zero rated spots again
  spots_audiences <- audiences_parsed %>%
    dplyr::left_join(audiences, by = c("audience_code")) %>%
    dplyr::mutate(kpi_var = audience_size_hundreds) %>%
    dplyr::select(document.id.x,
                  panel_region,
                  is_macro_region,
                  station_name,
                  clearcast_commercial_title,
                  standard_datetime,
                  audience_name,
                  kpi_var) %>%
    tidyr::pivot_wider(names_from = audience_name, values_from = kpi_var) %>%
    dplyr::rename(document.id = document.id.x)

  spots_parsed_wider <- spots_parsed

  spots_parsed_wider[setdiff(names(spots_audiences), names(spots_parsed))] <- NA
  spots_parsed_wider <- tibble::as_tibble(spots_parsed_wider)

  spots_all <- spots_audiences %>%
    dplyr::union_all(
      dplyr::filter(spots_parsed_wider, !document.id %in% spots_audiences$document.id)
    )

  spots_all
}
