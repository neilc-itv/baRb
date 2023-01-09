#' Get the list of available advertiser names
#'
#' @param min_transmission_date Min date for the request
#' @param max_transmission_date Maax date for the request
#'
#' @return A character vector of advertiser names
#' @export
#'
#' @examples
#' barb_get_advertisers("2022-01-01", "2022-01-31")
barb_get_advertisers <- function(
    min_transmission_date = NULL,
    max_transmission_date = NULL){

  raw_json <- barb_query_api(
    barb_url_advertisers(),
    list(
      "min_transmission_date" = min_transmission_date,
      "max_transmission_date" = max_transmission_date
    )
  )

  as.character(raw_json)
}
