#' Get the list of available advertiser names
#'
#' @return A character vector of advertiser names
#' @export
#'
#' @examples
#' barb_get_advertisers()
barb_get_advertisers <- function(){

  raw_json <- barb_query_api(
    barb_url_advertisers()
  )

  as.character(raw_json$json)
}
