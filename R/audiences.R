#' Add missing audiences to a spot or programme impacts table
#'
#' @param impacts output from barb_get_spots() or barb_get_programmes
#'
#' @return
#' @export
#'
#' @examples
barb_add_female_audiences <- function(impacts){
  impacts %>%
    dplyr::mutate(all_women = all_adults - all_men,
    women_18_20 = adults_18_20 - men_18_20,
    women_16_24 = adults_16_24 - men_16_24,
    women_16_34 = adults_16_34 - men_16_34,
    women_35_44 = adults_35_44 - men_35_44,
    women_45_49 = adults_45_49 - men_45_49,
    women_45_54 = adults_45_54 - men_45_54,
    women_55_64 = adults_55_64 - men_55_64,
    adults_65_plus = all_adults - adults_16_34 - adults_35_44 - adults_45_54 - adults_55_64,
    men_65_plus = all_men - men_16_34 - men_35_44 - men_45_54 - men_55_64,
    women_65_plus = adults_65_plus - men_65_plus,
    women_abc1 = adults_abc1 - men_abc1,
    adults_c2de = all_adults - adults_abc1,
    men_c2de = all_men - men_abc1,
    women_c2de = adults_c2de - men_c2de)
}
