#' England and region area code lookup
#'
#' A lookup of the ONS area codes for England and the nine English statistical
#' regions, the default set of areas for the Health trends in England (HtiE)
#' reports.
#'
#' [get_data()] compares the requested area codes against this table. When they
#' match exactly (England and all nine regions, as for HtiE), indicators that
#' do not have data for every region have their regional rows removed, so that
#' regional pages are only produced for indicators with complete coverage.
#'
#' @format A tibble with 10 rows and 2 columns:
#' \describe{
#'   \item{AreaCode}{Character. 9-character ONS area code: "E92000001" for
#'     England and "E12000001" to "E12000009" for the regions.}
#'   \item{AreaName}{Character. Area name, e.g. "England", "North East",
#'     "London".}
#' }
#'
#' @source Office for National Statistics (ONS) geography codes. Built by
#'   `data-raw/lkup_table.R`.
#'
#' @examples
#' region_lookup_table
"region_lookup_table"
