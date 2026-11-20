#' Retrieve and compile indicator data from Fingertips API
#'
#' This function fetches raw data for given indicators and their profiles from
#' the Fingertips API, including any missing indicators that may be available in
#' other profiles. It ensures that all indicators listed in `own_prof_all` are
#' represented in the final dataset.
#'
#' @param own_prof_all dataframe of profiles containing `IndicatorID` and
#'   `OwnerProfileId`
#' @param area_codes 9 character ONS area codes of areas to include in the
#'   report. Only 2023 upper tier local authorities (codes starting "E06",
#'   "E08", "E09" or "E10") and statistical regions (codes starting "E12") are
#'   supported. England is always included in the output automatically.
#' @param areatypeid double, area type ID of interest; 502 for UTLAs, 6 for
#'    region and 15 for England
#'
#' @return A list with two elements:
#' \describe{
#'   \item{df}{A data frame containing the complete raw data for all indicators,
#'             including any retrieved from other profiles.}
#'   \item{other_prof}{A data frame of profile info for any other profiles used
#'                    to retrieve missing indicators. If no other profiles were
#'                    used, this will be an empty data frame.}
#' }
#' @noRd
get_raw_data <- function(own_prof_all,
                         area_codes,
                         areatypeid) {

  api_url <- config::get(
    value = "api_url",
    config = "production"
  )

  # get original set of indicators
  indicators <- own_prof_all$IndicatorID

  initial_data <- fingertipsR::fingertips_data(
    IndicatorID = own_prof_all$IndicatorID,
    AreaCode = area_codes,
    ProfileID = own_prof_all$OwnerProfileId,
    AreaTypeID = areatypeid,
    path = api_url
  )

  # return data if data has been pulled back for all inds ----------------------
  if (length(indicators) == length(unique(initial_data$IndicatorID))) {
    # create empty other_prof if not other profs were used
    other_prof <- own_prof_all[0, ]

    return(
      list(
        df = initial_data,
        other_prof = other_prof
      )
    )
  }

  # continue if data has not been pulled back for all inds ---------------------

  # get missing data
  missing_data_list <- fetch_missing_data(own_prof_all = own_prof_all,
                                          initial_data = initial_data,
                                          area_codes = area_codes,
                                          areatypeid = areatypeid)

  # combine initial data with the previously missing data
  combined_data <- initial_data |>
    bind_rows(missing_data_list$missing_df)

  other_prof <- missing_data_list$other_prof

  list(df = combined_data, other_prof = other_prof)
}

#------------------------------------------------------------------------------#
#' Fetch missing indicator data from other profiles
#'
#' This function identifies indicators that are missing from the initial dataset
#' and attempts to retrieve them from other available profiles via the
#' Fingertips API.
#' @param own_prof_all dataframe of profiles containing `IndicatorID` and
#'   `OwnerProfileId`
#' @param initial_data A data frame containing the initially retrieved data,
#'   used to identify missing indicators.
#' @param area_codes 9 character ONS area codes of areas to include in the
#'   report. Only 2023 upper tier local authorities (codes starting "E06",
#'   "E08", "E09" or "E10") and statistical regions (codes starting "E12") are
#'   supported. England is always included in the output automatically.
#' @param areatypeid double, area type ID of interest; 502 for UTLAs, 6 for
#'   regions and 15 for England
#'
#' @return A list with two elements:
#' \describe{
#'   \item{missing_df}{A data frame containing the additional data retrieved for
#'    missing indicators.}
#'   \item{other_prof}{A data frame of profile metadata for any other profiles
#'                     used to retrieve missing indicators. If no other profiles
#'                     were used, this will be an empty data frame.}
#' }
#' @noRd
fetch_missing_data <- function(own_prof_all,
                               initial_data,
                               area_codes,
                               areatypeid) {

  # get original set of indicators
  indicators <- own_prof_all$IndicatorID

  api_url <- config::get(
    value = "api_url",
    config = "production"
  )

  # Get missing data for area using other profiles -----------------------------
  missing_inds <- get_missing_inds(indicators, initial_data)
  other_prof <- find_other_profile(missing_inds, areatypeid)

  # if inds with missing data have been found in other profiles, get data
  if (nrow(other_prof) > 0) {
    missing_df  <- fingertipsR::fingertips_data(
      IndicatorID = other_prof$IndicatorID,
      AreaCode = area_codes,
      ProfileID = other_prof$OwnerProfileId,
      AreaTypeID = areatypeid,
      path = api_url
    )

  } else {
    missing_df <- data.frame()
  }

  # check if any indicators are still missing ----------------------------------
  if (nrow(missing_df) > 0) {
    if (length(missing_inds) != length(unique(missing_df$IndicatorID))) {
      missing_inds_2 <- get_missing_inds(missing_inds, missing_df)
      warning(paste0(
        "Indicators missing at AreaTypeID ",
        areatypeid,
        " level: ",
        paste(missing_inds_2, collapse = ", ")
      ),
      call. = FALSE)
    }
  }

  list(
    missing_df = missing_df,
    other_prof = other_prof
  )
}
