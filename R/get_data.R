#' Create metadata for all indicators

#' `get_metadata` gets fingertips metadata for all indicators and saves to data folder
#'
#' @param indicators numeric vector, all indicators IDs
#' @return list, metadata list for all indicators
#' @noRd
get_metadata <- function(indicators) {
  # get api address from config
  api_url <- config::get(value = "api_url", config = "production")

  # Make the API call using the extracted indicator IDs
  all_meta_selected_inds <- fingertipsR::get_fingertips_api(
    paste0(
      api_url,
      "indicator_metadata/by_indicator_id?indicator_ids=",
      paste0(as.character(indicators), collapse = "%2C"),
      "&include_definition=yes"
    )
  )

  # remove HTML from selected Descriptive metadata fields
  all_meta <- all_meta_selected_inds |>
    map(remove_meta_html)

  all_meta
}

#------------------------------------------------------------------------------#
#' Remove indicators with incomplete region data
#'
#' `remove_incomplete_region_data` identifies indicators that have some
#' regional data but not data for all regions, and removes the regional (E12)
#' rows for those indicators. Indicators available at England level only are
#' retained as-is. A warning is raised listing the affected indicators.
#'
#' Note: the number of expected areas (10) and the region prefix ("E12") are
#' currently hardcoded for the region area type and will need parameterising
#' if extended to other area types.
#'
#' @param data data frame, formatted indicator data (i.e. `df_profile` in
#' `get_data`), containing at least IndicatorID, AreaCode and Value columns.
#' @return list with two elements: `data`, the input data frame with incomplete
#'   regional rows removed; and `area_check`, a data frame of indicators with
#'   fewer than 10 areas, used for further filtering downstream in `get_data`.
#' @noRd
remove_incomplete_region_data <- function(data) {
  # Check complete regional data
  area_check <- data |>
    drop_na("Value") |>
    filter(!(.data$Value %in% -1)) |>
    select("IndicatorID", "AreaCode") |>
    unique() |>
    group_by(.data$IndicatorID) |>
    count() |>
    filter(n < 10)

  area_less_10 <- unique(area_check$IndicatorID)

  area_only_1 <- area_check |>
    filter(n <= 1) |>
    pull(.data$IndicatorID) |>
    unique()

  miss_rgn <- setdiff(area_less_10, area_only_1)

  # data removed if regional data incomplete
  df_remove <- data |>
    filter(!(
      .data$IndicatorID %in% miss_rgn &
        str_sub(.data$AreaCode, 1, 3) %in% c("E12")
    ))

  warning(
    paste0(
      "The following indicators do NOT have complete regional data and have been removed for regions: \n",
      paste(miss_rgn, collapse = ", "),
      "\nThe following indicators have England level data only: \n",
      paste(area_only_1, collapse = ", ")
    )
  )

  list(data = df_remove, area_check = area_check)
}


#------------------------------------------------------------------------------#
#' Get Health trends data from Fingertips
#'
#' `get_data()` pulls data and metadata for every indicator in the indicator
#' specification CSV from the Fingertips API, for England and the requested
#' areas. It applies the HtiE filtering rules, adds Fingertips link columns
#' and saves the result to `healthtrendsfiles/data/<data_name>`.
#'
#' @param area_codes Character vector of 9-character ONS area codes. Upper
#'   tier local authorities (2023 boundaries; prefixes "E06", "E08", "E09",
#'   "E10") and statistical regions ("E12") are supported. England
#'   ("E92000001") is always included. If not specified, defaults to England
#'   and the nine regions.
#' @param data_name File name for the saved `.rds` file. Defaults to
#'   `"health_trends_data.rds"`.
#' @param generate_urls Logical. If `TRUE` (the default), adds Fingertips
#'   trend, definition and profile URL columns. If `FALSE`, those columns are
#'   added but filled with `NA`, which makes the run faster when testing.
#'
#' @return A data frame with one row per indicator, area, sex and time period,
#'   containing values, confidence limits, indicator metadata and Fingertips
#'   URLs. The same data frame is saved to `healthtrendsfiles/data/<data_name>`.
#'
#' @examples
#' \dontrun{
#' import_healthtrends_files()
#' df <- get_data(area_codes = c("E12000007"), generate_urls = FALSE)
#' }
#' @export
get_data <- function(area_codes = NULL,
                     data_name = "health_trends_data.rds",
                     generate_urls = TRUE) {
  # ---- Setup ----

  # If area_codes is NULL, default to England and all regions
  if (is.null(area_codes)) {
    area_codes <- c("E92000001", "E12000001", "E12000002", "E12000003",
                    "E12000004", "E12000005", "E12000006", "E12000007",
                    "E12000008", "E12000009")
  }

  # error check for data_name
  if (((!is.character(data_name)) ||
                    (str_sub(data_name, -4) != ".rds"))) {
    stop('data_name must be a character string ending in ".rds"',
         call. = FALSE)
  }

  # error check for generate_urls
  if (!rlang::is_bool(generate_urls)) {
    stop('The generate_urls argument must be a single TRUE or FALSE value.',
         call. = FALSE)
  }

  # Force England to be included
  if (!("E92000001" %in% area_codes)) {
    area_codes <- c(area_codes, "E92000001")
  }

  # Get area types ids
  areatypeid_lookup <- area_type_lookup()

  area_df <- tibble(AreaCode = area_codes, prefix = str_sub(area_codes, 1, 3)) |>
    left_join(areatypeid_lookup, by = "prefix") |>
    select(-"prefix")
  # TODO: error handling for areas that are not Eng, regions, or utlas

  # get api address from config
  api_url <- config::get(value = "api_url", config = "production")
  # get indicators
  ind_df <- get_indicator_csv(as_df = TRUE)
  indicators <- ind_df$IndicatorID

  # Set threshold for the minimum number of time points
  tp_threshold_upper <- 4 # upper geog
  tp_threshold_lower <- 2 # all geogs below upper geog

  # Set threshold for the minimum number of non-na values
  na_threshold_upper <-  4 # upper geog
  na_threshold_lower <- 2 # all geogs below upper geog

  # Upper (top-level) geography used for the time point threshold check.
  # Indicators below the threshold here are removed entirely; indicators below
  # the threshold for lower level areas only are retained.
  area_code_upper <- "E92000001"
  area_codes_lower <- area_codes[area_codes != area_code_upper]

  if (is.null(indicators)) {
    stop("indicators must have an input")
  }

  if (!(is.numeric(indicators))) {
    stop("indicators must be of type numeric")
  }

  # ---- Metadata ----
  # create new metadata file
  cat("\nGetting metadata from ", api_url, "\n")

  # start timer to time execution at indicator level
  start_time <- Sys.time()

  all_meta <- get_metadata(indicators)

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  cat("  Metadata generation completed in ",
    duration,
    " minutes\n",
    sep = ""
  )


  # ---- Profile IDs ----
  # get profile ids for each indicator
  cat("\nGetting profile ids from", api_url, "\n")

  # start timer to time execution at indicator level
  start_time <- Sys.time()

  own_prof_all <- profile_owner(indicators)

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  cat("  Profiles ID generation completed in ",
    duration,
    " minutes\n",
    sep = ""
  )


  # ---- Raw data pull ----
  # get raw fingertips data data
  cat("\nGetting raw data from", api_url, "\n")
  start_time <- Sys.time()

  # order areatypeids from smallest to largest geog
  # so that default profile ids can be change to that of smallest geog
  area_type_order <- c(502, 6, 15)
  areatypeids <- unique(area_df$AreaTypeID)
  areatypeids <- area_type_order[area_type_order %in% areatypeids]

  all_df <- vector("list", length(areatypeids)) |> stats::setNames(areatypeids)
  all_other_prof <- vector("list", length(areatypeids)) |> stats::setNames(areatypeids)

  for (areatypeid in areatypeids) {
    area_codes_subset <- area_df |>
      filter(.data$AreaTypeID == areatypeid) |>
      pull(.data$AreaCode)

    df_list <- get_raw_data(
      own_prof_all = own_prof_all,
      area_codes = area_codes_subset,
      areatypeid = areatypeid
    )

    all_df[[as.character(areatypeid)]] <- df_list$df
    all_other_prof[[as.character(areatypeid)]] <- df_list$other_prof

    # if other profile used to get lower geog data,
    # use this for all other geogs
    if (areatypeid %in% areatypeids[1]) {
      other_prof <- df_list$other_prof

      if (nrow(other_prof) > 0) {
        own_prof_all <- own_prof_all |>
          filter(!.data$IndicatorID %in% other_prof$IndicatorID) |>
          bind_rows(other_prof)
      }
    }
  }

  df <- dplyr::bind_rows(all_df)
  other_prof_all <- own_prof_all # renaming as changed in for-loop
  # TODO: cases when there's no data for higher geog areas, at lower geog other_prof id

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  cat("  Raw data generation completed in ", duration, " minutes\n", sep = "")


  # ---- Default filtering ----
  # filter out indicators with non-year timeperiod ranges
  # whole year ranges required for completing TimeperiodSortable column
  df_yr <- df |>
    filter(str_ends(.data$Timeperiodrange, "y"))

  miss_yr <- setdiff(unique(df$IndicatorID), unique(df_yr$IndicatorID))

  if (length(miss_yr) > 0) {
    warning(paste0("The following indicators do not have annual data and have been removed: \n",
                   paste(miss_yr, collapse = ", ")))
  }

  # filter to "Persons" as default
  df_sex <- df_yr |>
    group_by(.data$IndicatorID) |>
    filter(if ("Persons" %in% .data$Sex) {
      .data$Sex == "Persons"
    } else {
      .data$Sex == .data$Sex
    }) |>
    ungroup()


  # ---- Core formatting ----
  # format data frame
  df_selected <- df_sex |>
    mutate(AreaName = str_remove_all(.data$AreaName,
                                     " region \\(statistical\\)")) |>
    select("IndicatorID",
           "IndicatorName",
           "AreaCode",
           "AreaName",
           "AreaType",
           "Timeperiod",
           "Sex",
           "Age",
           "Value",
           "LowerCI95.0limit",
           "UpperCI95.0limit",
           "Count",
           "TimeperiodSortable") |>
    distinct()


  # ---- QA checks ----
  # Remove indicators without complete regional data
  # if only England and Regions are requested (as required for HtiE)
  if (setequal(area_codes, healthtrends::region_lookup_table$AreaCode)) {
    regional_check <- remove_incomplete_region_data(df_selected)
    df_remove <- regional_check$data
    area_check <- regional_check$area_check
  } else {
    df_remove <- df_selected
    area_check <- tibble(IndicatorID = integer(), n = integer())
  }


  # ---- Complete time periods ----
  reduced_inds <- unique(df_remove$IndicatorID)

  # get complete time periods and main age category for indicators
  complete_trend_df <- complete_trend(
    indicators = reduced_inds,
    tp_threshold = tp_threshold_upper
  ) |>
    unique()

  # add a completed TimeperiodSortable column
  complete_trend_df <- complete_trend_df |>
    mutate(TimeperiodSortable = str_remove(.data$Timeperiod, "^[^0-9]*"),
           TimeperiodSortable = str_sub(.data$TimeperiodSortable, start = 1, end = 4),
           TimeperiodSortable = as.integer(.data$TimeperiodSortable) * 10000)

  complete_trend_area <- df_remove |>
    select("IndicatorID", "AreaCode") |>
    unique() |>
    full_join(complete_trend_df, join_by("IndicatorID"), relationship = "many-to-many") |>
    left_join(
      complete_trend_df,
      by = join_by(
        "IndicatorID",
        "Timeperiod",
        "Sex",
        "SexID",
        "Age",
        "AgeID",
        "YearRange",
        "TimeperiodOrder2",
        "TimeperiodOrder",
        "TimeperiodSortable"
      )
    )

  add_tps <- df_remove |>
    right_join(complete_trend_area,
      by = join_by("IndicatorID",
                   "Timeperiod",
                   "AreaCode",
                   "Sex",
                   "Age",
                   "TimeperiodSortable")
    ) |>
    select("IndicatorID",
           "AreaCode",
           "Timeperiod":"TimeperiodOrder",
           "TimeperiodSortable")

  # add complete timeperiod rows to dataframe and include only main age category
  df_tp <- df_remove |>
    select("IndicatorID":"AreaType") |>
    unique() |>
    left_join(add_tps, by = join_by("IndicatorID", "AreaCode")) |>
    arrange(.data$IndicatorID, .data$AreaCode, .data$TimeperiodOrder2) |>
    # remove any timeperiods which haven't joined to existing data
    filter(
      !is.na(.data$AreaCode),
      !(
        .data$IndicatorID %in% area_check$IndicatorID &
          str_sub(.data$AreaCode, 1, 3) %in% c("E12")
      )
    )


  # ---- Threshold check ----

  # Threshold removals: upper

  # Check which indicators are below the time point threshold for the upper area
  below_upper_threshold_df <- tp_threshold_check(
    data = df_tp,
    tp_area_codes = area_code_upper,
    tp_threshold = tp_threshold_upper
  )

  # Remove an indicator for all areas if it is below the non-na threshold for
  # the upper area
  inds_below_upper_threshold <- below_upper_threshold_df$IndicatorID |> unique()
  df_threshold_upper <- df_tp |>
    filter(!(.data$IndicatorID %in% inds_below_upper_threshold))

  if (length(inds_below_upper_threshold) > 0) {
    warning(
      paste0(
        "The following indicators have fewer than ",
        tp_threshold_upper,
        " time points for ",
        unique(below_upper_threshold_df$AreaName),
        " and have been removed for all areas: \n",
        paste(inds_below_upper_threshold, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Threshold removals: lower

  # Check if any remaining indicators are below the lower area timperiod threshold
  below_lower_threshold_df <- tp_threshold_check(
    data = df_threshold_upper,
    tp_area_codes = area_codes_lower,
    tp_threshold = tp_threshold_lower
  )

  # Remove indicators which are below lower geog threshold for those areas only
  df_threshold_lower <- df_threshold_upper |>
    anti_join(below_lower_threshold_df,
              by = join_by("IndicatorID",
                           "AreaCode",
                           "Sex",
                           "Age"))

  if (nrow(below_lower_threshold_df) > 0) {

    removed_tp_text <- below_lower_threshold_df |>
      distinct(AreaName, IndicatorID) |>
      group_by(AreaName) |>
      summarise(
        indicators = paste(IndicatorID, collapse = ", "),
        .groups = "drop"
      ) |>
      mutate(
        text = paste0(AreaName, " - ", indicators)
      ) |>
      pull(text) |>
      paste(collapse = "\n")

    warning(
      paste0(
        "The following indicators have fewer than ",
        tp_threshold_lower,
        " time points and have been removed:\n",
        removed_tp_text
      ),
      call. = FALSE
    )
  }


  # Threshold removals: non-na upper

  # Check which indicators are below the non-na value threshold for the upper area
  below_upper_na_threshold_df <- na_threshold_check(
    data = df_threshold_lower,
    na_area_codes = area_code_upper,
    na_threshold = na_threshold_upper
  )

  # Remove an indicator for all areas if it is below the non-na threshold for
  # the upper area
  inds_below_upper_na_threshold <- below_upper_na_threshold_df$IndicatorID |> unique()
  df_na_threshold_upper <- df_threshold_lower |>
    filter(!(.data$IndicatorID %in% inds_below_upper_na_threshold))

  if (length(inds_below_upper_na_threshold) > 0) {
    warning(
      paste0(
        "The following indicators have fewer than ",
        na_threshold_upper,
        " non-na values for ",
        unique(below_upper_na_threshold_df$AreaName),
        " and have been removed for all areas: \n",
        paste(inds_below_upper_na_threshold, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Threshold removals: non-na lower

  # Check if any remaining indicators are below the lower area non-na threshold
  below_lower_na_threshold_df <- na_threshold_check(
    data = df_na_threshold_upper,
    na_area_codes = area_codes_lower,
    na_threshold = na_threshold_lower
  )

  # Remove indicators which are below lower geog threshold for those areas only
  df_na_threshold_lower <- df_na_threshold_upper |>
    anti_join(below_lower_na_threshold_df,
              by = join_by("IndicatorID",
                           "AreaCode",
                           "Sex",
                           "Age"))

  if (nrow(below_lower_na_threshold_df) > 0) {

    removed_na_text <- below_lower_na_threshold_df |>
      distinct(AreaName, IndicatorID) |>
      group_by(AreaName) |>
      summarise(
        indicators = paste(IndicatorID, collapse = ", "),
        .groups = "drop"
      ) |>
      mutate(
        text = paste0(AreaName, " - ", indicators)
      ) |>
      pull(text) |>
      paste(collapse = "\n")

    warning(
      paste0(
        "The following indicators have fewer than ",
        na_threshold_lower,
        " non-na values and have been removed:\n",
        removed_na_text
      ),
      call. = FALSE
    )
  }


  # ---- Data preparation ----

  # Join owner profile information and update indicator-level fields
  df_prep <- df_na_threshold_lower |>
    left_join(
      other_prof_all |>
        select("IndicatorID", "OwnerProfileName", "OwnerProfileUrlKey"),
      by = join_by("IndicatorID")
    ) |>
    group_by(.data$IndicatorID) |>
    mutate(
      IndicatorName = get_simple_name(all_meta, unique(.data$IndicatorID)),
      Decimal_places = get_decimal_places(.data$Value)
    ) |>
    ungroup()


  # Format time period and age label columns
  df_dp <- df_prep |>
    mutate(
      Timeperiod = factor(.data$Timeperiod, ordered = TRUE),
      Age = format_age_labels(.data$Age)
    )


  # ---- URLs ----

  # add AreaTypeID column
  df_dp <- df_dp |>
    left_join(area_df, by = join_by("AreaCode"))

  df_url <- add_url_columns(
    data = df_dp,
    data_name = data_name,
    ind_df = ind_df,
    generate_urls = generate_urls
  )


  # ---- Attach indicator metadata ----

  df_meta <- df_url |>
    group_by(.data$IndicatorID) |>
    mutate(
      Definition = get_simple_definition(all_meta, unique(.data$IndicatorID)),
      Unit = all_meta[[as.character(unique(.data$IndicatorID))]]$Unit$Label,
      ValueType = all_meta[[as.character(unique(.data$IndicatorID))]]$ValueType$Name,
      YearType = all_meta[[as.character(unique(.data$IndicatorID))]]$YearType$Name,
      DataSource = all_meta[[as.character(unique(.data$IndicatorID))]]$Descriptive$DataSource
    ) |>
    ungroup()


  # ---- Output + save ----

  df <- df_meta |>
    select(
      c(
        "IndicatorID",
        "IndicatorName",
        "AreaCode",
        "AreaName",
        "AreaType",
        "Timeperiod",
        "Sex",
        "Age",
        "Value",
        "LowerCI95.0limit",
        "UpperCI95.0limit",
        "TimeperiodSortable",
        "YearRange",
        "TimeperiodOrder",
        "OwnerProfileName",
        "TrendURL",
        "DefinitionURL",
        "ProfileURL",
        "ProfNameLC",
        "IndicatorNameLC",
        "Decimal_places",
        "Definition",
        "Unit",
        "ValueType",
        "YearType",
        "DataSource"
      )
    )

  # could also exclude: "AreaCode", "AreaType", "OwnerProfileName"

  # save the data into an rds file
  file_path <- file.path("healthtrendsfiles", "data", data_name)

  readr::write_rds(df, file_path)

  data_note <- paste0("  Data file saved to ", file_path, "\n")

  cat(data_note)

  df
}
#------------------------------------------------------------------------------#
