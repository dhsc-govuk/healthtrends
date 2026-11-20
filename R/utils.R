#------------------------------------------------------------------------------#
# Helper functions----
#------------------------------------------------------------------------------#

#' Replace characters with HTML symbols
#'
#' `clean_html` removes or replaces HTML code with the relevant symbols
#'
#' @param html_string character string, containing HTML code
#' @return clean character string
#' @noRd
clean_html <- function(html_string) {
  if (!is.character(html_string) || !(length(html_string) == 1)) {
    stop("Error: input must be a single character string")
  }

  string <- str_replace_all(html_string, "<.*?>", "")
  string <- str_replace_all(string, "&nbsp;", " ")
  string <- str_replace_all(string, "&.squo;", "'")
  string <- str_replace_all(string, "&rsquo;", "'")
  string <- str_replace_all(string, "&frac12;", "\\u00bd") # ascii for "½"
  string <- str_replace_all(string, "&sup2;", "\\u00b2") # ascii for "²"
  string <- str_replace_all(string, "\r\n", "")

  string
}

#------------------------------------------------------------------------------#
#' Remove HTML code from a metadata list for a single indicator
#'
#' `remove_meta_html` is used to remove HTML code from a metadata list for a
#' single indicator. This is applied only to free-text fields of the metadata
#' that are used to produce HtiE. Namely, the "Name", "Definition",
#' "DataSource", "SimpleName" and "SimpleDefinition"  of the "Descriptive"
#' sub-list.
#'
#' @param meta_list list, metadata for a single indicator containing the
#' "Descriptive" sub-list
#' @return list, metadata for a single indicator containing the "Descriptive"
#' sub-list with HTML code removed for the "Name", "Definition", "DataSource",
#' "SimpleName" and "SimpleDefinition" fields
#' @noRd
remove_meta_html <- function(meta_list) {
  # stop the function if meta_list is not a list
  if (!is.list(meta_list)) {
    stop("The function input must be a list.")
  }

  # stop the function if the Descriptive sublist does not exist
  if (is.null(meta_list$Descriptive)) {
    stop("The 'Descriptive' sublist does not exist in the provided list.")
  }

  # take out the Descriptive sublist
  sublist <- meta_list$Descriptive

  # apply the function to specific named elements
  selected_fields <- c(
    "Name",
    "Definition",
    "DataSource",
    "SimpleName",
    "SimpleDefinition"
  )

  # check if these names exist in the list and apply the function
  for (field in selected_fields) {
    if (field %in% names(sublist)) {
      sublist[[field]] <- clean_html(sublist[[field]])
    }
  }

  meta_list$Descriptive <- sublist

  meta_list
}

#------------------------------------------------------------------------------#
#' Pull profile owner information
#'
#' `profile_owner` pulls profile owner ID, name and URL key from fingertips for
#' specified indicators
#'
#' @param indicators numeric vector, IDs of the indicators of interest
#' @return dataframe with IndicatorID, OwnerProfileId, OwnerProfileName and
#' OwnerPRfileUrlkey
#' @noRd
profile_owner <- function(indicators) {
  if (is.null(indicators)) {
    stop("indicators must have an input")
  }

  # get api address from config
  api_url <- config::get(value = "api_url", config = "production")

  # Get url to api for fingertips profile owner metadata
  url_prof_own <- paste0(
    api_url,
    "indicator_metadata/indicator_owner?indicator_ids=%s"
  )

  # Get profile owner dataframe
  profile_owner_df <- fingertipsR::get_fingertips_api(paste0(sprintf(
    url_prof_own, paste0(indicators, collapse = "%2C%20")
  ))) |>
    select(-c("IndicatorName")) |>
    rename("IndicatorID" = "IndicatorId") |>
    distinct()

  profile_owner_df
}

#------------------------------------------------------------------------------#
#' Find other profile containing indicator data
#'
#' `find_other_profile` is for use with indicators which owner profiles
#' don't contain data at the specified area type level. Returns the information
#' for profiles which have data at the specified area type level.
#'
#' @param indicators numeric vector, indicator IDs
#' @param areatypeid double, area type ID of interest; 502 for UTLAs, 6 for
#'  region and 15 for England
#' @return data frame of indicators and profile information for data at the
#'  specified area type level
#' @noRd
find_other_profile <- function(indicators, areatypeid) {
  # get api address from config
  api_url <- config::get(value = "api_url", config = "production")

  # find which profile contains data for missing inds
  other_prof <- fingertipsR::get_fingertips_api(
    paste0(
      api_url,
      "profiles_containing_indicators?indicator_ids=",
      paste0(indicators, collapse = "%2C%20"),
      "&area_type_id=",
      areatypeid
    )
  )

  if (length(other_prof) == 0) {
    warning(
      paste0(
        "The following missing indicators did not find an alternative profile for AreaTypeID ",
        areatypeid,
        ": ",
        paste0(indicators, collapse = ", ")
      ),
      call. = FALSE
    )
    other_prof <- tibble(
      IndicatorID = integer(),
      OwnerProfileId = integer(),
      OwnerProfileName = character(),
      OwnerProfileUrlKey = character()
    )
    return(other_prof)
  }

  # retrieve profile name for each indicator
  profs <- other_prof |>
    map(\(x) pluck(x, "ProfileName", 1)) |>
    utils::stack() |>
    mutate(IndicatorID = as.numeric(as.character(.data$ind))) |>
    select(!"ind")

  # find details for each profile
  prof_key <- fingertipsR::get_fingertips_api(paste0(api_url, "profiles")) |>
    filter(.data$Name %in% profs$values) |>
    right_join(profs, by = c("Name" = "values")) |>
    select(
      "IndicatorID",
      "OwnerProfileId" = "Id",
      "OwnerProfileName" = "Name",
      "OwnerProfileUrlKey" = "Key"
    )

  if (length(setdiff(indicators, prof_key$IndicatorID)) != 0) {
    warning(
      paste0(
        "The following missing indicators did not find an alternative profile for AreaTypeID ",
        areatypeid,
        ": ",
        paste0(setdiff(indicators, prof_key$IndicatorID), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  prof_key
}


#------------------------------------------------------------------------------#
#' Get valid ONS area codes from the Fingertips API
#'
#' `get_valid_area_codes` queries the Fingertips API for every area code
#' available at the given area type(s). Used to validate user supplied area
#' codes before generating reports, so that codes Fingertips does not hold
#' (e.g. codes from a newer boundary set) can be caught early.
#'
#' @param areatypeids numeric vector, Fingertips area type IDs to look up
#'   (502 for UTLAs, 6 for regions, 15 for England)
#' @return character vector of valid ONS area codes for the given area type(s)
#' @noRd
get_valid_area_codes <- function(areatypeids) {

  api_url <- config::get(value = "api_url", config = "production")

  areatypeids |>
    unique() |>
    lapply(function(id) {
      fingertipsR::get_fingertips_api(
        paste0(api_url, "areas/by_area_type?area_type_id=", id)
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::pull("Code")
}
#------------------------------------------------------------------------------#
#' Correct superseded Barnsley and Sheffield area codes
#'
#' `correct_superseded_codes` swaps the 2025 Barnsley and Sheffield ONS area
#' codes for the previous ("old") codes that Fingertips still reports against.
#' Fingertips has not been updated to the 2025 boundary changes, so a user
#' entering the current ONS code would get no data back. Any code swapped is
#' reported via a warning; all other codes are returned unchanged.
#'
#' @param area_codes character vector of ONS area codes
#' @return character vector of area codes with any superseded Barnsley/Sheffield
#'   codes replaced by the codes Fingertips holds
#' @noRd
correct_superseded_codes <- function(area_codes) {
  superseded_areas <- data.frame(
    new_code = c("E08000038", "E08000039"),
    name     = c("Barnsley",  "Sheffield"),
    old_code = c("E08000016", "E08000019"),
    stringsAsFactors = FALSE
  )

  superseded_hits <- superseded_areas[superseded_areas$new_code %in% area_codes, ]

  if (nrow(superseded_hits) > 0) {

    # swap each superseded code for the code Fingertips still reports against
    idx <- match(area_codes, superseded_hits$new_code)
    area_codes[!is.na(idx)] <- superseded_hits$old_code[idx[!is.na(idx)]]

    warning(
      "Corrected ", nrow(superseded_hits), " area code(s) to the codes ",
      "Fingertips still reports against.\n",
      "Fingertips has not been updated to the 2025 ONS boundary changes, so ",
      "the previous code is used for the following:\n\n",
      paste0(
        sprintf(
          "  \u2022 %s: %s \u2192 %s",
          superseded_hits$name, superseded_hits$new_code, superseded_hits$old_code
        ),
        collapse = "\n"
      ),
      "\n",
      call. = FALSE
    )
  }

  area_codes
}
#------------------------------------------------------------------------------#
#' Identify missing indicators in a dataset
#'
#' Given a set of indicator IDs and a dataframe containing available indicators,
#' this function identifies which indicators are not present in the dataframe.
#'
#' @param indicators numeric vector, containing indicator IDs to check
#' @param df dataframe, containing an `IndicatorID` column
#'
#' @return A vector of indicator IDs that are missing from `df`.
#' @noRd
get_missing_inds <- function(indicators, df) {
  missing_vals <- !indicators %in% unique(df$IndicatorID)
  indicators[missing_vals]
}


#------------------------------------------------------------------------------#
#' Identify indicators below a time point threshold
#'
#' `tp_threshold_check()` Checks which indicators have trends with fewer than
#' tp_threshold time points, for specified areas. This allows for warnings or
#' removal of these indicators. Returns a dataframe of indicators with fewer
#' than tp_threshold time points.
#'
#' @param data dataframe of England and region data
#' @param tp_area_codes double vector, 9 character ONS area codes
#' @param tp_threshold numeric, threshold for the minimum number of time points
#'   in a time series
#' @return dataframe of indicators with fewer than tp_threshold time points
#' @noRd
tp_threshold_check <- function(data, tp_area_codes, tp_threshold) {
  below_threshold_inds <- data |>
    group_by(
      .data$IndicatorID,
      .data$AreaName,
      .data$AreaCode,
      .data$Sex,
      .data$Age,
      .data$YearRange
    ) |>
    summarise(n = n_distinct(.data$Timeperiod),
              .groups = "drop_last") |>
    filter(.data$AreaCode %in% tp_area_codes) |>
    filter(n < tp_threshold)

  below_threshold_inds
}


#------------------------------------------------------------------------------#
#' Identify indicators below a non-na value threshold
#'
#' `na_threshold_check()` Checks which indicators have fewer than na_threshold
#' non-na values, for specified areas. This allows for warnings or removal of
#' these indicators. Returns a dataframe of indicators with fewer than
#' na_threshold non-na values.
#'
#' @param data dataframe of England and region data
#' @param na_area_codes double vector, 9 character ONS area codes
#' @param na_threshold numeric, threshold for the minimum number of non-na
#'   values
#' @return dataframe of indicators with fewer than na_threshold non-na values
#' @noRd
na_threshold_check <- function(data, na_area_codes, na_threshold) {
  below_na_threshold_inds <- data |>
    group_by(
      .data$IndicatorID,
      .data$AreaName,
      .data$AreaCode,
      .data$Sex,
      .data$Age,
      .data$YearRange
    ) |>
    summarise(non_na = sum(!is.na(.data$Value)),
              .groups = "drop_last") |>
    filter(.data$AreaCode %in% na_area_codes) |>
    filter(non_na < na_threshold)

  below_na_threshold_inds
}

#------------------------------------------------------------------------------#
#' Create a dataframe of indicators and one age category per indicator
#'
#' `select_age` identifies indicators with multiple age categories and pulls the
#' main age categories from the fingertips API so that there is only 1 age group
#' per indicator. Used to fix issue of multiple age categories per indicator.
#'
#' @param data dataframe with indicators and age categories
#' @return dataframe of indicator IDs and age categories
#' @noRd

select_age <- function(data) {
  api_url <- config::get(value = "api_url", config = "production")
  data <- data |>
    select("IndicatorID", "Age") |>
    distinct() |>
    group_by(.data$IndicatorID)

  # keep indicators with single age category
  single <- data |>
    filter(n() == 1) |>
    ungroup()

  # indicators with multiple age categories
  multiple <- data |>
    filter(n() > 1) |>
    ungroup()

  # if indicators with multiple age categories exist
  # create a df with a single main age category per indicator
  if (nrow(multiple) >= 1) {
    # get profile ids for indicators with multiple age categories
    prof_mult <- profile_owner(multiple$IndicatorID)

    # url for age metadata
    url_age <- paste0(
      api_url,
      "grouproot_summaries/by_indicator_id?indicator_ids=%s&profile_id=%s&area_type_id=15"
    )

    # create a df with main age categories
    for (i in seq_len(nrow(prof_mult))) {
      if (i == 1) {
        selected_age_df <- data.frame()
      }

      age_meta <- fingertipsR::get_fingertips_api(paste0(
        sprintf(
          url_age,
          prof_mult$IndicatorID[i],
          prof_mult$OwnerProfileId[i]
        )
      )) |>
        select(IndicatorID = "IID", Age = "Age.Name")

      selected_age_df <- selected_age_df |>
        bind_rows(age_meta)
    }

    # Create df of single age categories for all indicators
    selected_age <- single |>
      bind_rows(selected_age_df)
  } else {
    selected_age <- single
  }

  selected_age
}

#------------------------------------------------------------------------------#
#' Create a dataframe of indicators with one timeperiod category per indicator
#'
#' `select_tp` creates a data frame of indicator IDs with a single timeperiod
#' series. Used to fix issue of multiple timeperiod series per indicator.
#' Logic for deciding which timeperiod series to keep:
#' 1. keep the one with multiple timepoints over a single timepoint
#' 2. keep the one with single years over the one with multiple years
#'
#' @param data dataframe with indicators, timeperiod series and year ranges
#' @param tp_threshold numeric, threshold for the minimum number of time points
#' in a time series. Indicators with time series below this threshold will be
#' removed
#' @return dataframe of indicators and timeperiod series
#' @noRd
select_tp <- function(data, tp_threshold) {
  # keep indicators with single time series
  single <- data |>
    select("IndicatorID", "Timeperiod", "YearRange") |>
    distinct() |>
    group_by(.data$IndicatorID) |>
    filter(n() == 1) |>
    ungroup()

  # check if any indicators have multiple time series
  if (length(single$IndicatorID) != length(unique(data$IndicatorID))) {
    # keep indicators with multiple time series at or above tp threshold
    multiple <- data |>
      select("IndicatorID", "Timeperiod", "YearRange") |>
      distinct() |>
      group_by(.data$IndicatorID) |>
      filter(n() > 1) |>
      ungroup() |>
      mutate(n_tp = lengths(.data$Timeperiod)) |>
      filter(.data$n_tp >= tp_threshold) |>
      select(-c("n_tp"))

    # keep indicators that now have a single time series
    single_threshold <- multiple |>
      group_by(.data$IndicatorID) |>
      filter(n() == 1) |>
      ungroup()

    # for remaining inds, select those with the minimum year range
    single_yearrange <- multiple |>
      group_by(.data$IndicatorID) |>
      filter(n() > 1) |>
      mutate(minrange = min(.data$YearRange)) |>
      filter(.data$minrange == .data$YearRange) |>
      ungroup()

    selected_tp <- single |>
      bind_rows(single_threshold, single_yearrange) |>
      select("IndicatorID", "Timeperiod")
  } else {
    selected_tp <- single |>
      select("IndicatorID", "Timeperiod")
  }

  selected_tp
}

#------------------------------------------------------------------------------#
#' Create a dataframe with complete timeperiod series and other metadata for
#' each indicator
#'
#' `complete_trend` creates a data frame of indicator IDs and complete
#' timeperiod series, with one timeperiod series and one age category per
#' indicator. Pulls time periods from the metadata to ensure complete timeperiod
#' series where there is missing data.
#' All columns included: IndicatorID, Timeperiod, Sex, SexID, Age, AgeID,
#' YearRange, TimeperiodOrder2, TimeperiodOrder
#'
#' @param indicators numeric vector, ID of the indicator of interest
#' @param tp_threshold numeric, threshold for the minimum number of time points
#' in a time series. Indicators with time series below this threshold will be
#' removed
#' @return dataframe with indicator ID, complete time periods, age, year range
#' and timeperiod order
#' @noRd
complete_trend <- function(indicators, tp_threshold) {
  api_url <- config::get(value = "api_url", config = "production")

  # get url for fingertips API
  url_trend <- paste0(
    api_url,
    "trend_data/specific_indicators_for_child_areas?area_type_id=15&parent_area_code=e92000001&indicator_ids=%s"
  )

  # get metadata with complete timeperiods series from fingertips API
  trend_meta_raw <- fingertipsR::get_fingertips_api(paste0(sprintf(
    url_trend, paste0(indicators, collapse = "%2C%20")
  )))


  trend_meta <- trend_meta_raw |>
    select(
      "IndicatorID" = "IID",
      "Timeperiod" = "Periods",
      "Sex" = "Sex.Name",
      "SexID" = "Sex.Id",
      "Age" = "Age.Name",
      "AgeID" = "Age.Id",
      "YearRange"
    )

  trend_meta_sex <- trend_meta |>
    group_by(.data$IndicatorID) |>
    filter(if ("Persons" %in% .data$Sex) {
      .data$Sex == "Persons"
    } else {
      .data$Sex == .data$Sex
    }) |>
    unique() |>
    ungroup()

  # df with only one age category per indicator
  select_age_df <- select_age(data = trend_meta_sex)

  # keep only one age category per indicator
  trend_meta_age <- trend_meta_sex |>
    right_join(select_age_df, by = join_by("IndicatorID", "Age"))

  # df with only one time series per indicator
  select_tp_df <- select_tp(data = trend_meta_age, tp_threshold)

  # keep only one time series per indicator
  trend_meta_timeperiods <- trend_meta_age |>
    right_join(select_tp_df, by = join_by("IndicatorID", "Timeperiod"))

  # check if any indicators have multiple time period series (should be 0)
  check_periods <- trend_meta_timeperiods |>
    select("IndicatorID", "Timeperiod") |>
    distinct() |>
    count(.data$IndicatorID) |>
    filter(n > 1)

  # stop if any indicators have multiple timeperiod series
  if (nrow(check_periods) > 0) {
    stop(
      paste0(
        "The following indicators have multiple timeperiod series: ",
        check_periods$IndicatorID
      )
    )
  }

  # add timeperiod order columns
  trend_df <- trend_meta_timeperiods |>
    unnest("Timeperiod") |>
    mutate(TimeperiodOrder2 = row_number()) |>
    mutate(TimeperiodOrder = row_number(), .by = "IndicatorID")

  trend_df
}

#------------------------------------------------------------------------------#
#' Retrieve simple indicator name
#'
#' `get_simple_name` retrieves the simple indicator name from the metadata
#' if available. If not populated, it returns the original indicator name
#'
#' @param all_meta list, metadata file for all indicators
#' @param ind numeric, indicator ID
#' @return character string, new indicator name
#' @noRd
get_simple_name <- function(all_meta, ind) {
  meta <- all_meta[[as.character(ind)]]

  if ("SimpleName" %in% names(meta$Descriptive)) {
    name <- meta$Descriptive$SimpleName
  } else {
    name <- meta$Descriptive$Name
  }

  name
}

#------------------------------------------------------------------------------#
#' Create fingertips links columns
#'
#' `fingertips_link` creates urls to the trend and metadata pages on
#' fingertips for an indicator to go on the details pages
#'
#' @param data dataframe, full dataset of all indicators and areas
#' @param data_name character string, name of dataset being used to create
#' the page.
#' @return dataframe with two additional columns for trend and definition
#' fingertips page. Dataframe filtered for all rows which need distinct
#' fingertips links
#' @noRd
fingertips_link <- function(data, data_name) {
  api_url <- config::get(value = "api_url", config = "production")

  # get all rows which need distinct fingertips links
  distinct_link_df <- data |>
    select(
      "IndicatorID",
      "AreaCode",
      "AreaName",
      "SexID",
      "AgeID",
      "YearRange",
      "OwnerProfileName",
      "OwnerProfileUrlKey",
      "AreaTypeID"
    ) |>
    distinct()

  # pull start of url from the API for all ind profiles (returns list)
  url_prof_list <- fingertipsR::get_fingertips_api(paste0(
    api_url,
    "profiles_containing_indicators?indicator_ids=",
    paste0(unique(distinct_link_df$IndicatorID), collapse = "%2C%20")
  ))

  # all inds and their profiles
  url_prof_df <- list_rbind(url_prof_list, names_to = "IndicatorID") |>
    mutate(IndicatorID = as.numeric(.data$IndicatorID)) |>
    rename(TrendURL = "Url")

  # filter for profiles used in HtiE data
  link_df <- distinct_link_df |>
    left_join(url_prof_df,
      by = join_by("IndicatorID", "OwnerProfileName" == "ProfileName")
    )

  # beginning of url
  trend_url_df <- link_df |>
    mutate(
      TrendURL = .data$TrendURL |>
        str_replace(
          paste0(.data$OwnerProfileUrlKey, "#"),
          paste0(.data$OwnerProfileUrlKey, "/data#page/4/")
        ) |>
        str_replace("https://testprofiles", "http://fingertips") |>
        str_replace("phe.org.uk", "phe.org.uk/profile") |>
        str_remove_all("ati/.*")
    )

  # the middle of url that is different for england and regions
  middle_url_df <- trend_url_df |>
    mutate(TrendURL = if_else(
      .data$AreaName %in% "England",
      paste0(
        .data$TrendURL,
        "pat/159/par/K02000001/ati/15/are/E92000001/iid/"
      ),
      paste0(
        .data$TrendURL,
        "pat/15/par/E92000001/ati/",
        .data$AreaTypeID,
        "/are/",
        .data$AreaCode,
        "/iid/"
      )
    ))


  # the end of url
  end_url_df <- middle_url_df |>
    mutate(
      TrendURL = paste0(
        .data$TrendURL,
        .data$IndicatorID,
        "/age/",
        .data$AgeID,
        "/sex/",
        .data$SexID,
        "/yrr/",
        .data$YearRange
      )
    )

  # add column for definitions url
  definitions_url_df <- end_url_df |>
    mutate(DefinitionURL = .data$TrendURL |>
             str_replace(paste0("/data#page/4/"), paste0("/data#page/6/")))

  # substitute generic link into DefinitionURL if needed
  fingertips_link_df <- definitions_url_df |>
    mutate(DefinitionURL = if_else(
      .data$TrendURL %in% paste0("http://fingertips.phe.org.uk/search/", .data$IndicatorID),
      paste0("http://fingertips.phe.org.uk/search/", .data$IndicatorID),
      .data$DefinitionURL
    ))

  fingertips_link_df
}

#------------------------------------------------------------------------------#
#' Create fingertips profile links and name columns
#'
#' `profile_link` generates a dataframe with fingertips profile url and name
#' columns for profiles within the passed in dataframe
#'
#' @param data dataframe, must contain OwnerProfileUrlKey column
#' @param ind_df dataframe, must contain IndicatorID and ProfileID columns
#' @return dataframe with profile url and lower case name columns
#' @noRd
profile_link <- function(data, ind_df) {
  # get api address from config
  api_url <- config::get(value = "api_url", config = "production")
  # select profiles used in HtiE
  ind_id <- data |>
    pull(.data$IndicatorID) |>
    unique()

  profile_id <- ind_df |>
    filter(.data$IndicatorID %in% ind_id) |>
    select("IndicatorID", "ProfileId")

  # get profile info
  prof_info <- fingertipsR::get_fingertips_api(paste0(api_url, "profiles"))

  # check if each profile id from csv exists in api
  profile_check <- unique(profile_id$ProfileId) %in% prof_info$Id

  # find any invalid profile ids
  invalid_prof_ids <- unique(profile_id$ProfileId)[!profile_check]

  # handle invalid profile ids
  if (length(invalid_prof_ids) > 0) {
    stop(paste(
      "Profile IDs in Indicator CSV are invalid:",
      paste(invalid_prof_ids, collapse = ", ")
    ))
  }

  # generate urls for profiles
  url_df <- prof_info |>
    select("Id", "Name", "Key", "HasOwnFrontPage") |>
    filter(.data$Id %in% unique(profile_id$ProfileId)) |>
    mutate(ProfilePageType = if_else(.data$HasOwnFrontPage, "profile/", "profile-group/")) |>
    mutate(
      ProfileURL = paste0(
        "https://fingertips.phe.org.uk/",
        .data$ProfilePageType,
        .data$Key
      )
    )

  # format name to be used within text to describe link
  prof_name_df <- url_df |>
    mutate(ProfNameLC = str_to_lower(.data$Name)) |>
    mutate(ProfNameLC = if_else(
      str_detect(.data$ProfNameLC, "profile", negate = TRUE),
      paste0(.data$ProfNameLC, " profile"),
      .data$ProfNameLC
    )) |>
    select("Id", "CSVProfileUrlKey" = "Key", "ProfileURL", "ProfNameLC")

  profile_url_df <- profile_id |>
    left_join(prof_name_df, by = join_by("ProfileId" == "Id")) |>
    select(
      "IndicatorID",
      "CSVProfileUrlKey",
      "ProfileURL",
      "ProfNameLC"
    )

  profile_url_df
}

#------------------------------------------------------------------------------#
#' Lower all words in a string, excluding acronyms
#'
#' `lowercase_except_acronyms` is used lower all words in a string, except for
#' acronyms or exceptions. Acronyms are assumed to be words with two or more
#' consecutive capital letters or capital letters separated by dots. Exceptions
#' are manually identified phrases chosen to be except from processing. Warnings
#' are generated when any mid-sentence word is decapitalised and when any
#' acronyms or exception phrases remain.
#'
#' @param text character string, text to be processed
#' @return character string, processed text with all words decapitalised except
#' acronyms or exceptions
#' @noRd
lowercase_except_acronyms <- function(text) {
  # save unprocessed text for warnings in for loop
  text_original <- text

  # list of phrases exempt from being modified
  exceptions <- c("Homelessness Reduction Act")

  # Replace exception phrases with temporary placeholders
  for (exception in exceptions) {
    placeholder <- str_replace_all(exception, "\\s+", "_")
    placeholder <- paste0("TEMP", placeholder, "TEMP")
    text <- str_replace(text, exception, placeholder)
  }

  # split the text into words
  words <- str_split(text, "\\s+")[[1]]

  processed_sentence <- c()
  first_word <- TRUE

  for (w in words) {
    # identify exception phrase placeholders and restore exception phrases to original state
    if (str_detect(w, "TEMP")) {
      processed_word <- w |>
        str_remove_all("TEMP") |>
        str_replace_all("_", " ")
      # include exception phrase warning
      print(
        paste0(
          'For the text "',
          text_original,
          '" the phrase "',
          processed_word,
          '" is listed as an exception and remains unchanged'
        )
      )

      # Assume is acronym if:
      # two or more consecutive capital letters or capital letters separated
      # by dots
    } else if (str_detect(w, "[A-Z]{2,}") ||
                 str_detect(w, "([A-Z]\\.){2,}")) {
      processed_word <- w
      # include acronym warning
      warning(
        'For the text "',
        text_original,
        '" the acronym "',
        w,
        '" remains capitalised'
      )
    } else {
      processed_word <- str_to_lower(w)
      # include warning if any mid-sentence words have been decapitalised
      if (!first_word && str_detect(w, "[A-Z]")) {
        warning(
          'For the text "',
          text_original,
          '" the word "',
          w,
          '" has been decapitalised mid-sentence'
        )
      }
    }

    first_word <- FALSE
    processed_sentence <- c(processed_sentence, processed_word)
  }

  final_sentence <- str_c(processed_sentence, collapse = " ")

  final_sentence
}

#------------------------------------------------------------------------------#
#' Retrieve simple indicator definition
#'
#' `get_simple_definition` retrieves the simple indicator definition from the metadata
#' if available. If not populated, it returns the original indicator definition
#'
#' @param all_meta list, metadata file for all indicators
#' @param ind numeric, indicator ID
#' @return character string, new indicator definition
#' @noRd
get_simple_definition <- function(all_meta, ind) {
  meta <- all_meta[[as.character(ind)]]

  if ("SimpleDefinition" %in% names(meta$Descriptive)) {
    definition <- meta$Descriptive$SimpleDefinition
  } else {
    definition <- meta$Descriptive$Definition
  }

  definition
}

#------------------------------------------------------------------------------#
#' Get indicator CSV
#'
#' `get_indicator_csv()` retrieves either the indicator CSV file or a vector of
#' indicators.
#'
#' @param as_df logical, if TRUE this returns the whole CSV
#'  as a dataframe, if FALSE it returns a numeric vector of all indicators
#' @return either dataframe of CSV or numeric vector of indicators
#' @noRd
get_indicator_csv <- function(as_df = TRUE) {
  # Validate arguments
  if (!rlang::is_bool(as_df)) {
    rlang::abort(message = "`as_df` must be a single TRUE or FALSE value.")
  }

  ind_file_path <- config::get(value = "indicator_spec", config = "production")

  if (!file.exists(ind_file_path)) {
    rlang::abort(paste0(
      "Indicator specification file does not exist: ",
      ind_file_path
    ))
  }

  indicator_df <- readr::read_csv(ind_file_path, show_col_types = FALSE)

  if (nrow(indicator_df) == 0) {
    rlang::warn("The indicator CSV contains no rows.")
  }

  required_cols <- c("IndicatorID", "TopicName")
  missing_cols <- setdiff(required_cols, names(indicator_df))

  if (length(missing_cols) > 0) {
    rlang::abort(c(
      "Required column(s) missing from the indicator CSV.",
      "x" = paste0(
        "Missing: ", paste0("`", missing_cols, "`", collapse = ", ")
      )
    ))
  }

  duplicate_ids <- indicator_df |>
    dplyr::count(.data$IndicatorID, name = "n") |>
    dplyr::filter(n > 1) |>
    dplyr::pull(.data$IndicatorID)

  if (length(duplicate_ids) > 0) {
    rlang::abort(c(
      "Duplicate values found in `IndicatorID`.",
      "x" = paste("duplicated IDs:", paste(duplicate_ids, collapse = ", "))
    ))
  }

  if (as_df) {
    return(indicator_df)
  }

  dplyr::pull(indicator_df, .data$IndicatorID)
}


#------------------------------------------------------------------------------#
#' Get indicators by area type
#'
#' `get_indicators` checks which indicators are available for each geography
#'
#' @param data_file_name character string, name of data file to check
#' available geographies
#' @param indicator_file_name character string, name of indicator CSV file
#' @param area_code character, area code of chosen area for page
#' @param topic_names logical, if TRUE returns a vector of topic names for
#' the area type instead of the indicator dataframe
#' @return dataframe of indicators and topic names at selected area type,
#' or just topic_names vector if selected
#' @noRd

get_indicators <- function(data_file = "health_trends_data.rds",
                           area_code = "E92000001",
                           topic_names = FALSE) {
  # Validate arguments
  if (!is.character(data_file) ||
        length(data_file) != 1L || is.na(data_file)) {
    rlang::abort("`data_file` must be a single character string.")
  }

  if (!stringr::str_detect(data_file, "\\.[[:alpha:]]+$")) {
    rlang::abort("`data_file` must include a file extension, e.g. './input/all_data.rds'.")
  }

  if (!file.exists(data_file)) {
    rlang::abort(paste("File does not exist: ", data_file))
  }

  if (!is.logical(topic_names) ||
        length(topic_names) != 1L || is.na(topic_names)) {
    rlang::abort("`topic_names` must be TRUE or FALSE.")
  }

  indicator_df <- get_indicator_csv(as_df = TRUE)

  area_type <- recode_values(
    str_sub(area_code, 1, 3),
    c("E92") ~ "England",
    c("E12") ~ "Region",
    c("E06", "E08", "E09", "E10") ~ "UTLA"
  )

  inds_geog <- readr::read_rds(data_file) |>
    dplyr::select("IndicatorID", "AreaCode") |>
    dplyr::mutate(AreaType = recode_values(
      str_sub(.data$AreaCode, 1, 3),
      c("E92") ~ "England",
      c("E12") ~ "Region",
      c("E06", "E08", "E09", "E10") ~ "UTLA"
    )) |>
    dplyr::distinct(.data$IndicatorID, .data$AreaType) |>
    dplyr::left_join(indicator_df, by = dplyr::join_by("IndicatorID")) |>
    dplyr::arrange(.data$TopicOrder, .data$IndicatorOrder) |>
    filter(.data$AreaType == area_type) |>
    select("IndicatorID", "TopicName")

  if (topic_names) {
    inds_geog |>
      dplyr::pull(.data$TopicName) |>
      unique()
  } else {
    inds_geog
  }
}

#------------------------------------------------------------------------------#
#' Format area name
#'
#' `format_area_name` adds "the" in front of areas which need it
#'
#' @param area_name character string, name of area
#' @return character string, formatted area name
#' @noRd
format_area_name <- function(area_name) {
  takes_the <- c(
    "North East",
    "North West",
    "East Midlands",
    "West Midlands",
    "East of England",
    "South East",
    "South West"
  )
  if (area_name %in% takes_the) {
    paste("the", area_name)
  } else {
    area_name
  }
}

#------------------------------------------------------------------------------#
#' Get indicators available for an area
#'
#' `get_area_indicators` returns the indicators available for a specified area,
#' along with their topic names, ordered for use in the report.
#'
#' @param all_data dataframe, all indicator data
#' @param area_code character, area code of the chosen area
#'
#' @return a dataframe containing indicator IDs and topic names for the chosen
#'   area
#' @noRd
get_area_indicators <- function(all_data, area_code) {

  indicator_df <- get_indicator_csv(as_df = TRUE)

  area_indicators <- all_data |>
    select("IndicatorID", "AreaCode") |>
    filter(.data$AreaCode %in% area_code) |>
    distinct(.data$IndicatorID) |>
    left_join(
      indicator_df,
      by = join_by("IndicatorID")
    ) |>
    arrange(.data$TopicOrder, .data$IndicatorOrder) |>
    select("IndicatorID", "TopicName")

  area_indicators
}

#------------------------------------------------------------------------------#
#' Check if any indicator needs multiple summary page tiles
#'
#' `get_spotlight_df` identifies the indicators to feature on the summary
#' page and checks whether multiple tiles are needed based on the number of
#' sex categories for each indicator.
#'
#' @param all_data dataframe, all indicator data
#' @param area_code character, area code of the chosen area
#' @param spotlight character, either `"first"` to use the first indicator per
#'   topic, or `"all"` to use all indicators
#'
#' @return a dataframe containing indicator IDs, topic names, sex categories,
#'   and `multiple_tile`, a logical column indicating whether an indicator
#'   requires more than one summary page tile
#' @noRd
get_spotlight_df <- function(all_data, area_code, spotlight) {

  area_indicators <- get_area_indicators(
    all_data = all_data,
    area_code = area_code
  )

  if (spotlight == "first") {
    area_indicators <- area_indicators |>
      distinct(.data$TopicName, .keep_all = TRUE)
  }

  spotlight_df <- area_indicators |>
    left_join(
      all_data |>
        filter(.data$AreaCode %in% area_code) |>
        select("IndicatorID", "Sex") |>
        distinct(),
      by = "IndicatorID"
    ) |>
    group_by(.data$IndicatorID) |>
    mutate(multiple_tile = n() > 1) |>
    ungroup()

  spotlight_df
}

#------------------------------------------------------------------------------#
#' Retrieve number of sexes per indicator
#'
#' `ind_n_sex` retrieves the number of sexes per indicator
#'
#' @param ind numeric, indicator ID
#' @param all_data dataframe, all indicator data
#' @return numeric, number of sexes per indicator
#' @noRd
ind_n_sex <- function(ind, all_data) {
  ind_data <- all_data |>
    filter(.data$IndicatorID %in% ind)

  sex_labels <- ind_data |>
    distinct(.data$Sex) |>
    pull(.data$Sex)

  n_sex <- length(sex_labels)

  n_sex
}

#------------------------------------------------------------------------------#
#' Create data table
#'
#' `data_table` produces the data table for an indicator on the details page
#'
#' @param ind_data dataframe, indicator data for England or England and area
#' @param area_code character string, name of selected area
#' @param area_name  character string, area code of selected area
#' @param n_sex numeric, number of sexes for indicator
#' @return dataframe, data table to present on the details page
#' @noRd
data_table <- function(ind_data, area_code, area_name, n_sex) {
  # remove leading and/or trailing NAs
  ind_data <- ind_data |>
    # find the earliest and latest time across all areas where `value` is not
    # NA
    mutate(
      first_non_na_time = min(.data$TimeperiodSortable[!is.na(ind_data$Value)], na.rm = TRUE),
      last_non_na_time = max(.data$TimeperiodSortable[!is.na(.data$Value)], na.rm = TRUE)
    ) |>
    filter((.data$TimeperiodSortable >= .data$first_non_na_time) &
             (.data$TimeperiodSortable <= .data$last_non_na_time)) |>
    select(-c("first_non_na_time", "last_non_na_time"))

  ind_data <- ind_data |>
    mutate(
      Value = formatC(
        as.numeric(.data$Value),
        format = "f",
        big.mark = ",",
        digits = unique(.data$Decimal_places)
      ),
      LowerCI95.0limit = formatC(
        as.numeric(.data$LowerCI95.0limit),
        format = "f",
        big.mark = ",",
        digits = unique(.data$Decimal_places)
      ),
      UpperCI95.0limit = formatC(
        as.numeric(.data$UpperCI95.0limit),
        format = "f",
        big.mark = ",",
        digits = unique(.data$Decimal_places)
      )
    )

  # get columns for table depending on whether there are multiple sexes
  if (n_sex == 2) {
    table <- ind_data |>
      filter(.data$AreaCode %in% area_code) |>
      select("Timeperiod", "Value", "Sex") |>
      pivot_wider(names_from = "Sex", values_from = "Value")

    names(table) <- ifelse(
      names(table) %in% c("Male", "Female"),
      paste0(area_name, " Value, ", names(table)),
      names(table)
    )
  } else {
    table <- ind_data |>
      filter(.data$AreaCode %in% area_code) |>
      select(
        "Timeperiod",
        "Value",
        "LowerCI95.0limit",
        "UpperCI95.0limit"
      ) |>
      rename(
        "95% confidence interval lower limit" = "LowerCI95.0limit",
        "95% confidence interval upper limit" = "UpperCI95.0limit"
      )

    names(table) <- ifelse(names(table) %in% "Value",
      paste0(area_name, " Value"),
      names(table)
    )
  }

  table <- table |>
    mutate(Timeperiod = as.character(.data$Timeperiod)) |>
    rename("Time period" = "Timeperiod")

  # Add on England value if region data
  if (!(area_code == "E92000001")) {
    eng_df <- ind_data |>
      filter(.data$AreaCode == "E92000001") |>
      select("Timeperiod", "Value", "Sex") |>
      mutate(Timeperiod = as.character(.data$Timeperiod)) |>
      rename("Time period" = "Timeperiod")

    if (n_sex == 2) {
      eng_df <- eng_df |>
        pivot_wider(names_from = "Sex", values_from = "Value") |>
        rename(
          "England Value, Male" = "Male",
          "England Value, Female" = "Female"
        )
    } else {
      eng_df <- eng_df |>
        select(-c("Sex")) |>
        rename("England Value" = "Value")
    }

    table <- left_join(table, eng_df, by = "Time period")
  }

  table
}

#------------------------------------------------------------------------------#
#' Set up the working directory for healthtrends
#'
#' `import_healthtrends_files()` copies the Quarto templates, stylesheets and
#' default config that ship with the package into the working directory, so
#' that [generate_reports()] can run. It creates:
#' * `healthtrendsfiles/`, with the templates and empty `data/` and `Images/`
#'   folders
#' * `input/`, with the default `config.yaml`
#' * `output/`
#'
#' @param overwrite Logical. If `TRUE` (the default), copies a fresh set of
#'   files, replacing existing ones. This undoes any changes made with
#'   [set_header()] and any edits to `input/config.yaml`. If `FALSE`, copies
#'   nothing and keeps the existing files; `healthtrendsfiles/` must already
#'   exist. Either way, reports already in `output/` are left untouched.
#'
#' @examples
#' \dontrun{
#' import_healthtrends_files()
#' }
#' @export
import_healthtrends_files <- function(overwrite = TRUE) {
  if (!rlang::is_bool(overwrite)) {
    rlang::abort(message = "`overwrite` must be a single TRUE or FALSE value.")
  }

  dest_dir <- path_wd("healthtrendsfiles")

  if (!overwrite && !dir_exists(dest_dir)) {
    rlang::abort(
      message =
        paste("healthtrendsfiles folder does not exist:", dest_dir)
    )
  }

  if (!overwrite) {
    message(
      message = paste(
        "The following folders: 'healthtrendsfiles' and 'output' have",
        "not been created. Existing files in healthtrendsfiles will be used."
      )
    )
    return(invisible(NULL))
  }

  # Get quarto files and source files from package external data
  dir_create(dest_dir)

  extdir <- system.file("extdata", package = "healthtrends")
  dir_copy(extdir, dest_dir, overwrite = overwrite)

  # Setup folder structure
  if (!dir_exists("output")) {
    dir_create("output")
  }
  if (!dir_exists("healthtrendsfiles/data")) {
    dir_create("healthtrendsfiles/data")
  }
  if (!dir_exists("healthtrendsfiles/Images")) {
    dir_create("healthtrendsfiles/Images")
  }

  if (!dir_exists("input")) {
    dir_create("input")
  }

  # copy config
  config_dir <- system.file("conf", package = "healthtrends")
  dir_copy(config_dir, path_wd("input"), overwrite = TRUE)

  # Return a message
  if (dir.exists("healthtrendsfiles")) {
    message(
      "The following folders: 'healthtrendsfiles', 'output' and 'input' have been created and successfully populated with files requried to run healthtrends."
    )
  } else {
    rlang::abort(
      message =
        "'healthtrendsfiles' have not been successfully imported and are missing."
    )
  }
}
#------------------------------------------------------------------------------#
#' Format age label strings
#'
#' `format_age_labels` replaces abbreviations and symbols in age strings with
#' readable text, e.g. "yrs" to "years", "<" to "Under", ">=" to "At least".
#'
#' @param age character vector, raw age labels
#' @return character vector, cleaned age labels
#' @noRd
format_age_labels <- function(age) {
  age <- str_replace_all(age, "yrs", "years")
  age <- str_replace_all(age, "yr", "year")
  age <- str_replace_all(age, "-", " to ")
  age <- str_replace_all(age, "^<", "Under ")
  age <- str_replace_all(age, "<", "under ")
  age <- str_replace_all(age, "\\+ years", " years and over")
  age <- str_replace_all(age, "^>=", "At least ")
  age <- str_replace_all(age, ">=", "at least ")
  age <- str_replace_all(age, "\\.5 ", "\U00BD ")
  age
}

#------------------------------------------------------------------------------#
#' Calculate number of decimal places for an indicator
#'
#' `get_decimal_places` uses the minimum absolute value of an indicator to
#' decide how many decimal places are appropriate for display.
#'
#' @param values numeric vector, indicator values
#' @return numeric, number of decimal places (0, 1, or 2)
#' @noRd
get_decimal_places <- function(values) {
  decimal_places <- case_when(
    min(abs(values), na.rm = TRUE) < 1 ~ 2,
    min(abs(values), na.rm = TRUE) < 100 ~ 1,
    TRUE ~ 0
  )

  decimal_places
}

#------------------------------------------------------------------------------#
#' Format indicator definition
#'
#' `format_definition` converts the indicator definition (from metadata) into
#' the first sentence of the chart description in the details pages. The output
#' starts with "This line chart shows" and follows with the formatted definition
#' that reads naturally as a sentence.
#'
#' @param definition character string, containing the indicator definition text
#' @return character string, formatted indicator definition sentence
#' @noRd
format_definition <- function(definition) {
  # remove space at the end of the definition
  if (str_sub(definition, -1) == " ") {
    definition <- str_sub(definition, 1, -2)
  }

  # add full stop to end if not already there
  if (str_sub(definition, -1) != ".") {
    definition <- paste0(definition, ".")
  }

  # Add start of sentence, inserting 'the' at start if needed
  formatted_definition <- paste0(
    "The line chart shows ",
    if (str_detect(definition, regex("^(the|all)\\b", ignore_case = TRUE))) {
      ""
    } else {
      "the "
    },
    str_to_lower(str_sub(definition, 1, 1)),
    str_sub(definition, 2)
  )

  formatted_definition
}
#------------------------------------------------------------------------------#
#' Add URL columns to the data
#'
#' `add_url_columns` adds Fingertips trend, definition, and profile URL columns
#' to the data frame. If `generate_urls` is FALSE, all URL columns are added
#' as NA.
#'
#' @param data dataframe, the prepared data frame to add URL columns to
#' @param data_name character string, name of rds file to be outputted,
#' either the main data or user test data
#' @param ind_df dataframe, must contain IndicatorID and ProfileID columns
#' @param generate_urls logical, default TRUE to generate fingertips url
#' columns. FALSE creates column names, but all NA values. FALSE can be used to
#' reduce data generation time when only testing data
#' @return dataframe with URL columns appended
#' @noRd
add_url_columns <- function(data, data_name, ind_df, generate_urls = TRUE) {
  if (!generate_urls) {
    return(
      data |>
        mutate(
          TrendURL = NA,
          DefinitionURL = NA,
          CSVProfileUrlKey = NA,
          ProfileURL = NA,
          ProfNameLC = NA,
          IndicatorNameLC = NA
        )
    )
  }

  # Add fingertips links columns: trends and definitions
  fingertips_link_df <- fingertips_link(data = data, data_name)

  df_ft_link <- data |>
    left_join(
      fingertips_link_df,
      by = c(
        "IndicatorID",
        "AreaCode",
        "AreaName",
        "SexID",
        "AgeID",
        "YearRange",
        "OwnerProfileName",
        "OwnerProfileUrlKey"
      )
    )

  # Add profile links columns: profile url and lower case names
  profile_link_df <- profile_link(data = df_ft_link, ind_df)

  df_prof_link <- df_ft_link |>
    left_join(profile_link_df, by = join_by("IndicatorID"))

  # Add lower case (excluding acronyms) indicator names for fingertips links
  df_ind_name <- df_prof_link |>
    select("IndicatorName") |>
    distinct() |>
    mutate(IndicatorNameLC = sapply(.data$IndicatorName, lowercase_except_acronyms))

  df_prof_link |>
    left_join(df_ind_name, by = join_by("IndicatorName"))
}
#------------------------------------------------------------------------------#
#' sets configuration file
#'
#' `set_config` sets the configuration file and loads into the session environment
#'
#' @param config_nm character string, name of config when loaded
#' @param config_file character string, location of and name of config file to be use
#'
#' @noRd
set_config <- function(config_nm = "config",
                       config_file = "input/config.yaml") {
  if (!file.exists(config_file)) {
    rlang::abort(paste("Configuration file does not exist:", config_file))
  }

  Sys.setenv(R_CONFIG_FILE = config_file)

  Sys.setenv(R_CONFIG_ACTIVE = config_nm)

  invisible(config_nm)
}


#------------------------------------------------------------------------------#
#' Allows the user to set the header of the healthtrends report
#'
#' @param title character string, the title of the report. Default is "PLACEHOLDER_TITLE"
#' @param org character string, the name of the organisation. Default is "PLACEHOLDER_ORG"
#' @param org_url character string, the url of the organisation. Default is "https://example.org/"
#' @param logo_path character string, the path to the logo file. Default is a grayed out placeholder image
#'
#' @return Overwrites the default template with the user-defined title, organisation name, organisation url, and logo path.
#'
#' @examples
#' \dontrun{
#' set_header(
#'   title = "Health Trends in England",
#'   org = "Department of<br>Health & Social Care",
#'   org_url = "https://www.gov.uk/government/organisations/department-of-health-and-social-care",
#'   logo_path = NULL
#' )
#' }
#'
#' @export
set_header <- function(title = NULL,
                       org = NULL,
                       org_url = NULL,
                       logo_path = NULL) {

  # Check that healthtrendsfiles have been imported
  if (!dir.exists(path_wd("healthtrendsfiles"))) {
    stop(
      "healthtrendsfiles folder does not exist - please run import_healthtrends_files() first."
    )
  }

  # Set messages
  title_msg <- NULL
  org_msg <- NULL
  org_url_msg <- NULL
  logo_path_msg <- NULL

  # Get a copy of the govuk template files and copy to www folder
  dest_dir <- path_wd("healthtrendsfiles/www/")
  extdir <- system.file("extdata/www/HTS.template", package = "healthtrends")

  hts_template <- readr::read_file(extdir)
  if (!is.null(title)) {
    hts_template <- str_replace_all(hts_template, "PLACEHOLDER_TITLE", title)
    title_msg <- paste0(" - Header text has been set to '", title, "'.")
  }
  if (!is.null(org)) {
    hts_template <- str_replace_all(hts_template, "PLACEHOLDER_ORG", org)
    org_msg <- paste0(" - Organisation text has been set to '", org, "'.")
  }
  if (!is.null(org_url)) {
    hts_template <- str_replace_all(hts_template, "https://example.org/", org_url)
    org_url_msg <- paste0(" - Organisation url has been set to '", org_url, "'.")
  }

  if (!is.null(logo_path)) {
    dest_file <- file.path(dest_dir, basename(logo_path))

    same_file <- normalizePath(logo_path, mustWork = FALSE) ==
      normalizePath(dest_file, mustWork = FALSE)

    if (!same_file) {
      ok <- file.copy(logo_path, dest_dir, overwrite = TRUE)
      if (!ok) {
        warning("Logo file could not be copied: ", logo_path)
      } else {
        logo_path_msg <- paste0(" - Now using logo from the following path '", logo_path, "'.")
      }
    }

    hts_template <- str_replace_all(
      hts_template,
      "PLACEHOLDER_LOGO.svg",
      basename(logo_path)
    )
  }

  readr::write_file(hts_template, paste0(dest_dir, "/HTS.template"))

  # Send message
  if (all(is.null(c(title, org, org_url, logo_path)))) {
    message("No changes made as no paramters were changed.")
  } else {
    message(
      "The following were updated: \n",
      paste0(c(title_msg, org_msg, org_url_msg, logo_path_msg), collapse = "\n")
    )
  }

}

#------------------------------------------------------------------------------#
#' ONS area code prefix to Fingertips area type ID lookup
#'
#' Single source of truth mapping ONS area code prefixes to Fingertips area
#' type IDs (15 = England, 6 = region, 502 = UTLA). Used both to attach area
#' type IDs to area codes in `get_data` and to validate user supplied codes in
#' `generate_reports`.
#'
#' @return tibble with `prefix` and `AreaTypeID` columns
#' @noRd
area_type_lookup <- function() {
  tibble(
    prefix     = c("E92", "E12", "E06", "E08", "E09", "E10"),
    AreaTypeID = c(15,    6,     502,   502,   502,   502)
  )
}
