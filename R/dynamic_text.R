#' Dynamic text
#'
#' `dynamic_text` Produces indicator text using metadata description and the
#' first and last data points to calculate the trend
#'
#' @param area_data dataframe, indicator data for selected area
#' @param area_name character string, area name
#' @param n_sex numeric, number of sexes per indicator
#' @return character string, text description for indicator
#' @noRd
dynamic_text <- function(area_data, area_name, n_sex) {
  # TODO: work out area n n_sex within function? reduce unnecessary arguments

  # retrieve data
  df <- area_data |>
    drop_na("Value") |>
    mutate(
      Value = sprintf(
        paste0("%.", unique(.data$Decimal_places), "f"),
        .data$Value
      )
    )

  # check that there is enough non NA data before continuing
  if (nrow(df) < (2 * n_sex)) {
    stop("Cannot produce text: indicator has less than 2 years of data")
  }

  # build first sentence: definition
  definition <- unique(area_data$Definition)
  sentence1 <- format_definition(definition)

  # build sentence fragment: timeperiod (add Timeperiod_text column)
  df <- df |>
    generate_timeperiod_text()

  # combined description for indicators with two sexes (for details pages)
  if (n_sex == 2 && length(unique(df$Sex)) == 2) {
    chart_text <- paste0(
      sentence1, " For ",
      str_to_lower(unique(df$Sex)[1]), "s, the ",
      generate_trend_text(df, area_name, unique(df$Sex)[1]),
      " For ", str_to_lower(unique(df$Sex)[2]), "s, the ",
      generate_trend_text(df, area_name, unique(df$Sex)[2])
    )

    # single sex description for indicators with two sexes (for overview pages)
  } else if (n_sex == 2 && length(unique(df$Sex)) == 1) {
    sentence_le <- str_replace(
      sentence1,
      "person",
      str_to_lower(unique(df$Sex))
    )

    chart_text <- paste0(
      sentence_le, " For ",
      str_to_lower(unique(df$Sex)), "s, the ",
      generate_trend_text(df, area_name, unique(df$Sex))
    )
  } else {
    chart_text <- paste0(
      sentence1, " The ",
      generate_trend_text(df, area_name, sex = NULL)
    )
  }

  chart_text
}

#------------------------------------------------------------------------------#
#' Generate accessible Timeperiod text
#'
#' Returns a short text fragment describing the year type, range
#' (if applicable), and Timeperiod value in an accessible format. For
#' non-calendar years descriptive year-type text (e.g. academic or financial
#' year) is generated.
#'
#' @param df A data frame containing `Timeperiod`, `YearRange` and `YearType`
#' columns.
#'
#' @return The input data frame with an added `Timeperiod_text` column.
#' @noRd
generate_timeperiod_text <- function(df) {
  year_type <- unique(df$YearType)
  year_range <- unique(df$YearRange)

  # year structure: YYYY/YY - YYYY/YY
  if (any(str_detect(df$Timeperiod, "[0-9]{4}/[0-9]{2} - [0-9]{2}/[0-9]{2}"))) {
    # generate year_type_tex
    year_type_text <- get_year_type_text(
      year_type = year_type,
      year_range = year_range,
      include_range = TRUE
    )

    df$Timeperiod_text <- paste0(
      year_type_text,
      str_sub(df$Timeperiod, 1, 2),
      str_sub(df$Timeperiod, 14, 15)
    )

    # year structure: YYYY/YY
  } else if (any(str_detect(df$Timeperiod, "[0-9]{4}/[0-9]{2}"))) {
    # generate year_type_text
    year_type_text <- get_year_type_text(
      year_type = year_type,
      year_range = year_range,
      include_range = FALSE
    )

    df$Timeperiod_text <- paste0(
      year_type_text,
      str_sub(df$Timeperiod, 1, 2),
      str_sub(df$Timeperiod, 6, 7)
    )

    # year structure: YYYY - YY
  } else if (any(str_detect(df$Timeperiod, "[0-9]{4} - [0-9]{2}"))) {
    df$Timeperiod_text <- paste0(
      str_sub(df$Timeperiod, 1, 4), " to ",
      str_sub(df$Timeperiod, 1, 2),
      str_sub(df$Timeperiod, 8, 9)
    )

    # year structure: YYYY
  } else {
    df$Timeperiod_text <- df$Timeperiod
  }

  df
}

#------------------------------------------------------------------------------#
#' Generate year-type text for non-calendar years
#'
#' Returns a short text fragment describing the year type, optionally including
#' the year range. Note: year-type text is not needed for "Calender" year type.
#'
#' @param year_type Character. One of "November-November", "Academic", or
#' "Financial".
#' @param year_range Integer. The year range.
#' @param include_range Logical. Whether to include the year range.
#'
#' @return A character string describing the year type.
#' @noRd
get_year_type_text <- function(year_type, year_range, include_range = FALSE) {
  if (year_type %in% c("Academic", "Financial")) {
    year_type_lower <- str_to_lower(year_type)

    if (include_range) {
      year_type_text <- paste0(
        "the ", year_type_lower, " ", year_range, " year period ending "
      )
    } else {
      year_type_text <- paste0("the ", year_type_lower, " year ending ")
    }
  } else if (year_type %in% "November-November") {
    if (include_range) {
      year_type_text <- paste0("the ", year_range, " year period ending ")
    } else {
      year_type_text <- "the period ending "
    }
  } else {
    stop(
      paste0(
        "Unexpected YearType. Expected year types are: November-November,",
        " Academic, Financial"
      )
    )
  }

  year_type_text
}

#------------------------------------------------------------------------------#
#' Calculate trend between two time points
#'
#' Compares `start_year` and `end_year` data points to determine the direction
#' of change. Uses confidence intervals where available to distinguish
#' statistically significant change from nominal change.
#'
#' @param start_year dataframe row, earliest data point for an area/sex
#' @param end_year dataframe row, latest data point for an area/sex
#' @return character string, one of `"increased"`, `"decreased"`,
#'   `"changed"`, or `"no change"`
#' @noRd
calculate_trend <- function(start_year, end_year) {
  cis_available <- !is.na(start_year$LowerCI95.0limit) &
    !is.na(end_year$LowerCI95.0limit)

  if (cis_available) {
    if (end_year$LowerCI95.0limit > start_year$UpperCI95.0limit) {
      return("increased")
    } else if (end_year$UpperCI95.0limit < start_year$LowerCI95.0limit) {
      return("decreased")
    } else if (end_year$Value != start_year$Value) {
      return("changed")
    } else {
      return("no change")
    }
  }

  if (end_year$Value != start_year$Value) {
    return("changed")
  }

  "no change"
}

#------------------------------------------------------------------------------#
#' Generate accessible trend description text fragment
#'
#' Returns a text fragment describing the trend. Includes information about the
#' changes to the Value and the trend (using calculate_trend function and the
#' Timeperiod_text column).
#'
#' @param df dataframe, indicator data for selected area, must also include
#' "Timeperiod_text" column
#' @param area_name character string, area name
#' @param sex character string, value to filter the "Sex" column by. Optional
#'
#' @returns character string, text fragment describing the trend
#' @noRd
generate_trend_text <- function(df, area_name, sex = NULL) {
  # Filter df by sex
  if (!is.null(sex)) {
    df <- df |>
      filter(.data$Sex == sex)
  }

  # calculate trend
  # take first and last year to calculate trend
  start_year <- df |>
    slice_head()
  end_year <- df |>
    slice_tail()
  trend <- calculate_trend(start_year, end_year)

  unit_text <- str_to_lower(unique(df$ValueType))
  unit <- ifelse(
    unique(df$Unit) == "%",
    "%",
    paste0(" ", str_to_lower(unique(df$Unit)))
  )

  # create text output
  if (trend == "no change") {
    chart_text <- paste0(
      unit_text,
      " in ",
      format_area_name(area_name),
      " in ",
      end_year$Timeperiod_text,
      " was ",
      format(end_year$Value, big.mark = ","),
      unit,
      ", the same as it was in ",
      start_year$Timeperiod_text,
      "."
    )
  } else {
    chart_text <-
      paste0(
        unit_text,
        " in ",
        format_area_name(area_name),
        " ",
        trend,
        " from ",
        format(start_year$Value, big.mark = ","),
        unit,
        " in ",
        start_year$Timeperiod_text,
        " to ",
        format(end_year$Value, big.mark = ","),
        unit,
        " in ",
        end_year$Timeperiod_text,
        "."
      )
  }

  chart_text
}
