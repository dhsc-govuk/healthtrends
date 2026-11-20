# synthetic data ---------------------------------------------------------------
make_area_data <- function(
  sexes = c("Male", "Female"),
  years = c("2022", "2023"),
  values = c(10.1, 12.3, 11.4, 13.2),
  definition = "This indicator describes the rate of something in a person population.",
  value_type = "Rate",
  unit = "per 100,000",
  decimal_places = 1
) {
  expand_grid(
    Sex = sexes,
    Timeperiod = years
  ) |>
    mutate(
      Value = values[seq_len(n())],
      Decimal_places = decimal_places,
      Definition = definition,
      ValueType = value_type,
      Unit = unit
    ) |>
    select(
      "Definition",
      "Timeperiod",
      "Sex",
      "Value",
      "Decimal_places",
      "ValueType",
      "Unit"
    )
}

# calculate_trend --------------------------------------------------------------
test_that('returns "increased" when confidence intervals do not overlap and end is higher', {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  end_year <- tibble::tibble(
    Value = 20,
    LowerCI95.0limit = 13,
    UpperCI95.0limit = 27
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "increased"
  )
})

test_that('returns "decreased" when confidence intervals do not overlap and end is lower', {
  start_year <- tibble::tibble(
    Value = 20,
    LowerCI95.0limit = 18,
    UpperCI95.0limit = 22
  )

  end_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "decreased"
  )
})

test_that('returns "changed" when confidence intervals overlap but values differ', {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  end_year <- tibble::tibble(
    Value = 15,
    LowerCI95.0limit = 10,
    UpperCI95.0limit = 20
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "changed"
  )
})

test_that('returns "no change" when confidence intervals overlap and values are equal', {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  end_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 9,
    UpperCI95.0limit = 11
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "no change"
  )
})

test_that('returns "changed" when confidence intervals are unavailable and values differ', {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = NA_real_,
    UpperCI95.0limit = NA_real_
  )

  end_year <- tibble::tibble(
    Value = 15,
    LowerCI95.0limit = NA_real_,
    UpperCI95.0limit = NA_real_
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "changed"
  )
})

test_that('returns "no change" when confidence intervals are unavailable and values are equal', {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = NA_real_,
    UpperCI95.0limit = NA_real_
  )

  end_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = NA_real_,
    UpperCI95.0limit = NA_real_
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "no change"
  )
})

test_that("uses value comparison when only one row has confidence intervals", {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  end_year <- tibble::tibble(
    Value = 15,
    LowerCI95.0limit = NA_real_,
    UpperCI95.0limit = NA_real_
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "changed"
  )
})

test_that("touching confidence intervals are treated as overlapping", {
  start_year <- tibble::tibble(
    Value = 10,
    LowerCI95.0limit = 8,
    UpperCI95.0limit = 12
  )

  end_year <- tibble::tibble(
    Value = 15,
    LowerCI95.0limit = 12,
    UpperCI95.0limit = 18
  )

  expect_equal(
    calculate_trend(start_year, end_year),
    "changed"
  )
})

# generate_timeperiod_text -----------------------------------------------------
test_that("formats year ranges in YYYY/YY - YY/YY format", {
  local_mocked_bindings(
    get_year_type_text = function(year_type, year_range, include_range) {
      expect_true(include_range)
      "Financial year "
    }
  )

  df <- tibble::tibble(
    Timeperiod = "2020/21 - 22/23",
    YearType = "Financial",
    YearRange = 3
  )

  result <- generate_timeperiod_text(df)

  expect_equal(
    result$Timeperiod_text,
    "Financial year 2023"
  )
})

test_that("formats YYYY/YY time periods", {
  local_mocked_bindings(
    get_year_type_text = function(year_type, year_range, include_range) {
      expect_false(include_range)
      "Financial year "
    }
  )

  df <- tibble::tibble(
    Timeperiod = "2020/21",
    YearType = "Financial",
    YearRange = 1
  )

  result <- generate_timeperiod_text(df)

  expect_equal(
    result$Timeperiod_text,
    "Financial year 2021"
  )
})

test_that("formats YYYY - YY time periods", {
  df <- tibble::tibble(
    Timeperiod = "2020 - 22",
    YearType = "Calendar",
    YearRange = 3
  )

  result <- generate_timeperiod_text(df)

  expect_equal(
    result$Timeperiod_text,
    "2020 to 2022"
  )
})

test_that("returns original Timeperiod when no recognised format is present", {
  df <- tibble::tibble(
    Timeperiod = "Q1 2024",
    YearType = "Quarterly",
    YearRange = 1
  )

  result <- generate_timeperiod_text(df)

  expect_equal(
    result$Timeperiod_text,
    "Q1 2024"
  )
})

test_that("adds Timeperiod_text column and preserves existing columns", {
  local_mocked_bindings(
    get_year_type_text = function(year_type, year_range, include_range) {
      "Academic year "
    }
  )

  df <- tibble::tibble(
    AreaCode = "E06000001",
    Timeperiod = "2020/21",
    YearType = "Academic",
    YearRange = 1
  )

  result <- generate_timeperiod_text(df)

  expect_named(
    result,
    c("AreaCode", "Timeperiod", "YearType", "YearRange", "Timeperiod_text")
  )

  expect_equal(result$AreaCode, "E06000001")
})

test_that("uses unique YearType and YearRange values when calling get_year_type_text", {
  local_mocked_bindings(
    get_year_type_text = function(year_type, year_range, include_range) {
      expect_equal(year_type, "Academic")
      expect_equal(year_range, 3)
      "Academic year "
    }
  )

  df <- tibble::tibble(
    Timeperiod = "2020/21 - 2022/23",
    YearType = "Academic",
    YearRange = 3
  )

  expect_no_error(generate_timeperiod_text(df))
})

test_that("works for multiple rows", {
  local_mocked_bindings(
    get_year_type_text = function(year_type, year_range, include_range) {
      "Academic year "
    }
  )

  df <- tibble::tibble(
    Timeperiod = c("2020/21", "2021/22"),
    YearType = "Academic",
    YearRange = 1
  )

  result <- generate_timeperiod_text(df)

  expect_equal(
    result$Timeperiod_text,
    c("Academic year 2021", "Academic year 2022")
  )
})

# get_year_type_text -----------------------------------------------------------

test_that("returns academic year text without range", {
  expect_equal(
    get_year_type_text(
      year_type = "Academic",
      year_range = 3
    ),
    "the academic year ending "
  )
})

test_that("returns academic year text with range", {
  expect_equal(
    get_year_type_text(
      year_type = "Academic",
      year_range = 3,
      include_range = TRUE
    ),
    "the academic 3 year period ending "
  )
})

test_that("returns financial year text without range", {
  expect_equal(
    get_year_type_text(
      year_type = "Financial",
      year_range = 5
    ),
    "the financial year ending "
  )
})

test_that("returns financial year text with range", {
  expect_equal(
    get_year_type_text(
      year_type = "Financial",
      year_range = 5,
      include_range = TRUE
    ),
    "the financial 5 year period ending "
  )
})

test_that("returns November-November text without range", {
  expect_equal(
    get_year_type_text(
      year_type = "November-November",
      year_range = 3
    ),
    "the period ending "
  )
})

test_that("returns November-November text with range", {
  expect_equal(
    get_year_type_text(
      year_type = "November-November",
      year_range = 3,
      include_range = TRUE
    ),
    "the 3 year period ending "
  )
})

test_that("uses supplied year range in output", {
  expect_equal(
    get_year_type_text(
      year_type = "Academic",
      year_range = 10,
      include_range = TRUE
    ),
    "the academic 10 year period ending "
  )
})

test_that("throws an error for unsupported year types", {
  expect_error(
    get_year_type_text(
      year_type = "Calendar",
      year_range = 1
    ),
    "Unexpected YearType"
  )
})

test_that("throws an error for invalid year type values", {
  expect_error(
    get_year_type_text(
      year_type = "Quarterly",
      year_range = 1
    ),
    "Unexpected YearType"
  )
})

test_that("default include_range is FALSE", {
  expect_equal(
    get_year_type_text(
      year_type = "Academic",
      year_range = 3
    ),
    get_year_type_text(
      year_type = "Academic",
      year_range = 3,
      include_range = FALSE
    )
  )
})


# generate_trend_text ----------------------------------------------------------
test_that("generate_trend_text uses supplied area in output, works with no sex input provided", {
  local_mocked_bindings(
    calculate_trend = function(start_year, end_year) {
      "increased"
    }
  )

  df <- tibble::tibble(
    Sex = c("Female", "Female", "Female"),
    Value = c(14.1, 15.9, 16.4),
    Timeperiod_text = c(2024, 2025, 2026),
    ValueType = "Proportion",
    Unit = "%"
  )

  expect_equal(
    generate_trend_text(
      df = df,
      area_name = "South West",
      sex = NULL
    ),
    "proportion in the South West increased from 14.1% in 2024 to 16.4% in 2026."
  )
})

test_that("generate_trend_text filters by sex when sex is provided", {
  local_mocked_bindings(
    calculate_trend = function(start_year, end_year) {
      "increased"
    }
  )

  df <- tibble::tibble(
    Sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
    Value = c(14.1, 15.9, 16.4, 13.1, 14.7, 15.5),
    Timeperiod_text = c(2024, 2025, 2026, 2024, 2025, 2026),
    ValueType = "Proportion",
    Unit = "%"
  )

  expect_equal(
    generate_trend_text(
      df = df,
      area_name = "South West",
      sex = "Male"
    ),
    "proportion in the South West increased from 13.1% in 2024 to 15.5% in 2026."
  )
})

test_that("generate_trend_text returns correct text when trend is no change", {
  local_mocked_bindings(
    calculate_trend = function(start_year, end_year) {
      "no change"
    }
  )

  df <- tibble::tibble(
    Sex = c("Female", "Female", "Female"),
    Value = c(14.1, 14.1, 14.1),
    Timeperiod_text = c(2024, 2025, 2026),
    ValueType = "Proportion",
    Unit = "%"
  )

  expect_equal(
    generate_trend_text(
      df = df,
      area_name = "South West",
      sex = NULL
    ),
    "proportion in the South West in 2026 was 14.1%, the same as it was in 2024."
  )
})
