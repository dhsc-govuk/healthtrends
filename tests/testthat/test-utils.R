# ---------------------------------------------------------------------------- #
# UTILS  ----
# ---------------------------------------------------------------------------- #

# Tests for clean_html function
test_that("Cleaning and converting html functionality", {
  expect_equal(clean_html("Some <b>Text</b>"), "Some Text")
  expect_equal(clean_html("2 to 2&frac12; years"), "2 to 2½ years")
  expect_equal(clean_html("5&sup2; is 5 squared"), "5² is 5 squared")
  expect_equal(clean_html("This &nbsp; is a space"), "This   is a space")
  expect_equal(clean_html("Remove\r\n from text"), "Remove from text")
})

test_that("Strings without html remain unchanged", {
  expect_equal(clean_html(""), "")
  expect_equal(clean_html("Plain text"), "Plain text")
  expect_equal(clean_html("< less than"), "< less than")
})

test_that("Strings without html remain unchaged", {
  expect_error(clean_html(c("a", "b")))
  expect_error(clean_html(12345))
})


## lowercase_except_acronyms  --------------------------------------------------

test_that(
  "Test that lowercase_except_acronyms works as intended",
  {
    expect_equal(
      suppressWarnings(lowercase_except_acronyms("BoYs Homelessness Reduction Act")),
      "boys Homelessness Reduction Act"
    )
    expect_equal(
      suppressWarnings(lowercase_except_acronyms("BoYs Reduction Act")),
      "boys reduction act"
    )
    expect_equal(
      suppressWarnings(lowercase_except_acronyms("Xsdg AFsG liGhTs")),
      "xsdg AFsG lights"
    )
    expect_equal(
      suppressWarnings(lowercase_except_acronyms("4NHS NHS NHS Sa7ve LiVeS")),
      "4NHS NHS NHS sa7ve lives"
    )
    expect_equal(
      capture_warnings(lowercase_except_acronyms("LIVE Live")),
      c(
        'For the text "LIVE Live" the acronym "LIVE" remains capitalised',
        'For the text "LIVE Live" the word "Live" has been decapitalised mid-sentence'
      )
    )
  }
)


## multiple_tile ---------------------------------------------------------------

data <- tibble(
  IndicatorID = sample(c(253, 20401, 90362), size = 100, replace = TRUE),
  Sex = sample(c("Male", "Female", "Persons"),
               size = 100,
               replace = TRUE,
               prob = c(0.4, 0.4, 0.2)
  )
)

test_that(
  "Test that ind_n_sex returns the correct numer",
  {
    expect_equal(ind_n_sex(ind = 253, all_data = data), 3)
    expect_equal(ind_n_sex(ind = 20401, all_data = data), 3)
    expect_equal(ind_n_sex(ind = 90362, all_data = data), 3)
    expect_equal(ind_n_sex(ind = 999, all_data = data), 0)
  }
)


## data_table ------------------------------------------------------------------

### Synthesis data ----
sex_data <- tibble(
  AreaCode = c(
    "E12000008", "E12000008",
    "E12000008", "E12000008",
    "E92000001", "E92000001",
    "E92000001", "E92000001"
  ),
  AreaName = c(
    "South East", "South East",
    "South East", "South East",
    "England", "England",
    "England", "England"
  ),
  Timeperiod = c(
    2022, 2022,
    2023, 2023,
    2022, 2022,
    2023, 2023
  ),
  TimeperiodSortable = c(
    20220000, 20220000, 20230000, 20230000,
    20220000, 20220000, 20230000, 20230000
  ),
  Sex = c(
    "Male", "Female",
    "Male", "Female",
    "Male", "Female",
    "Male", "Female"
  ),
  Value = c(
    1234.56, 2345.67,
    3456.78, 4567.89,
    1111.11, 2222.22,
    3333.33, 4444.44
  ),
  LowerCI95.0limit = c(
    1200.00, 2300.00,
    3400.00, 4500.00,
    1100.00, 2200.00,
    3300.00, 4400.00
  ),
  UpperCI95.0limit = c(
    1300.00, 2400.00,
    3500.00, 4600.00,
    1200.00, 2300.00,
    3400.00, 4500.00
  ),
  Decimal_places = c(2, 2, 2, 2, 2, 2, 2, 2)
)

persons_data <- tibble(
  AreaCode = c(
    "E12000008", "E12000008",
    "E92000001", "E92000001"
  ),
  AreaName = c(
    "South East", "South East",
    "England", "England"
  ),
  Timeperiod = c(2022, 2023, 2022, 2023),
  TimeperiodSortable = c(20220000, 20230000, 20220000, 20230000),
  Sex = c("Persons", "Persons", "Persons", "Persons"),
  Value = c(1234.56, 9876.54, 1111.11, 2222.22),
  LowerCI95.0limit = c(1200.12, 9800.12, 1100.11, 2200.22),
  UpperCI95.0limit = c(1300.99, 9900.99, 1200.22, 2300.33),
  Decimal_places = c(2, 2, 2, 2)
)

england_only_sex_data <- tibble(
  AreaCode = c("E92000001", "E92000001"),
  AreaName = c("England", "England"),
  Timeperiod = c(2022, 2022),
  TimeperiodSortable = c(20220000, 20220000),
  Sex = c("Male", "Female"),
  Value = c(1000.12, 2000.34),
  LowerCI95.0limit = c(950.00, 1950.00),
  UpperCI95.0limit = c(1050.00, 2050.00),
  Decimal_places = c(2, 2)
)

england_only_persons_data <- tibble(
  AreaCode = c("E92000001", "E92000001"),
  AreaName = c("England", "England"),
  Timeperiod = c(2022, 2023),
  TimeperiodSortable = c(20220000, 20230000),
  Sex = c("Persons", "Persons"),
  Value = c(1000.1, 2000.2),
  LowerCI95.0limit = c(900.1, 1900.2),
  UpperCI95.0limit = c(1100.1, 2100.2),
  Decimal_places = c(1, 1)
)

### Tests: n_sex == 2 ----
test_that("data_table returns sex-specific wide table for non-England area", {
  result <- data_table(
    ind_data = sex_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 2
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  expect_named(
    result,
    c(
      "Time period",
      "South East Value, Male",
      "South East Value, Female",
      "England Value, Male",
      "England Value, Female"
    )
  )

  expect_equal(result$`Time period`, c("2022", "2023"))
  expect_equal(result$`South East Value, Male`, c("1,234.56", "3,456.78"))
  expect_equal(result$`South East Value, Female`, c("2,345.67", "4,567.89"))
  expect_equal(result$`England Value, Male`, c("1,111.11", "3,333.33"))
  expect_equal(result$`England Value, Female`, c("2,222.22", "4,444.44"))
})

test_that("data_table does not append England columns when area is England and n_sex == 2", {
  result <- data_table(
    ind_data = england_only_sex_data,
    area_code = "E92000001",
    area_name = "England",
    n_sex = 2
  )

  expect_named(
    result,
    c(
      "Time period",
      "England Value, Male",
      "England Value, Female"
    )
  )

  expect_equal(result$`Time period`, "2022")
  expect_equal(result$`England Value, Male`, "1,000.12")
  expect_equal(result$`England Value, Female`, "2,000.34")
})

test_that("data_table formats values according to Decimal_places for n_sex == 2", {
  result <- data_table(
    ind_data = sex_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 2
  )

  expect_match(result$`South East Value, Male`[1], "^1,234\\.56$")
  expect_match(result$`England Value, Female`[2], "^4,444\\.44$")
})

### Tests: n_sex != 2 ----
test_that("data_table returns CI table for non-England area when n_sex != 2", {
  result <- data_table(
    ind_data = persons_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 1
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  expect_named(
    result,
    c(
      "Time period",
      "South East Value",
      "95% confidence interval lower limit",
      "95% confidence interval upper limit",
      "England Value"
    )
  )

  expect_equal(result$`Time period`, c("2022", "2023"))
  expect_equal(result$`South East Value`, c("1,234.56", "9,876.54"))
  expect_equal(
    result$`95% confidence interval lower limit`,
    c("1,200.12", "9,800.12")
  )
  expect_equal(
    result$`95% confidence interval upper limit`,
    c("1,300.99", "9,900.99")
  )
  expect_equal(result$`England Value`, c("1,111.11", "2,222.22"))
})

test_that("data_table does not append England Value when area is England and n_sex != 2", {
  result <- data_table(
    ind_data = england_only_persons_data,
    area_code = "E92000001",
    area_name = "England",
    n_sex = 1
  )

  expect_named(
    result,
    c(
      "Time period",
      "England Value",
      "95% confidence interval lower limit",
      "95% confidence interval upper limit"
    )
  )

  expect_equal(result$`Time period`, c("2022", "2023"))
  expect_equal(result$`England Value`, c("1,000.1", "2,000.2"))
  expect_equal(
    result$`95% confidence interval lower limit`,
    c("900.1", "1,900.2")
  )
  expect_equal(
    result$`95% confidence interval upper limit`,
    c("1,100.1", "2,100.2")
  )
})

test_that("data_table renames Timeperiod to Time period", {
  result1 <- data_table(
    ind_data = sex_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 2
  )

  result2 <- data_table(
    ind_data = persons_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 1
  )

  expect_true("Time period" %in% names(result1))
  expect_true("Time period" %in% names(result2))
  expect_false("Timeperiod" %in% names(result1))
  expect_false("Timeperiod" %in% names(result2))
})

test_that("data_table returns character Time period column", {
  result <- data_table(
    ind_data = persons_data,
    area_code = "E12000008",
    area_name = "South East",
    n_sex = 1
  )

  expect_type(result$`Time period`, "character")
})


## format_area_name ------------------------------------------------------------
test_that("tests for format_area_name", {
  expect_equal(
    format_area_name("Kent"),
    "Kent"
  )
  expect_equal(
    format_area_name("England"),
    "England"
  )
  expect_equal(
    format_area_name("england"),
    "england"
  )
  expect_equal(
    format_area_name("ENGLAND"),
    "ENGLAND"
  )
  expect_equal(
    format_area_name("West Yorkshire"),
    "West Yorkshire"
  )
  expect_equal(
    format_area_name("north yorkshire"),
    "north yorkshire"
  )
  expect_equal(
    format_area_name("Greater London"),
    "Greater London"
  )
  expect_equal(
    format_area_name("inner london"),
    "inner london"
  )
  expect_length(format_area_name("Kent"), 1)
})

# format_age_labels ------------------------------------------------------------
test_that('replaces "yrs" with "years"', {
  expect_equal(
    format_age_labels("5-9 yrs"),
    "5 to 9 years"
  )
})

test_that('replaces "yr" with "year"', {
  expect_equal(
    format_age_labels("1 yr"),
    "1 year"
  )
})

test_that('replaces hyphens with " to "', {
  expect_equal(
    format_age_labels("10-14"),
    "10 to 14"
  )
})

test_that('replaces leading less-than with "Under "', {
  expect_equal(
    format_age_labels("<1 yr"),
    "Under 1 year"
  )
})

test_that('replaces embedded less-than with "under "', {
  expect_equal(
    format_age_labels("Age <18 yrs"),
    "Age under 18 years"
  )
})

test_that('replaces plus years with " years and over"', {
  expect_equal(
    format_age_labels("65+ years"),
    "65 years and over"
  )
})

test_that('replaces leading greater-than-or-equal-to with "At least "', {
  expect_equal(
    format_age_labels(">=18 yrs"),
    "At least 18 years"
  )
})

test_that('replaces embedded greater-than-or-equal-to with "at least "', {
  expect_equal(
    format_age_labels("Age >=18 yrs"),
    "Age at least 18 years"
  )
})

test_that("replaces decimal half with one-half symbol", {
  expect_equal(
    format_age_labels("18.5 years"),
    "18½ years"
  )
})

test_that("applies multiple substitutions in a single label", {
  expect_equal(
    format_age_labels("<1-4 yrs"),
    "Under 1 to 4 years"
  )
})

test_that("returns unchanged values when no substitutions are required", {
  expect_equal(
    format_age_labels("All ages"),
    "All ages"
  )
})

test_that("works on vectors", {
  expect_equal(
    format_age_labels(c("<1 yr", "65+ years")),
    c("Under 1 year", "65 years and over")
  )
})

test_that("preserves NA values", {
  expect_true(is.na(format_age_labels(NA_character_)))
})

# format_definition ------------------------------------------------------------
test_that("adds a full stop when one is missing", {
  expect_equal(
    format_definition("Deaths from cardiovascular disease"),
    "The line chart shows the deaths from cardiovascular disease."
  )
})

test_that('does not prepend "the" when definition starts with "the"', {
  expect_equal(
    format_definition("The proportion of adults who smoke"),
    "The line chart shows the proportion of adults who smoke."
  )
})

test_that("detects all case-insensitively for leading all", {
  expect_equal(
    format_definition("ALL deaths registered in England"),
    "The line chart shows aLL deaths registered in England."
  )
})

test_that('adds "the" when definition does not start with "the" or "all"', {
  expect_equal(
    format_definition("Deaths from cardiovascular disease"),
    "The line chart shows the deaths from cardiovascular disease."
  )
})

test_that("lowercases the first letter of the definition", {
  expect_equal(
    format_definition("Deaths from cardiovascular disease"),
    "The line chart shows the deaths from cardiovascular disease."
  )
})

test_that("preserves the remainder of the definition", {
  expect_equal(
    format_definition("Mortality rate per 100,000 population"),
    "The line chart shows the mortality rate per 100,000 population."
  )
})

test_that("handles definitions beginning with lowercase letters", {
  expect_equal(
    format_definition("deaths from cardiovascular disease"),
    "The line chart shows the deaths from cardiovascular disease."
  )
})

test_that("handles multiple formatting operations together", {
  expect_equal(
    format_definition("The mortality rate "),
    "The line chart shows the mortality rate."
  )
})


# get_decimal_places -----------------------------------------------------------
test_that("returns 2 decimal places when minimum absolute value is less than 1", {
  expect_equal(
    get_decimal_places(c(10, 0.45, 100)),
    2
  )
})

test_that("returns 1 decimal place when minimum absolute value is between 1 and 100", {
  expect_equal(
    get_decimal_places(c(10, 50, 250)),
    1
  )
})

test_that("returns 0 decimal places when all absolute values are at least 100", {
  expect_equal(
    get_decimal_places(c(100, 250, 1000)), 0
  )
})
# import_healthtrends_files-----------------------------------------------------
test_that("overwrite must be a single logical value", {
  expect_error(
    import_healthtrends_files("TRUE"),
    "`overwrite` must be a single TRUE or FALSE value."
  )
  expect_error(
    import_healthtrends_files(c(TRUE, FALSE)),
    "`overwrite` must be a single TRUE or FALSE value."
  )
})

test_that("errors when overwrite is FALSE and healthtrendsfiles does not exist", {
  tmp <- withr::local_tempdir()

  old_wd <- setwd(tmp)
  withr::defer(setwd(old_wd))

  expect_error(
    import_healthtrends_files(overwrite = FALSE),
    "healthtrendsfiles folder does not exist"
  )
})

test_that("creates expected folder structure", {
  tmp <- withr::local_tempdir()

  old_wd <- setwd(tmp)
  withr::defer(setwd(old_wd))

  expect_message(
    import_healthtrends_files(),
    "successfully populated"
  )

  expect_true(fs::dir_exists("healthtrendsfiles"))
  expect_true(fs::dir_exists("output"))
  expect_true(fs::dir_exists("healthtrendsfiles/data"))
  expect_true(fs::dir_exists("healthtrendsfiles/Images"))
})
## get_indicator_csv------------------------------------------------------------
## create temp csv of indicator list
csv_file <- tempfile(fileext = ".csv")

readr::write_csv(
  tibble::tibble(
    IndicatorID = c(1, 2, 3, 4),
    TopicName = c("A", "A", "B", "C"),
    IndicatorOrder = c(1, 2, 1, 1),
    TopicOrder = c(1, 1, 2, 3),
    ProfileId = c(19, 19, 21, 9)
  ),
  csv_file
)

test_that("get_indicator_csv returns dataframe when as_df = TRUE", {
  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  result <- get_indicator_csv()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_named(result, c(
    "IndicatorID", "TopicName", "IndicatorOrder",
    "TopicOrder", "ProfileId"
  ))
})

test_that("get_indicator_csv returns IndicatorID vector when as_df = FALSE", {
  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  result <- get_indicator_csv(as_df = FALSE)

  expect_equal(result, c(1, 2, 3, 4))
})

test_that("get_indicator_csv validates as_df", {
  expect_error(
    get_indicator_csv(as_df = "TRUE"),
    "`as_df`"
  )

  expect_error(
    get_indicator_csv(as_df = c(TRUE, FALSE)),
    "`as_df`"
  )

  expect_error(
    get_indicator_csv(as_df = NA),
    "`as_df`"
  )

  expect_error(
    get_indicator_csv(as_df = NULL),
    "`as_df`"
  )
})

test_that("get_indicator_csv errors when file does not exist", {
  testthat::local_mocked_bindings(
    get = function(...) "missing_file.csv",
    .package = "config"
  )

  expect_error(
    get_indicator_csv(),
    "does not exist"
  )
})

test_that("get_indicator_csv warns when csv contains no rows", {
  csv_file <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      IndicatorID = numeric(),
      TopicName = character(),
      IndicatorOrder = numeric(),
      TopicOrder = numeric(),
      ProfileId = numeric()
    ),
    csv_file
  )

  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  expect_warning(
    result <- get_indicator_csv(),
    "contains no rows"
  )

  expect_equal(nrow(result), 0)
})

test_that("get_indicator_csv errors when IndicatorID column is missing", {
  csv_file <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      TopicName = c("A", "A", "B", "C"),
      IndicatorOrder = c(1, 2, 1, 1),
      TopicOrder = c(1, 1, 2, 3),
      ProfileId = c(19, 19, 21, 9)
    ),
    csv_file
  )

  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  expect_error(
    get_indicator_csv(),
    "IndicatorID"
  )
})

test_that("get_indicator_csv errors when TopicName column is missing", {
  csv_file <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      IndicatorID = c(1, 2, 3, 4),
      IndicatorOrder = c(1, 2, 1, 1),
      TopicOrder = c(1, 1, 2, 3),
      ProfileId = c(19, 19, 21, 9)
    ),
    csv_file
  )

  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  expect_error(
    get_indicator_csv(),
    "TopicName"
  )
})

test_that("IndicatorID values are unique", {
  csv_file <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      IndicatorID = c(1, 1, 2, 3),
      TopicName = c("A", "A", "B", "C"),
      IndicatorOrder = c(1, 2, 1, 1),
      TopicOrder = c(1, 1, 2, 3),
      ProfileId = c(19, 19, 21, 9)
    ),
    csv_file
  )

  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  expect_error(
    get_indicator_csv(),
    "Duplicate"
  )
})

test_that("get_indicator_csv lists the duplicated IDs in the error", {
  csv_file <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(IndicatorID = c(1, 1, 2, 3, 3), TopicName = "A"),
    csv_file
  )

  testthat::local_mocked_bindings(
    get = function(...) csv_file,
    .package = "config"
  )

  expect_error(
    get_indicator_csv(),
    "duplicated IDs: 1, 3"
  )
})

## get_indicators---------------------------------------------------------------

### Synthesis data ----
# Indicator CSV. IndicatorOrder is deliberately reversed for topic A so the
# tests check that results are sorted, not just returned in input order.
indicator_df <- tibble::tibble(
  IndicatorID = c(1, 2, 3, 4),
  TopicName = c("A", "A", "B", "C"),
  IndicatorOrder = c(2, 1, 1, 1),
  TopicOrder = c(1, 1, 2, 3),
  ProfileId = c(19, 19, 21, 9)
)

# Data file. Some indicator/area pairs appear more than once (as they would
# with several time periods) to check de-duplication.
#   England (E92):        indicators 1, 2, 3, 4
#   Regions (E12):        indicators 1, 2, 3 (in two regions)
#   UTLAs (E06 and E09):  indicator 2 in E06000001, indicator 4 in E09000033
indicators_data_file <- tempfile(fileext = ".rds")

readr::write_rds(
  tibble::tibble(
    IndicatorID = c(
      1, 1, 2, 3, 4,
      1, 2, 3, 1, 2, 3,
      2, 2, 4
    ),
    AreaCode = c(
      rep("E92000001", 5),
      rep("E12000001", 3), rep("E12000002", 3),
      "E06000001", "E06000001", "E09000033"
    )
  ),
  indicators_data_file
)

# England-only data file, for area types with no indicators
england_only_data_file <- tempfile(fileext = ".rds")

readr::write_rds(
  tibble::tibble(
    IndicatorID = c(1, 2),
    AreaCode = c("E92000001", "E92000001")
  ),
  england_only_data_file
)


### Tests: indicator data frame ----
test_that("get_indicators returns England indicators in topic then indicator order", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  result <- get_indicators(
    data_file = indicators_data_file,
    area_code = "E92000001"
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("IndicatorID", "TopicName"))
  expect_equal(result$IndicatorID, c(2, 1, 3, 4))
  expect_equal(result$TopicName, c("A", "A", "B", "C"))
})

test_that("get_indicators defaults to England", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  expect_equal(
    get_indicators(data_file = indicators_data_file),
    get_indicators(data_file = indicators_data_file, area_code = "E92000001")
  )
})

test_that("get_indicators returns region indicators with no duplicates", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  result <- get_indicators(
    data_file = indicators_data_file,
    area_code = "E12000001"
  )

  expect_equal(result$IndicatorID, c(2, 1, 3))
  expect_equal(result$TopicName, c("A", "A", "B"))
})

test_that("get_indicators returns UTLA indicators across all UTLA prefixes", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  result <- get_indicators(
    data_file = indicators_data_file,
    area_code = "E06000001"
  )

  expect_equal(result$IndicatorID, c(2, 4))
  expect_equal(result$TopicName, c("A", "C"))
})

test_that("get_indicators works at area type level, not area level", {
  # Current behaviour: any UTLA code returns the indicators available for
  # any UTLA in the data file, including codes not in the data at all.
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  e06 <- get_indicators(data_file = indicators_data_file, area_code = "E06000001")
  e09 <- get_indicators(data_file = indicators_data_file, area_code = "E09000033")
  e08 <- get_indicators(data_file = indicators_data_file, area_code = "E08000025")

  expect_equal(e09, e06)
  expect_equal(e08, e06)
})

test_that("get_indicators returns an empty data frame when the area type has no data", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  result <- get_indicators(
    data_file = england_only_data_file,
    area_code = "E12000001"
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("IndicatorID", "TopicName"))
  expect_equal(nrow(result), 0)
})


### Tests: topic_names = TRUE ----
test_that("get_indicators returns unique topic names in topic order", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  expect_equal(
    get_indicators(
      data_file = indicators_data_file,
      area_code = "E92000001",
      topic_names = TRUE
    ),
    c("A", "B", "C")
  )
  expect_equal(
    get_indicators(
      data_file = indicators_data_file,
      area_code = "E12000001",
      topic_names = TRUE
    ),
    c("A", "B")
  )
  expect_equal(
    get_indicators(
      data_file = indicators_data_file,
      area_code = "E06000001",
      topic_names = TRUE
    ),
    c("A", "C")
  )
})

test_that("get_indicators returns an empty vector when the area type has no topics", {
  testthat::local_mocked_bindings(get_indicator_csv = function(...) indicator_df)

  result <- get_indicators(
    data_file = england_only_data_file,
    area_code = "E12000001",
    topic_names = TRUE
  )

  expect_type(result, "character")
  expect_length(result, 0)
})


### Tests: argument validation ----
test_that("get_indicators errors when data_file is not a single string", {
  expect_error(
    get_indicators(data_file = 1),
    "`data_file` must be a single character string"
  )
  expect_error(
    get_indicators(data_file = c("a.rds", "b.rds")),
    "`data_file` must be a single character string"
  )
  expect_error(
    get_indicators(data_file = NA_character_),
    "`data_file` must be a single character string"
  )
})

test_that("get_indicators errors when data_file has no file extension", {
  expect_error(
    get_indicators(data_file = "health_trends_data"),
    "file extension"
  )
})

test_that("get_indicators errors when data_file does not exist", {
  expect_error(
    get_indicators(data_file = file.path(tempdir(), "missing.rds")),
    "File does not exist"
  )
})

test_that("get_indicators errors when topic_names is not a single TRUE or FALSE", {
  expect_error(
    get_indicators(data_file = indicators_data_file, topic_names = "TRUE"),
    "`topic_names` must be TRUE or FALSE"
  )
  expect_error(
    get_indicators(data_file = indicators_data_file, topic_names = NA),
    "`topic_names` must be TRUE or FALSE"
  )
  expect_error(
    get_indicators(data_file = indicators_data_file, topic_names = c(TRUE, FALSE)),
    "`topic_names` must be TRUE or FALSE"
  )
})

## set_config-------------------------------------------------------------------
test_that(
  "error if config file doesn't exist",
  {
    expect_error(
      set_config(config_file = "not_a_config.yaml")
    )
  }
)

test_that(
  "configuration set correctly",
  {
    expect_invisible(
      set_config(
        config_nm = "config",
        config_file = test_path("conf", "test-config.yaml")
      )
    )

    expect_identical(
      Sys.getenv("R_CONFIG_FILE"),
      test_path("conf", "test-config.yaml")
    )

    expect_identical(
      Sys.getenv("R_CONFIG_ACTIVE"),
      "config"
    )
  }
)


## get_valid_area_codes -------------------------------------------------------
test_that("get_valid_area_codes returns the Code column from the API", {
  testthat::local_mocked_bindings(
    get_fingertips_api = function(url) {
      tibble(Code = c("E08000016", "E08000019", "E08000025"))
    },
    .package = "fingertipsR"
  )

  result <- get_valid_area_codes(
    502
  )

  expect_type(result, "character")
  expect_setequal(result, c("E08000016", "E08000019", "E08000025"))
})

test_that("get_valid_area_codes combines codes across multiple area types", {
  testthat::local_mocked_bindings(
    get_fingertips_api = function(url) {
      if (grepl("area_type_id=6", url, fixed = TRUE)) {
        tibble(Code = c("E12000001", "E12000002"))
      } else {
        tibble(Code = c("E08000016", "E08000019"))
      }
    },
    .package = "fingertipsR"
  )

  result <- get_valid_area_codes(
    c(6, 502)
  )

  expect_setequal(
    result,
    c("E12000001", "E12000002", "E08000016", "E08000019")
  )
})

test_that("get_valid_area_codes queries each area type id once", {
  called_urls <- character(0)
  testthat::local_mocked_bindings(
    get_fingertips_api = function(url) {
      called_urls <<- c(called_urls, url)
      tibble(Code = character(0))
    },
    .package = "fingertipsR"
  )

  get_valid_area_codes(
    c(502, 502, 6)
  )

  # de-duplicated to one call each for 502 and 6
  expect_length(called_urls, 2)
  expect_true(any(grepl("area_type_id=502", called_urls, fixed = TRUE)))
  expect_true(any(grepl("area_type_id=6", called_urls, fixed = TRUE)))
})


## correct_superseded_codes ---------------------------------------------------
test_that("correct_superseded_codes swaps new Barnsley/Sheffield codes for old", {
  expect_warning(
    result <- correct_superseded_codes(c("E08000038", "E08000039")),
    "Corrected 2 area code"
  )
  expect_equal(result, c("E08000016", "E08000019"))
})

test_that("correct_superseded_codes only swaps superseded codes, leaving others", {
  expect_warning(
    result <- correct_superseded_codes(c("E08000038", "E12000003", "E92000001")),
    "Barnsley"
  )
  expect_equal(result, c("E08000016", "E12000003", "E92000001"))
})

test_that("correct_superseded_codes preserves the position of each code", {
  expect_warning(
    result <- correct_superseded_codes(c("E12000003", "E08000039")),
    "Sheffield"
  )
  expect_equal(result, c("E12000003", "E08000019"))
})

test_that("correct_superseded_codes handles a single superseded code", {
  expect_warning(
    result <- correct_superseded_codes("E08000038"),
    "Corrected 1 area code"
  )
  expect_equal(result, "E08000016")
})

test_that("correct_superseded_codes is a no-op when no superseded codes present", {
  expect_no_warning(
    result <- correct_superseded_codes(c("E08000016", "E12000003"))
  )
  expect_equal(result, c("E08000016", "E12000003"))
})


# get_area_indicators ----------------------------------------------------------
all_data <- tibble::tribble(
  ~IndicatorID, ~AreaCode,    ~Sex,
  100001,       "E12000006",  "Persons",
  100001,       "E12000007",  "Persons",
  100002,       "E12000006",  "Persons",
  100003,       "E12000006",  "Male",
  100003,       "E12000006",  "Female",
  100004,       "E12000007",  "Persons",
  100005,       "E12000006",  "Persons"
)

indicator_df <- tibble::tribble(
  ~IndicatorID, ~TopicName, ~IndicatorOrder, ~TopicOrder,
  100001,       "Topic 1",  1,               1,
  100002,       "Topic 1",  2,               1,
  100003,       "Topic 2",  3,               2,
  100004,       "Topic 3",  4,               3,
  100005,       "Topic 3",  5,               3,
  100006,       "Topic 4",  6,               4
)

run_get_area_indicators <- function(area_code) {
  testthat::local_mocked_bindings(
    get_indicator_csv = function(as_df) indicator_df
  )

  get_area_indicators(
    all_data = all_data,
    area_code = area_code
  )
}

area_indicators <- run_get_area_indicators(
  area_code = "E12000006"
)

test_that("get_area_indicators returns a data frame", {
  expect_s3_class(area_indicators, "data.frame")
  expect_gt(nrow(area_indicators), 0)
})

test_that("get_area_indicators returns expected columns", {
  expected_cols <- c("IndicatorID", "TopicName")

  expect_named(area_indicators, expected_cols)
})

test_that("get_area_indicators selects indicators for the requested area", {
  expect_equal(
    area_indicators$IndicatorID,
    c(100001, 100002, 100003, 100005)
  )
})


# get_spotlight_df -------------------------------------------------------------
run_get_spotlight_df <- function(area_code, mock_area_indicators) {
  testthat::local_mocked_bindings(
    get_area_indicators = function(all_data, area_code) mock_area_indicators
  )

  get_spotlight_df(
    all_data = all_data,
    area_code = area_code,
    spotlight = "first"
  )
}

spotlight_df <- run_get_spotlight_df(
  area_code = "E12000006",
  mock_area_indicators = area_indicators
)

test_that("get_spotlight_df returns a data frame", {
  expect_s3_class(spotlight_df, "data.frame")
  expect_gt(nrow(spotlight_df), 0)
})

test_that("get_spotlight_df returns expected columns", {
  expected_cols <- c("IndicatorID", "TopicName", "Sex", "multiple_tile")

  expect_named(spotlight_df, expected_cols)
})

test_that("get_spotlight_df selects the correct indicators", {
  expect_equal(
    unique(spotlight_df$IndicatorID),
    c(100001, 100003, 100005)
  )
})

test_that("get_spotlight_df identifies multiple tiles", {
  expect_equal(
    spotlight_df$multiple_tile,
    c(FALSE, TRUE, TRUE, FALSE)
  )
})

test_that("get_spotlight_df selects one indicator per topic", {
  expect_equal(
    n_distinct(spotlight_df$IndicatorID),
    n_distinct(spotlight_df$TopicName)
  )
})


# get_data_path ----------------------------------------------------------------
test_that("get_data_path builds the path from output_path in config", {
  testthat::local_mocked_bindings(
    get = function(value, ...) {
      expect_equal(value, "output_path")
      "output/2026-11-26"
    },
    .package = "config"
  )

  expect_equal(
    get_data_path("my_data.rds"),
    file.path("output/2026-11-26", "my_data.rds")
  )
})

test_that("get_data_path defaults to health_trends_data.rds", {
  testthat::local_mocked_bindings(
    get = function(...) "output",
    .package = "config"
  )

  expect_equal(
    get_data_path(),
    file.path("output", "health_trends_data.rds")
  )
})

test_that("get_data_path no longer points at healthtrendsfiles/data", {
  testthat::local_mocked_bindings(
    get = function(...) "output",
    .package = "config"
  )

  expect_no_match(get_data_path(), "healthtrendsfiles")
})
