# ---------------------------------------------------------------------------- #
# SETUP ----
# ---------------------------------------------------------------------------- #

# Run this to source the setup file when running the tests locally
if (!grepl("testthat", getwd(), fixed = TRUE)) {
  source("tests/testthat/setup.R")
}

area_codes <- c(
  "E12000001", "E12000002", "E12000003", "E12000004",
  "E12000005", "E12000006", "E12000007", "E12000008", "E12000009"
)

# ---------------------------------------------------------------------------- #
# profile_owner() ----
# ---------------------------------------------------------------------------- #
own_prof_all <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "profile_owner"),
    {
      profile_owner(
        indicators = c(253, 20401, 90362)
      )
    }
  )
)

test_that("profile_owner returns a data frame", {
  expect_s3_class(own_prof_all, "data.frame")
  expect_gt(nrow(own_prof_all), 0)
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "OwnerProfileId", "OwnerProfileName", "OwnerProfileUrlKey"
    )
    expect_true(all(expected_cols %in% names(own_prof_all)))
  }
)

# ---------------------------------------------------------------------------- #
# GET RAW DATA ----
# ---------------------------------------------------------------------------- #

## get_raw_data ----------------------------------------------------------------
get_raw_data_results <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "get_raw_data"),
    {
      get_raw_data(
        area_codes = area_codes,
        own_prof_all = own_prof_all,
        areatypeid = 6
      )
    }
  )
)

test_that("get_raw_data returns a data frame", {
  expect_type(get_raw_data_results, "list")
  expect_length(get_raw_data_results, 2)
  walk(get_raw_data_results, ~ expect_s3_class(.x, "data.frame"))
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "OwnerProfileId", "OwnerProfileName", "OwnerProfileUrlKey"
    )
    expect_true(all(expected_cols %in% names(get_raw_data_results$other_prof)))

    expected_cols <- c(
      "IndicatorID", "IndicatorName", "ParentCode", "ParentName",
      "AreaCode", "AreaName", "AreaType", "Sex", "Age",
      "CategoryType", "Category", "Timeperiod",
      "Value", "LowerCI95.0limit", "UpperCI95.0limit", "LowerCI99.8limit",
      "UpperCI99.8limit", "Count", "Denominator", "Valuenote",
      "RecentTrend", "ComparedtoEnglandvalueorpercentiles",
      "Comparedtopercentiles", "TimeperiodSortable", "Newdata", "Comparedtogoal"
    )
    expect_true(all(expected_cols %in% names(get_raw_data_results$df)))
  }
)

## fetch_missing_data ----------------------------------------------------------
mock_prof <- tibble(
  IndicatorID    = c(90366L, 90362L, 92901L, 90825L, 93721L, 108L, 93823L),
  OwnerProfileId = rep(19L, 7)
)

initial_data <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "initial_data"),
    {
      fingertipsR::fingertips_data(
        IndicatorID = mock_prof$IndicatorID,
        ProfileID = mock_prof$OwnerProfileId,
        AreaTypeID = 6,
        AreaCode = area_codes,
        path = "https://fingertips.phe.org.uk/api/"
      )
    }
  )
)

missing_data_list <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "fetch_missing_data"),
    fetch_missing_data(
      own_prof_all = own_prof_all,
      initial_data = initial_data,
      areatypeid = 6,
      area_codes = area_codes
    )
  )
)

test_that(
  "missing data is fetched and returned as a data frame",
  {
    expect_type(missing_data_list, "list")
    expect_length(missing_data_list, 2)
    walk(missing_data_list, ~ expect_s3_class(.x, "data.frame"))
  }
)


test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "OwnerProfileId", "OwnerProfileName", "OwnerProfileUrlKey"
    )
    expect_true(all(expected_cols %in% names(missing_data_list$other_prof)))


    expected_cols <- c(
      "IndicatorID", "IndicatorName", "ParentCode", "ParentName",
      "AreaCode", "AreaName", "AreaType", "Sex", "Age",
      "CategoryType", "Category", "Timeperiod",
      "Value", "LowerCI95.0limit", "UpperCI95.0limit", "LowerCI99.8limit",
      "UpperCI99.8limit", "Count", "Denominator", "Valuenote",
      "RecentTrend", "ComparedtoEnglandvalueorpercentiles",
      "Comparedtopercentiles", "TimeperiodSortable", "Newdata",
      "Comparedtogoal", "Timeperiodrange"
    )
    expect_true(all(expected_cols %in% names(missing_data_list$missing_df)))
  }
)

## Test for find_other_profile -------------------------------------------------
# Successfully identify missing data for all indicators
other_prof <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "find_other_profile", "success"),
    {
      find_other_profile(
        indicators = c(253, 90362),
        areatypeid = 6
      )
    }
  )
)

test_that("find_other_profile returns a data frame", {
  expect_s3_class(other_prof, "data.frame")
  expect_gt(nrow(other_prof), 0)
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "OwnerProfileId", "OwnerProfileName", "OwnerProfileUrlKey"
    )
    expect_true(all(expected_cols %in% names(other_prof)))
  }
)

# Mix of missing data
test_that("Warns for missing indicators", {
  suppress_encoding_warning(
    with_mock_dir(
      file.path("..", "mock_data", "find_other_profile", "partial_out"),
      {
        expect_warning(
          result <- find_other_profile(
            indicators = c(253, 000001),
            areatypeid = 6
          ),
          "missing indicators"
        )
        expect_s3_class(result, "data.frame")
        expect_gt(nrow(result), 0)
        expected_cols <- c(
          "IndicatorID", "OwnerProfileId", "OwnerProfileName",
          "OwnerProfileUrlKey"
        )
        expect_true(all(expected_cols %in% names(result)))
      }
    )
  )
})

# No data for missing indicator
test_that("Warns for missing indicators and passes an empty dataframe", {
  suppress_encoding_warning(
    with_mock_dir(
      file.path("..", "mock_data", "find_other_profile", "empty_out"),
      {
        expect_warning(
          result <- find_other_profile(
            indicators = c(000001),
            areatypeid = 6
          ),
          "missing indicators"
        )
        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), 0)
        expect_equal(ncol(result), 4)
      }
    )
  )
})


## get_missing_inds ------------------------------------------------------------
indicators <- own_prof_all$IndicatorID

missing_inds <- get_missing_inds(
  indicators,
  initial_data
)

test_that("compare_eng_data returns a vector", {
  expect_type(missing_inds, "integer")
  expect_true(length(missing_inds) > 0)
})


# ---------------------------------------------------------------------------- #
# metadata ----
# ---------------------------------------------------------------------------- #

## get_metadata ----------------------------------------------------------------
all_meta <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "get_metadata"),
    {
      get_metadata(indicators)
    }
  )
)

test_that("get_metadata returns a named list keyed by indicator", {
  expect_type(all_meta, "list")
  expect_gt(length(all_meta), 0)
  expect_true(all(as.character(unique(indicators)) %in% names(all_meta)))
})

test_that("each indicator's metadata contains a Descriptive sublist", {
  walk(all_meta, ~ expect_false(is.null(.x$Descriptive)))
})

test_that("get_metadata strips HTML from the descriptive free-text fields", {
  fields <- c("Name", "Definition", "DataSource", "SimpleName", "SimpleDefinition")
  walk(all_meta, function(meta) {
    present <- intersect(fields, names(meta$Descriptive))
    walk(present, function(f) {
      val <- meta$Descriptive[[f]]
      if (is.character(val) && length(val) == 1) {
        expect_false(grepl("<[^>]+>", val)) # no residual tags
        expect_false(grepl("&nbsp;", val, fixed = TRUE))
      }
    })
  })
})


## remove_meta_html ------------------------------------------------------------

meta_list_html <- list(
  Descriptive = list(
    Name             = "Some <b>indicator</b> name",
    Definition       = "Rate per 1,000 &nbsp; population<br>",
    DataSource       = "ONS <i>2023</i>",
    SimpleName       = "Simple <span>name</span>",
    SimpleDefinition = "A simple definition&frac12;",
    Unit             = "per <b>1,000</b>"
  ),
  IID = 253L
)

cleaned <- remove_meta_html(meta_list_html)

test_that("remove_meta_html strips HTML from the five selected fields", {
  expect_equal(cleaned$Descriptive$Name, "Some indicator name")
  expect_equal(cleaned$Descriptive$Definition, "Rate per 1,000   population")
  expect_equal(cleaned$Descriptive$DataSource, "ONS 2023")
  expect_equal(cleaned$Descriptive$SimpleName, "Simple name")
  expect_equal(cleaned$Descriptive$SimpleDefinition, "A simple definition\u00bd")
})

test_that("remove_meta_html leaves non-selected fields and top-level structure intact", {
  expect_equal(cleaned$Descriptive$Unit, "per <b>1,000</b>")
  expect_equal(cleaned$IID, 253L)
})

test_that("remove_meta_html only touches fields that are present", {
  partial <- list(Descriptive = list(Name = "Just a <b>name</b>"))
  out <- remove_meta_html(partial)
  expect_equal(out$Descriptive$Name, "Just a name")
  expect_named(out$Descriptive, "Name")
})

test_that("remove_meta_html errors on invalid input", {
  expect_error(remove_meta_html("not a list"), "must be a list")
  expect_error(remove_meta_html(list(Foo = 1)), "Descriptive")
})


## clean_html ------------------------------------------------------------------

test_that("clean_html strips HTML tags", {
  expect_equal(clean_html("<br>Some text here<br>"), "Some text here")
  expect_equal(clean_html("<a href='x'>link</a>"), "link")
  expect_equal(clean_html("a <b>bold</b> and <i>italic</i> b"), "a bold and italic b")
  expect_equal(clean_html("no tags at all"), "no tags at all")
})

test_that("clean_html replaces named HTML entities", {
  expect_equal(clean_html("a&nbsp;b"), "a b")
  expect_equal(clean_html("it&rsquo;s"), "it's")
  expect_equal(clean_html("one&frac12;"), "one\u00bd")
  expect_equal(clean_html("m&sup2;"), "m\u00b2")
})

test_that("clean_html removes carriage-return/newline pairs", {
  expect_equal(clean_html("line one\r\nline two"), "line oneline two")
})

test_that("clean_html leaves unhandled entities untouched", {
  expect_equal(clean_html("'good'"), "'good'")
})

test_that("clean_html errors on invalid input", {
  expect_error(clean_html(123), "single character string")
  expect_error(clean_html(c("a", "b")), "single character string")
  expect_error(clean_html(character(0)), "single character string")
})


# ---------------------------------------------------------------------------- #
# COMPLETE TREND ----
# ---------------------------------------------------------------------------- #

indicators <- c(90366L, 90631L, 20401L, 92517L, 93563L, 91183L, 91101L, 253L, 93574L, 92313L, 94175L)

## complete_trend --------------------------------------------------------------

complete_trend_df <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "complete_trend"),
    complete_trend(
      indicators = indicators,
      tp_threshold = 4
    )
  )
)

test_that("complelete trend data is fetched and returned as a data frame", {
  expect_s3_class(complete_trend_df, "data.frame")
  expect_gt(nrow(complete_trend_df), 0)
})

test_that("complelete trend data contains expected columns", {
  expected_cols <- c(
    "IndicatorID", "Timeperiod", "Sex", "SexID", "Age", "AgeID", "YearRange",
    "TimeperiodOrder2", "TimeperiodOrder"
  )
  expect_true(all(expected_cols %in% names(complete_trend_df)))
})

test_that("complelete trend data contains all indicators passed in", {
  expect_true(all(indicators %in% unique(complete_trend_df$IndicatorID)))
})

# other tests:
# make sure only one timeperiod series
# only one age category per ind


## select_age ------------------------------------------------------------------

load(paste0(testpath, "data/trend_meta_sex.RData"))

select_age_df <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "select_age"),
    select_age(data = trend_meta_sex)
  )
)

test_that("select age data is returned as a data frame", {
  expect_s3_class(select_age_df, "data.frame")
  expect_gt(nrow(select_age_df), 0)
})

test_that("select age data contains expected columns", {
  expected_cols <- c(
    "IndicatorID", "Age"
  )
  expect_true(all(expected_cols %in% names(select_age_df)))
})

test_that("select age data contains a single age category per indicator", {
  validation <- select_age_df |>
    summarise(
      n_age = n_distinct(.data$Age),
      .by = "IndicatorID"
    )

  expect_true(all(validation$n_age == 1))
})


## select_tp -------------------------------------------------------------------

load(paste0(testpath, "data/trend_meta_age.RData"))

select_tp_df <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "select_tp"),
    select_tp(
      data = trend_meta_age,
      tp_threshold = 4
    )
  )
)

test_that("select tp data is returned as a data frame", {
  expect_s3_class(select_tp_df, "data.frame")
  expect_gt(nrow(select_tp_df), 0)
})

test_that("select tp data contains expected columns", {
  expected_cols <- c(
    "IndicatorID", "Timeperiod"
  )
  expect_true(all(expected_cols %in% names(select_tp_df)))
})

test_that("select tp data contains a single time series per indicator", {
  validation <- select_tp_df |>
    summarise(
      n_tp = n_distinct(.data$Timeperiod),
      .by = "IndicatorID"
    )

  expect_true(all(validation$n_tp == 1))
})


# ---------------------------------------------------------------------------- #
# THRESHOLD CHECKS ----
# ---------------------------------------------------------------------------- #

tp_threshold_upper <- 4
tp_threshold_lower <- 2
na_threshold_upper <- 4
na_threshold_lower <- 2

make_dummy_rows <- function(indicator_id,
                            area_name,
                            area_code,
                            values) {
  tibble::tibble(
    IndicatorID = indicator_id,
    AreaName    = area_name,
    AreaCode    = area_code,
    Sex         = "Persons",
    Age         = "All ages",
    YearRange   = 1,
    Timeperiod  = seq_along(values),
    Value       = values
  )
}

dummy_data <- dplyr::bind_rows(
  # 100001: England below tp threshold, regions pass; all above na threshold
  make_dummy_rows(100001, "England", "E92000001", c(10, 20, 30)),
  make_dummy_rows(100001, "North East", "E12000001", c(10, 20, 30, 40)),
  make_dummy_rows(100001, "North West", "E12000002", c(10, 20, 30, 40)),

  # 100002: all areas above tp threshold, North West below na threshold
  make_dummy_rows(100002, "England", "E92000001", c(10, 20, 30, 40)),
  make_dummy_rows(100002, "North East", "E12000001", c(10, 20, NA, NA)),
  make_dummy_rows(100002, "North West", "E12000002", c(NA, NA, NA, NA))
)

## tp_threshold_check ----

below_upper_threshold_df <- tp_threshold_check(
  data = dummy_data,
  tp_area_codes = "E92000001",
  tp_threshold = tp_threshold_upper
)

test_that("tp_threshold_check returns a data frame", {
  expect_s3_class(below_upper_threshold_df, "data.frame")
  expect_gt(nrow(below_upper_threshold_df), 0)
})

test_that("data contains expected columns", {
  expected_cols <- c(
    "IndicatorID", "AreaName", "AreaCode", "Sex", "Age", "YearRange", "n"
  )
  expect_true(all(expected_cols %in% names(below_upper_threshold_df)))
})

test_that("tp_threshold_check identifies the expected indicator and area", {
  expect_equal(below_upper_threshold_df$IndicatorID, 100001)
  expect_equal(below_upper_threshold_df$AreaCode, "E92000001")
})

below_lower_threshold_df <- tp_threshold_check(
  data = dummy_data,
  tp_area_codes = c("E12000001", "E12000002"),
  tp_threshold = tp_threshold_lower
)

test_that("tp_threshold_check returns a data frame even when there are no indicators below tp threshold", {
  expect_s3_class(below_lower_threshold_df, "data.frame")
  expect_equal(nrow(below_lower_threshold_df), 0)
})

## na_threshold_check ----

below_lower_na_threshold_df <- na_threshold_check(
  data = dummy_data,
  na_area_codes = c("E12000001", "E12000002"),
  na_threshold = na_threshold_lower
)

test_that("na_threshold_check returns a data frame", {
  expect_s3_class(below_lower_na_threshold_df, "data.frame")
  expect_gt(nrow(below_lower_na_threshold_df), 0)
})

test_that("data contains expected columns", {
  expected_cols <- c(
    "IndicatorID", "AreaName", "AreaCode", "Sex", "Age", "YearRange", "non_na"
  )
  expect_true(all(expected_cols %in% names(below_lower_na_threshold_df)))
})

test_that("na_threshold_check identifies the expected indicator and area", {
  expect_equal(below_lower_na_threshold_df$IndicatorID, 100002)
  expect_equal(below_lower_na_threshold_df$AreaCode, "E12000002")
})

test_that("na_threshold_check returns a data frame even when there are no indicators below na threshold", {

  dummy_data2 <- dummy_data |> filter(IndicatorID != 100001)

  below_upper_na_threshold_df <- na_threshold_check(
    data = dummy_data2,
    na_area_codes = "E92000001",
    na_threshold = na_threshold_upper
  )

  expect_s3_class(below_upper_na_threshold_df, "data.frame")
  expect_equal(nrow(below_upper_na_threshold_df), 0)
})


# ---------------------------------------------------------------------------- #
# GET SIMPLE ----
# ---------------------------------------------------------------------------- #

## get_simple_name -------------------------------------------------------------

test_that("Test that get_simple_name returns the correct result", {
  expect_equal(
    get_simple_name(all_meta = all_meta, ind = 253),
    "Chronic obstructive pulmonary disease prevalence"
  )
  expect_equal(
    get_simple_name(all_meta = all_meta, ind = 20401),
    "Teenage pregnancy"
  )
  expect_equal(
    get_simple_name(all_meta = all_meta, ind = 90362),
    "Healthy life expectancy at birth"
  )
})

## get_simple_definition -------------------------------------------------------

test_that("Test that get_simple_name returns the correct result", {
  expect_equal(
    get_simple_definition(all_meta = all_meta, ind = 253),
    "Percentage of patients with chronic obstructive pulmonary disease (COPD) recorded on practice disease register"
  )

  expect_equal(
    get_simple_definition(all_meta = all_meta, ind = 20401),
    "Rate of conceptions in girls aged under 18"
  )

  expect_equal(
    get_simple_definition(all_meta = all_meta, ind = 90362),
    "The average number of years people can expect to live in 'good' general health based on contemporary mortality rates and prevalence of self-reported good health."
  )
})


# ---------------------------------------------------------------------------- #
# URL LINK  ----
# ---------------------------------------------------------------------------- #

## add_url_columns -------------------------------------------------------------
data_name <- tempfile(fileext = ".rds")
load(paste0(testpath, "data/url_link_data.RData"))

df_dp <- mutate(df_dp, AreaTypeID = 6)

df_url <- suppress_acronym_warning(
  suppress_encoding_warning(
    with_mock_dir(
      file.path("..", "mock_data", "add_url_columns"),
      {
        add_url_columns(
          data = df_dp,
          data_name = data_name,
          ind_df = ind_df,
          generate_urls = TRUE
        )
      }
    )
  )
)

test_that("add_url_columns returns a data frame", {
  expect_s3_class(df_url, "data.frame")
  expect_gt(nrow(df_url), 0)
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "IndicatorName", "AreaCode", "AreaName", "AreaType",
      "Timeperiod", "Sex", "Age",
      "Value", "LowerCI95.0limit", "UpperCI95.0limit",
      "Count", "TimeperiodSortable", "SexID", "AgeID",
      "YearRange", "TimeperiodOrder2", "TimeperiodOrder",
      "OwnerProfileName", "OwnerProfileUrlKey", "Decimal_places",
      "TrendURL", "DefinitionURL", "CSVProfileUrlKey",
      "ProfileURL", "ProfNameLC", "IndicatorNameLC"
    )
    expect_true(all(expected_cols %in% names(df_url)))
  }
)

## fingertips_link -------------------------------------------------------------

fingertips_link_df <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "fingertips_link"),
    {
      fingertips_link(
        data = df_dp,
        data_name = data_name
      )
    }
  )
)

test_that("fingertips_link returns a data frame", {
  expect_s3_class(fingertips_link_df, "data.frame")
  expect_gt(nrow(fingertips_link_df), 0)
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "AreaCode", "AreaName",
      "SexID", "AgeID", "YearRange",
      "OwnerProfileName", "OwnerProfileUrlKey", "TrendURL", "DefinitionURL"
    )
    expect_true(all(expected_cols %in% names(fingertips_link_df)))
    expect_equal(
      fingertips_link_df$TrendURL[1],
      "http://fingertips.phe.org.uk/profile/respiratory-disease/data#page/4/gid/8000003/pat/15/par/E92000001/ati/6/are/E12000001/iid/253/age/1/sex/4/yrr/1"
    )
  }
)


##  profile_link ---------------------------------------------------------------

df_ft_link <- df_dp |>
  left_join(
    fingertips_link_df,
    by = c(
      "IndicatorID", "AreaCode", "AreaName",
      "SexID", "AgeID", "YearRange",
      "OwnerProfileName", "OwnerProfileUrlKey"
    )
  )


profile_link_df <- suppress_encoding_warning(
  with_mock_dir(
    file.path("..", "mock_data", "profile_link"),
    {
      profile_link(
        data = df_ft_link,
        ind_df
      )
    }
  )
)

test_that("profile_link returns a data frame", {
  expect_s3_class(profile_link_df, "data.frame")
  expect_gt(nrow(profile_link_df), 0)
})

test_that(
  "data contains expected columns",
  {
    expected_cols <- c(
      "IndicatorID", "CSVProfileUrlKey", "ProfileURL", "ProfNameLC"
    )
    expect_true(all(expected_cols %in% names(profile_link_df)))
    expect_equal(
      profile_link_df$ProfileURL[1],
      "https://fingertips.phe.org.uk/profile/public-health-outcomes-framework"
    )
  }
)

# ---------------------------------------------------------------------------- #
# REGIONAL DATA CHECK ----
# ---------------------------------------------------------------------------- #

## remove_incomplete_region_data ---------------------------------------------

# 9 English regions (E12) + England (E92)
regions <- sprintf("E120000%02d", 1:9)
england <- "E92000001"

# Complete regional data: England + all 9 regions
complete_data <- tibble(
  IndicatorID = 1L,
  AreaCode = c(england, regions),
  Value = 1:10
)

# Incomplete regional data: England + 3 regions only
incomplete_data <- tibble(
  IndicatorID = 2L,
  AreaCode = c(england, regions[1:3]),
  Value = 1:4
)

# England-only indicator
england_only_data <- tibble(
  IndicatorID = 3L,
  AreaCode = england,
  Value = 1
)

complete_result <- suppressWarnings(remove_incomplete_region_data(complete_data))
incomplete_result <- suppressWarnings(remove_incomplete_region_data(incomplete_data))
england_result <- suppressWarnings(remove_incomplete_region_data(england_only_data))

test_that("remove_incomplete_region_data returns a list of data frames", {
  expect_type(complete_result, "list")
  expect_length(complete_result, 2)
  walk(complete_result, ~ expect_s3_class(.x, "data.frame"))
})

test_that("complete regional data is retained and not flagged", {
  expect_equal(nrow(complete_result$data), 10L)
  expect_equal(nrow(complete_result$area_check), 0L)
})

test_that("incomplete regional data has E12 rows removed but England retained", {
  expect_equal(incomplete_result$data$AreaCode, england)
  expect_true(2L %in% incomplete_result$area_check$IndicatorID)
})

test_that("England-only indicators are retained as-is", {
  expect_equal(nrow(england_result$data), 1L)
  expect_equal(england_result$data$AreaCode, england)
})

test_that("rows with -1 or missing Value are excluded from area counts", {
  # only England has a valid value, so the indicator is treated as
  # England-only and no E12 rows are removed
  value_filter_data <- tibble(
    IndicatorID = 4L,
    AreaCode = c(england, regions[1:2]),
    Value = c(1, -1, NA)
  )

  value_filter_result <- suppressWarnings(
    remove_incomplete_region_data(value_filter_data)
  )

  expect_equal(value_filter_result$area_check$n, 1L)
  expect_equal(nrow(value_filter_result$data), 3L)
})

test_that("a warning is raised listing affected indicators", {
  expect_warning(
    remove_incomplete_region_data(incomplete_data),
    "do NOT have complete regional data"
  )
})
