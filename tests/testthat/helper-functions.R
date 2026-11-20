# tests/testthat/helper-functions.R
# Helper functions used across the test files. testthat sources helper files
# before setup.R and before the tests run, and devtools::load_all() loads them
# too, so they are available when running tests interactively.

# Shortens Fingertips API URLs so httptest mock file paths stay short.
# Used by set_requester() in setup.R.
# Mock path abbreviations:
#   ft/d = api/all_data/csv/by_indicator_id
#   ft/t = api/trend_data/specific_indicators_for_child_areas
#   ft/p = api/profiles_containing_indicators
#   ft/a = api/area_types/parent_area_types
#   ft/m = api/indicator_metadata/by_indicator_id
#   ft/o = api/indicator_metadata/indicator_owner
#   ft/  = api/ (everything else)
shorten_url <- function(x, gsub_fn) {
  x |>
    gsub_fn("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id", "ft/d", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/trend_data/specific_indicators_for_child_areas", "ft/t", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/profiles_containing_indicators", "ft/p", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/area_types/parent_area_types", "ft/a", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/indicator_metadata/by_indicator_id", "ft/m", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/indicator_metadata/indicator_owner", "ft/o", fixed = TRUE) |>
    gsub_fn("https://fingertips.phe.org.uk/api/", "ft/", fixed = TRUE)
}

# This function silences the 'No encoding supplied: defaulting to UTF-8.' that
# occurs within the fingertipsR package due to no encoding being specified within
# the fingertipsR functions
suppress_encoding_warning <- function(expr) {
  withCallingHandlers(
    expr,
    message = function(m) {
      if (grepl("No encoding supplied", conditionMessage(m), fixed = TRUE)) {
        invokeRestart("muffleMessage")
      }
    }
  )
}

# This function silences the expected capitalisation warnings from
# lowercase_except_acronyms() (e.g. 'the acronym "NHS" remains capitalised').
# Any other warning still shows.
suppress_acronym_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("remains capitalised|decapitalised", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
