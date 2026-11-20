suppressPackageStartupMessages({
  library(httptest, warn.conflicts = FALSE, quietly = TRUE)
  library(tibble, warn.conflicts = FALSE, quietly = TRUE)
})

# tests/testthat/setup.R
# Global test setup. Helper functions (shorten_url(), suppress_encoding_warning(),
# suppress_acronym_warning()) live in helper-functions.R, which testthat sources
# before this file.

set_requester(function(request) {
  request |>
    gsub_request("timestamp=[0-9]+", "timestamp=REDACTED", fixed = FALSE) |>
    shorten_url(gsub_request)
})

# Adjust the path based on running within testthat or locally.
# This stays here rather than in a helper file: devtools::load_all() sources
# helper files from inside tests/testthat, which would always give "".
if (grepl("testthat", getwd(), fixed = TRUE)) {
  testpath <- ""
} else {
  testpath <- "tests/testthat/"
}

# set config for testing
Sys.setenv(
  R_CONFIG_FILE = testthat::test_path("conf", "test-config.yaml")
)

# fingertipsR checks the API is up by requesting the Fingertips homepage,
# which httptest saves as an .html mock in every folder. Skip the check
# during tests; only its status code is used.
mock_api_check <- function(...) TRUE

if (testthat::is_testing()) {
  # devtools::test(): mock for the test run, undone automatically afterwards
  testthat::local_mocked_bindings(
    fingertips_ensure_api_available = mock_api_check,
    .package = "fingertipsR",
    .env = testthat::teardown_env()
  )
} else {
  # Running tests line by line: local_mocked_bindings() would be undone as
  # soon as source() finishes, so patch fingertipsR for the rest of the
  # session instead. Restart R to undo.
  utils::assignInNamespace(
    "fingertips_ensure_api_available",
    mock_api_check,
    ns = "fingertipsR"
  )
}
