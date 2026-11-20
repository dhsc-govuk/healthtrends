# ---------------------------------------------------------------------------- #
# SETUP ----
# ---------------------------------------------------------------------------- #
# Run this to source the setup file when running the tests locally
if (!grepl("testthat", getwd(), fixed = TRUE)) {
  source("tests/testthat/setup.R")
}

# html_toc ---------------------------------------------------------------------
test_that("html_toc returns expected HTML structure", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_toc(
      topic = "My Topic",
      area_name = "My Area",
      section_titles = c("Section 1", "Section 2")
    )
  )

  out <- paste(out, collapse = "")

  expect_match(out, "<details")
  expect_match(out, "<ul class=\"app-subnav__section\"")
  expect_match(out, "Table of contents")
})

test_that("urls are correctly constructed", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_toc(
      topic = "My Topic",
      area_name = "My Area",
      section_titles = c("Section 1")
    )
  ) |> paste(collapse = "")

  expect_match(
    out,
    'href= "https://example.com/my_area/my_topic.html#section-1"'
  )
})

test_that("punctuation is removed from section titles in anchors", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_toc(
      topic = "Topic",
      section_titles = c("A+B%, Test (Example)")
    )
  ) |> paste(collapse = "")

  expect_match(out, "#ab-test-example")
})

test_that("area is optional and formatted correctly", {
  Sys.setenv(BASE_URL = "https://example.com/")

  # with area
  out1 <- capture.output(
    html_toc("Topic", "My Area", "Section")
  ) |> paste(collapse = "")

  expect_match(out1, "example.com/my_area/topic.html")

  # without area
  out2 <- capture.output(
    html_toc("Topic", "", "Section")
  ) |> paste(collapse = "")

  expect_match(out2, "example.com/topic.html")
})

test_that("handles empty section titles", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_toc("Topic", section_titles = character())
  ) |> paste(collapse = "")

  expect_match(out, "<ul")
  expect_no_match(out, "<li")
})


# html_sidebar -----------------------------------------------------------------
test_that("html_sidebar returns expected structure", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = c("Topic A", "Topic B"),
      selected_topic = "Topic A",
      area_name = "My Area"
    )
  ) |> paste(collapse = "")

  expect_match(out, "<ul class=\"app-subnav__section\"")
  expect_match(out, "Back to top")
  expect_match(out, "<br>")
})

test_that("urls are constructed correctly with area", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = c("Topic A"),
      area_name = "My Area"
    )
  ) |> paste(collapse = "")

  expect_match(
    out,
    'href= "https://example.com/my_area/topic_a.html"'
  )
})

test_that("About link excludes area from url", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = c("About"),
      area_name = "My Area"
    )
  ) |> paste(collapse = "")

  expect_match(out, 'href= "https://example.com/about.html"')
  expect_no_match(out, "my_area/about.html")
})

test_that("selected topic gets current class", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = c("Topic A", "Topic B"),
      selected_topic = "Topic B",
      area_name = "Area"
    )
  ) |> paste(collapse = "")

  expect_match(
    out,
    "Topic B</a></li>"
  )

  expect_match(
    out,
    "app-subnav__section-item--current"
  )
})

test_that("section break is added before About", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = c("Topic A", "About"),
      area_name = "Area"
    )
  ) |> paste(collapse = "")

  expect_match(out, "sidenav-section-break")
})

test_that("area is converted to snake case", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = "Topic A",
      area_name = "South East"
    )
  ) |> paste(collapse = "")

  expect_match(out, "south_east/topic_a.html")
})

test_that("handles empty topics vector", {
  Sys.setenv(BASE_URL = "https://example.com/")

  out <- capture.output(
    html_sidebar(
      topics = character(),
      area_name = "Area"
    )
  ) |> paste(collapse = "")

  expect_match(out, "<ul")
})


# html_title_dropdown ----------------------------------------------------------

test_that("Overview topic is displayed as 'Health trends'", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names  = c("South East", "North West"),
      topic = "Overview"
    )
  ), collapse = "")

  expect_match(html, "<h1>Health trends in the")
})

test_that("'the' is added for regions except England, Yorkshire and London", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West")
    )
  ), collapse = "")

  expect_match(html, "Health trends in the")
})

test_that("'the' is not added for England", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "England",
      all_area_names = c("England", "South East")
    )
  ), collapse = "")

  expect_match(html, "Health trends in")
  expect_no_match(html, "Health trends in the")
})

test_that("'the' is not added for London", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "London",
      all_area_names = c("London", "South East")
    )
  ), collapse = "")

  expect_no_match(html, "in the")
})

test_that("selected area is displayed in the dropdown button", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West")
    )
  ), collapse = "")

  expect_match(html, "South East &#9660;")
})

test_that("selected area does not appear in dropdown links", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West", "East Midlands")
    )
  ), collapse = "")

  expect_false(
    grepl(
      '<a href = "https://example.com/south_east/overviewEast</a>',
      html,
      fixed = TRUE
    )
  )
})

test_that("dropdown links are generated correctly", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West"),
      topic = "Health Care"
    )
  ), collapse = "")

  expect_match(
    html,
    'href = "https://example.com/north_west/health_care.html">North West</a>',
    fixed = TRUE
  )
})

test_that("multiple areas generate multiple links", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  html <- paste(capture.output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West", "East Midlands")
    )
  ), collapse = "")

  expect_equal(stringr::str_count(html, "<a href"), 2)
})

test_that("function returns HTML via cat()", {
  withr::local_envvar(BASE_URL = "https://example.com/")

  expect_output(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West")
    ),
    "<div id=\"main\">"
  )

  expect_null(
    html_title_dropdown(
      selected_area_name = "South East",
      all_area_names = c("South East", "North West")
    )
  )
})
