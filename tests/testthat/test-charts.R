# Testing Charts -------------------------------------------------------------
## Synthesis datasets ----
spotlight_details_data <- tibble(
  IndicatorID = 90362,
  AreaName = c(
    "South East", "South East",
    "England", "England"
  ),
  Timeperiod = c("2010-12", "2011-13", "2010-12", "2011-13"),
  TimeperiodOrder = c(1, 2, 1, 2),
  Sex = c("Male", "Female", "Male", "Female"),
  Value = c(60, 62, 65, 67),
  Unit = "%",
  ValueType = "Rate",
  Decimal_places = 1
)

spotlight_details_persons <- spotlight_details_data |>
  dplyr::mutate(Sex = "Persons")

spotlight_details_england <- spotlight_details_data |>
  dplyr::filter(AreaName == "England")


## spotlight_chart tests -------------------------------------------------------
test_that("spotlight_chart creates an SVG file", {
  file <- spotlight_chart(
    spotlight_data = spotlight_details_persons,
    spotlight_ind = 90362,
    area_name = "South East"
  )

  full_path <- file.path("healthtrendsfiles", file)

  expect_true(file.exists(full_path))
  expect_match(file, "\\.svg$")
})

test_that("spotlight_chart returns correct file naming structure", {
  file <- spotlight_chart(
    spotlight_data = spotlight_details_persons,
    spotlight_ind = 90362,
    area_name = "South East"
  )

  expect_match(file, "South_East_90362")
  expect_match(file, "Persons")
})

test_that("spotlight_chart handles NA trimming", {
  test_data <- spotlight_details_persons |>
    mutate(Value = c(NA, 60, 65, NA))

  expect_no_error(
    spotlight_chart(test_data, 90362, "South East")
  )
})

test_that("spotlight_chart handles percentage unit branch", {
  expect_no_error(
    spotlight_chart(
      spotlight_details_persons,
      spotlight_ind = 90362,
      area_name = "South East"
    )
  )
})

test_that("spotlight_chart returns relative path", {
  file <- spotlight_chart(
    spotlight_details_persons,
    90362,
    "South East"
  )

  expect_false(startsWith(file, "healthtrendsfiles"))
})


## details_chart tests -------------------------------------------------------
test_that("details_chart returns a plotly object", {
  fig <- details_chart(
    spotlight_details_persons,
    "South East",
    1
  )

  expect_s3_class(fig, "plotly")
})

test_that("details_chart errors when n_sex > 2", {
  expect_error(
    details_chart(
      spotlight_details_persons,
      "South East",
      3
    ),
    "more than 2 sexes"
  )
})
