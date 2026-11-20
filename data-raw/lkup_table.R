region_lookup_table <- tibble(
  AreaCode = c(
    "E92000001", "E12000001", "E12000002", "E12000003", "E12000004",
    "E12000005", "E12000006", "E12000007", "E12000008", "E12000009"
  ),
  AreaName = c(
    "England", "North East", "North West", "Yorkshire and the Humber", "East Midlands",
    "West Midlands", "East of England", "London", "South East", "South West"
  )
)

usethis::use_data(region_lookup_table, overwrite = TRUE)
