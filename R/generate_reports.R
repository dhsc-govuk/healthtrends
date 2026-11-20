#------------------------------------------------------------------------------#
#' Create summary page with try catch
#'
#' `try_catch_summary_page()` generates the summary page with error handling
#'
#' @param area_code character, area code of chosen area for page
#' @param area_name character, name of chosen area of the page
#' @param output_path character, file path for output
#' @param data_name character string, name of data rds file to be generated
#' @return error information if render fails
#' @noRd
try_catch_summary_page <- function(area_code,
                                   area_name,
                                   output_path,
                                   data_name) {
  tryCatch(
    {
      start_time <- Sys.time()

      quarto::quarto_render(
        input = "healthtrendsfiles/summary_page.qmd",
        output_format = "html",
        output_file = "output.html",
        execute_params = list(
          area_code = area_code,
          area_name = area_name,
          page_title = paste0("Health trends in ", format_area_name(area_name)),
          data_name = data_name
        ),
        execute_dir = getwd(),
        quiet = TRUE
      )

      # Move file to subdirectory to match website tree
      # QUARTO ISSUE: cannot make output file in a subdirectory otherwise it
      # doesn't 'see' libs website files to render
      if (
        !(
          str_replace_all(area_name, " ", "_") %in%
            list.dirs(output_path, full.names = FALSE)
        )
      ) {
        dir.create(paste0(output_path, "/", str_replace_all(area_name, " ", "_")))
      }

      # move file to subdirectory
      file.rename(
        "healthtrendsfiles/output.html",
        paste0(
          output_path,
          "/",
          str_replace_all(area_name, " ", "_"),
          "/overview.html"
        )
      )

      end_time <- Sys.time()
      duration <- round(difftime(end_time, start_time, units = "mins"), 2)
      cat("  Render of summary page for ", area_name, " completed in ", duration, " minutes\n", sep = "")

      TRUE
    },
    error = function(e) {
      error_row <- c("Summary", area_code, as.character(e))
      error_row
    }
  )
}


#------------------------------------------------------------------------------#
#' Create details page with try catch
#'
#' `try_catch_details_page()` generates the summary page with error handling
#'
#' @param topic character, chosen area for page
#' @param area_code character, area code of chosen area for page
#' @param area_name character, name of chosen area of the page
#' @param output_path character, file path for output
#' @param data_name character string, name of data rds file to be generated
#' @return error information if render fails
#' @noRd
try_catch_details_page <- function(topic,
                                   area_code,
                                   area_name,
                                   output_path,
                                   data_name) {
  tryCatch(
    {
      start_time <- Sys.time()

      quarto::quarto_render(
        input = "healthtrendsfiles/details_page.qmd",
        output_format = "html",
        output_file = "output.html",
        execute_params = list(
          area_code = area_code,
          area_name = area_name,
          selected_topic = topic,
          page_title = paste0(topic, " in ", format_area_name(area_name)),
          data_name = data_name
        ),
        execute_dir = getwd(),
        quiet = TRUE
      )

      if (
        !(
          str_replace_all(area_name, " ", "_") %in%
            list.dirs(output_path, full.names = FALSE)
        )
      ) {
        dir.create(paste0(output_path, "/", str_replace_all(area_name, " ", "_")))
      }

      # move file to subdirectory
      file.rename(
        "healthtrendsfiles/output.html",
        paste0(
          output_path,
          "/",
          str_replace_all(area_name, " ", "_"),
          "/",
          str_replace_all(str_to_lower(topic), " ", "_"),
          ".html"
        )
      )

      end_time <- Sys.time()
      duration <- round(difftime(end_time, start_time, units = "mins"), 2)
      cat("  Render of ", topic, " details page for ", area_name, " completed in ", duration, " minutes\n", sep = "")

      TRUE
    },
    error = function(e) {
      error_row <- c(topic, area_code, as.character(e))
      error_row
    }
  )
}


#------------------------------------------------------------------------------#
#' Rebase nav links for root-level pages
#'
#' `fix_root_page_links()` strips the leading "../" that `base_url` prepends to
#' every nav/TOC link. That prefix is only correct for area pages, which render
#' one directory deep (e.g. `England/overview.html`). The about and
#' accessibility pages render at the dated-folder root, so their links must be
#' root-relative instead. Edits the rendered HTML file in place.
#'
#' @param html_path character, path to the rendered HTML file to rewrite
#' @return invisibly TRUE
#' @noRd
fix_root_page_links <- function(html_path) {
  html <- readLines(html_path, warn = FALSE)
  html <- gsub('(href\\s*=\\s*["\'])\\.\\./', "\\1", html)
  writeLines(html, html_path)
  invisible(TRUE)
}


#------------------------------------------------------------------------------#
#' Generate the Health trends in England reports
#'
#' `generate_reports()` builds the Health trends in England (HtiE) HTML pages:
#' an about page, an accessibility statement, and an overview page plus one
#' details page per topic for England and each requested area. Pages are
#' rendered with Quarto and written to `output/<today's date>/`.
#'
#' A page that fails to render does not stop the run. Failed pages are listed
#' in `error_list.csv` in the output folder, and the full errors are printed
#' to the console.
#'
#' @param area_codes Character vector of 9-character ONS area codes. Upper
#'   tier local authorities (2023 boundaries; prefixes "E06", "E08", "E09",
#'   "E10") and statistical regions ("E12") are supported. England
#'   ("E92000001") is always included. Defaults to England and the nine
#'   regions.
#' @param refresh_data Logical. If `TRUE` (the default), pulls fresh data from
#'   the Fingertips API and saves it to
#'   `healthtrendsfiles/data/health_trends_data.rds`. If `FALSE`, reuses that
#'   file, which must already exist.
#' @param config_file Path to the config YAML file. Defaults to
#'   `"input/config.yaml"`, which [import_healthtrends_files()] creates.
#'
#' @seealso [import_healthtrends_files()] to set up the working directory,
#'   [set_header()] to customise the page header.
#'
#' @examples
#' \dontrun{
#' import_healthtrends_files()
#' generate_reports(area_codes = c("E12000007", "E09000033"))
#' }
#' @export
generate_reports <- function(
  area_codes = c("E92000001", "E12000001", "E12000002", "E12000003",
                 "E12000004", "E12000005", "E12000006", "E12000007",
                 "E12000008", "E12000009"),
  refresh_data = TRUE,
  config_file = "input/config.yaml"
) {
  # set config for session
  set_config(config_nm = "config", config_file = config_file)

  # Check area codes are correct type
  if (!is.character(area_codes) ||
        !isTRUE(all(nchar(area_codes) == 9)) ||
        !isTRUE(all(str_sub(area_codes, 1, 3) %in%
                      c("E06", "E08", "E09", "E10", "E12", "E92")))) {

    stop(
      "area_codes should be a character vector or 9 digit ONS areacodes. ",
      "Only UTLA areas (2023 boundaries) and statistical regions are supported.",
      call. = FALSE
    )

  }

  # correct any superseded Barnsley/Sheffield codes to the codes Fingertips holds
  area_codes <- correct_superseded_codes(area_codes)

  # Areas whose figures are, for most indicators, combined with another named
  # area in Fingertips rather than reported on their own. Each row names the
  # area it is usually combined with and that area's code.
  combined_areas <- data.frame(
    code      = c("E06000053",       "E09000001"),
    name      = c("Isles of Scilly", "City of London"),
    neighbour = c("Cornwall",        "Hackney"),
    nbr_code  = c("E06000052",       "E09000012"),
    stringsAsFactors = FALSE
  )

  hits <- combined_areas[combined_areas$code %in% area_codes, ]

  if (nrow(hits) > 0) {

    # one line per affected area, reused in both the stop and warning messages
    area_lines <- paste0(
      sprintf(
        "  \u2022 %s (%s) is usually combined with %s, so please use %s (%s) instead",
        hits$name, hits$code, hits$neighbour, hits$neighbour, hits$nbr_code
      ),
      collapse = "\n"
    )

    # areas left once the combined-only ones are removed (England, E92, is
    # added automatically later so does not count as a requested area)
    reportable <- area_codes[!area_codes %in% hits$code &
                               str_sub(area_codes, 1, 3) != "E92"]

    if (length(reportable) == 0) {
      # only combined-only areas were requested, so nothing can be produced
      stop(
        "No reports can be produced for the requested area(s).\n",
        "For most indicators these areas are usually combined with another ",
        "area in Fingertips rather than reported on their own:\n\n",
        area_lines,
        call. = FALSE
      )
    }

    # otherwise drop the combined-only areas and carry on with the rest
    area_codes <- area_codes[!area_codes %in% hits$code]

    warning(
      "Skipped ", nrow(hits), " area(s) that are usually combined with ",
      "another area in Fingertips.\n",
      "For most indicators these have little or no standalone data, so the ",
      "run will continue without them:\n\n",
      area_lines,
      "\n",
      call. = FALSE
    )
  }

  # error check for refresh_data word
  if (!rlang::is_bool(refresh_data)) {
    rlang::abort(message = "The refresh_data argument must be a single TRUE or FALSE value.")
  }

  # get api address from config
  api_url <- config::get(
    value = "api_url",
    config = "production"
  )

  # error check for api_url word
  if (
    !str_detect(api_url, "fingertips")
  ) {
    stop(
      "The api_url argument must be a valid fingertips url i.e. https://fingertips.phe.org.uk/api/"
    )
  }

  # Validate the requested area codes against the codes Fingertips actually
  # holds. England (E92000001) is added automatically and is a different area
  # type, so is excluded from this check.
  codes_to_check <- area_codes[str_sub(area_codes, 1, 3) != "E92"]

  if (length(codes_to_check) > 0) {
    # map area code prefix to Fingertips area type id (6 = region, 502 = UTLA)
    prefix_to_areatype <- with(area_type_lookup(), stats::setNames(AreaTypeID, prefix))
    areatypeids <- unique(prefix_to_areatype[str_sub(codes_to_check, 1, 3)])

    valid_codes <- get_valid_area_codes(areatypeids)

    invalid_codes <- setdiff(codes_to_check, valid_codes)

    if (length(invalid_codes) > 0) {
      stop(
        "The following area code(s) were not found in Fingertips for the ",
        "supported area types, so no report can be produced for them:\n\n  \u2022 ",
        paste(invalid_codes, collapse = "\n  \u2022 "),
        "\n\nCheck each code against the current Fingertips area list. If an ",
        "area's boundary changed recently, Fingertips may still hold it under ",
        "its previous ONS code.",
        call. = FALSE
      )
    }
  }

  data_name <- "health_trends_data.rds"

  # check data exists or create data
  if (!refresh_data && !file.exists(file.path("healthtrendsfiles", "data", data_name))) {
    rlang::abort(
      "Dataset does not exist. Set `refresh_data = TRUE` to create it."
    )
  }

  area_codes <- c("E92000001", area_codes[area_codes != "E92000001"])

  if (refresh_data) {
    # get all_data and save as an rds file
    get_data(area_codes = area_codes,
             data_name = data_name)
  }

  # Read in data file
  file_path <- file.path("healthtrendsfiles", "data", data_name)
  all_data <- readr::read_rds(file_path) |>
    filter(.data$AreaCode %in% area_codes)

  # Check that all area_codes required exist in the data
  missing <- setdiff(area_codes, unique(all_data$AreaCode))
  if (length(missing) > 0) {
    stop(
      "The following area_codes are not present in data: ",
      paste(missing, collapse = ", "),
      ". Please refresh the data with refresh_data = TRUE."
    )
  }

  # get all area names
  areas <- all_data |>
    distinct(.data$AreaCode, .data$AreaName)

  cat("\nCreating HTML output\n")

  # get output path from config
  output_path <- config::get(
    value = "output_path",
    config = "production"
  )

  if (!(file.exists(output_path))) {
    dir.create(output_path)
  }

  ## About page

  start_time <- Sys.time()

  quarto::quarto_render(
    input = "healthtrendsfiles/about.qmd",
    output_format = "html",
    output_file = "output.html",
    execute_dir = getwd(),
    quiet = TRUE
  )

  # Root-level page: strip the "../" base_url prefix so nav/TOC links resolve
  fix_root_page_links("healthtrendsfiles/output.html")

  file.rename("healthtrendsfiles/output.html", paste0(output_path, "/about.html"))

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  cat("  Render of about page completed in ", duration, " minutes\n", sep = "")


  ## Accessibility statement

  start_time <- Sys.time()

  quarto::quarto_render(
    input = "healthtrendsfiles/accessibility.qmd",
    output_format = "html",
    output_file = "output.html",
    execute_dir = getwd(),
    quiet = TRUE
  )

  # Root-level page: strip the "../" base_url prefix so nav/TOC links resolve
  fix_root_page_links("healthtrendsfiles/output.html")

  file.rename("healthtrendsfiles/output.html", paste0(output_path, "/accessibility.html"))

  end_time <- Sys.time()
  duration <- round(difftime(end_time, start_time, units = "mins"), 2)
  cat("  Render of accessibility statement page completed in ", duration, " minutes\n", sep = "")

  # Set up error dataframe to catch any failed pages
  error_df <- data.frame(
    col1 = character(),
    col2 = character(),
    col3 = character()
  )

  # Loop through areas to render
  for (area_code in area_codes) {
    area_name <- areas$AreaName[areas$AreaCode == area_code]

    cat("\n  ---- ", area_name, " ----\n", sep = "")


    # render page and catch error (x) if it fails to render

    x <- try_catch_summary_page(
      area_code = area_code,
      area_name = area_name,
      output_path = output_path,
      data_name = data_name
    )

    # Return if error (for some reason if quarto renders correctly, it returns
    # an error of TRUE ?? therefore test logical)
    if (!(is.logical(x))) {
      error_df <- rbind(error_df, x)
    }

    ##### details pages ########################################################

    # Get all topic names available for the area (just in case some topics are
    # only England level)
    topic_names <- get_indicators(
      data_file = file_path,
      area_code = area_code,
      topic_names = TRUE
    )

    # loop through all topics
    for (topic in topic_names) {
      x <- try_catch_details_page(
        topic = topic,
        area_code = area_code,
        area_name = area_name,
        output_path = output_path,
        data_name = data_name
      )

      # If there is an error, add it to error_df
      if (!(is.logical(x))) {
        error_df <- rbind(error_df, x)
      }
    }
  }

  cat("\n")

  # Write error dataframe to CSV if there are any errors

  if (nrow(error_df) > 0) {
    colnames(error_df) <- c("Page", "Area", "Error")

    # Print the full error details to the console, one per failed page, so the
    # messages are not truncated as they would be in a wide data frame column.
    cat("\nThe following pages failed to render:\n\n")
    for (i in seq_len(nrow(error_df))) {
      cat(
        "  Page:  ", error_df$Page[i], "\n",
        "  Area:  ", error_df$Area[i], "\n",
        "  Error: ", error_df$Error[i], "\n\n",
        sep = ""
      )
    }

    # The CSV lists only the failed pages (Page and Area). The full error
    # messages are printed to the console above rather than written to file.
    readr::write_csv(
      error_df[, c("Page", "Area")],
      paste0(output_path, "/", "error_list.csv")
    )

    message(
      nrow(error_df),
      " html page(s) were not created. See 'error_list.csv' in the output ",
      "directory for the list of failed pages, and the console output above ",
      "for the full error messages."
    )
  } else {
    message("SUCCESS! All pages generated with 0 errors")
  }
}
