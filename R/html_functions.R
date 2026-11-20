#' Generate HTML sidebar of anchored links based on base URL
#'
#' `html_title_dropdown` produces HTML code for the title with area dropdown
#'
#' @param selected_area_name character string, the name of the area of the page
#' @param all_area_names vector, all areas included in the project
#' @param topic character string, current topic
#' @return HTML code for title with dropdown
#' @noRd
html_title_dropdown <- function(selected_area_name, all_area_names, topic = "Overview") {
  topic2 <- str_to_lower(str_replace_all(topic, " ", "_"))

  # Change title to 'Health trends in region' for summary pages
  if (topic == "Overview") {
    topic <- "Health trends"
  }

  # Article ("the") for regions that need it, kept as black header text.
  # format_area_name() returns "the <name>" or "<name>"; take the prefix.
  the <- str_remove(format_area_name(selected_area_name), selected_area_name)

  header <- paste0('<div id="main"><h1>', topic, " in ", the)

  button <- paste0(
    '<div class="dropdown3"><button onclick="myFunction3()" class="dropbtn3">',
    selected_area_name,
    " &#9660;</button>"
  )

  dropdown_content <- '<div id="myDropdown3" class="dropdown-content3">'

  # remove selected area from list to avoid duplication
  areas2 <- all_area_names[!all_area_names == selected_area_name]
  base_url <- config::get(
    value = "base_url",
    config = "production"
  )

  for (area in areas2) {
    area2 <- str_to_lower(str_replace_all(area, " ", "_"))

    url <- paste0('"', base_url, area2, "/", topic2, '.html"')

    dropdown_content <- paste0(dropdown_content, "<a href = ", url, ">", area, "</a>")
  }

  html <- paste0(header, button, dropdown_content, "</div></div></h1></div>")

  cat(html)
}

#------------------------------------------------------------------------------#
#' Generate HTML sidebar of page links based on base URL
#'
#' `html_sidebar` produces sidebar to navigate through topics on website
#'
#' @param topics vector, all topics in project
#' @param selected_topic character string, the topic of the page
#' @param area_name character string, name of the area of the page
#' @return HTML code for sidebar
#' @noRd
html_sidebar <- function(topics, selected_topic = "%", area_name) {
  area2 <- str_to_lower(str_replace_all(area_name, " ", "_"))

  html <- '<br><ul class="app-subnav__section">'

  base_url <- config::get(
    value = "base_url",
    config = "production"
  )

  for (topic in topics) {
    topic2 <- str_to_lower(str_replace_all(topic, " ", "_"))

    # Use different class for different styling if topic is selected
    if (topic == selected_topic) {
      class <- '"app-subnav__section-item app-subnav__section-item--current"'
    } else {
      class <- '"app-subnav__section-item"'
    }

    # Create url and exclude area name from "About" url
    if (topic != "About") {
      url <- paste0('"', base_url, area2, "/", topic2, '.html"')
    } else {
      url <- paste0('"', base_url, topic2, '.html"')
    }


    # Create section break above "About"
    if (topic == "About") {
      html <- paste0(
        html,
        '<div class="sidenav-section-break"></div>'
      )
    }

    html <- paste0(
      html,
      "<li class = ", class,
      ' ><a class="app-subnav__link govuk-link govuk-link--no-visited-state govuk-link--no-underline" href= ',
      url, ">",
      topic,
      "</a></li>"
    )
  }

  # Add 'Back to top' link at the end
  html <- paste0(
    html,
    '<li class="app-subnav__section-item" style="margin-top: 20px"><a class="govuk-link govuk-link--no-visited-state" href="#top"><svg class="app-back-to-top__icon" xmlns="http://www.w3.org/2000/svg" width="13" height="17" viewBox="0 0 13 17">
      <path fill="currentColor" d="M6.5 0L0 6.5 1.4 8l4-4v12.7h2V4l4.3 4L13 6.4z"></path></svg>Back to top</a></li>'
  )

  cat(paste0(html, "</ul>"))
}

#------------------------------------------------------------------------------#
#' Generate HTML table of contents inside a details component
#'
#' `html_toc` produces table of contents for details page listing chart titles
#'
#' @param topic character, selected topic
#' @param area_name character string, name of the area of the page, default set to an empty
#' string if page is not in a subdirectory
#' @param section_titles character vector, list of titles to go in table of contents
#' @return HTML code for table of contents
#' @noRd
html_toc <- function(topic, area_name = "", section_titles) {
  area2 <- str_to_lower(str_replace_all(area_name, " ", "_"))

  # if URL has area subdirectory, add forward slash
  if (area2 != "") {
    area2 <- paste0(area2, "/")
  }

  topic2 <- str_to_lower(str_replace_all(topic, " ", "_"))

  html <- '<ul class="app-subnav__section" style="font-size: 18px;">'

  base_url <- config::get(
    value = "base_url",
    config = "production"
  )

  for (title in section_titles) {
    # change punctuation for URL
    title2 <- title |>
      str_to_lower() |>
      str_remove_all("[:+%,/'()]") |>
      str_replace_all(" ", "-") |>
      str_remove("-$")

    url <- paste0('"', base_url, area2, topic2, ".html#", title2, '"')

    html <- paste0(
      html,
      '<li class = "app-subnav__section-item">',
      '<a class="app-subnav__link govuk-link govuk-link--no-visited-state govuk-link--no-underline" href= ',
      url, ">",
      title,
      "</a></li>"
    )
  }

  # Put table of contents within details component
  details1 <-
    '<details class="govuk-details"><summary class="govuk-details__summary"><span class="govuk-details__summary-text">'

  details_title <- "Table of contents"

  details2 <- '</span></summary><div class="govuk-details__text">'

  details3 <- "</div></details>"

  # Combine details component and chart name table of contents
  cat(paste0(details1, details_title, details2, html, "</ul>", details3))
}
