# ------------------------------------------------------------------------------
#' Spotlight chart image
#'
#' `spotlight_chart` creates the chart for the summary page as an image
#'
#' @param spotlight_data dataframe, data for spotlight indicator
#' @param spotlight_ind numeric, indicator to spotlight on overview page
#' @param area_name character string, name of selected area
#' @return file path to the chart image
#' @noRd
spotlight_chart <- function(spotlight_data, spotlight_ind, area_name) {
  # retrieve df for selected indicator
  df <- spotlight_data |>
    # remove NAs at start of trend
    filter(cumsum(stats::complete.cases(.data$Value)) != 0) |>
    mutate(Value = janitor::round_half_up(.data$Value, unique(.data$Decimal_places)))

  # remove NAs at end of trend
  if (is.na(slice_tail(df)$Value)) {
    cut_off <- df |>
      filter(!is.na(.data$Value)) |>
      slice_tail() |>
      pull(.data$TimeperiodOrder)
    df <- df |>
      filter(.data$TimeperiodOrder <= cut_off)
  }

  # retrieve unit
  if (any(unique(spotlight_data$Unit) %in% "%")) {
    unit <- unique(spotlight_data$Unit)
  } else {
    unit <- ""
  }

  # html friendly date labels - replace dash with html code for dash
  start_timeperiod <- df$Timeperiod[1] |>
    as.character() |>
    str_replace_all("-", "&#8210;")

  end_timeperiod <- utils::tail(df$Timeperiod, 1) |>
    as.character() |>
    str_replace_all("-", "&#8210;")

  # adjust expansion for longer dates
  if (max(nchar(as.character(df$Timeperiod))) > 9) {
    exp <- 0.3
  } else {
    exp <- 0.15
  }

  # set y axis limits

  # function to round to nearest 10
  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }
  # life expectancy
  if (unique(df$IndicatorID) %in% 90366) {
    limits <- c(70, 90)
    breaks <- c(70, 80, 90)
  } else if (unique(df$IndicatorID) %in% 90362) {
    limits <- c(50, 70)
    breaks <- c(50, 60, 70)
    # percentages
  } else if (unique(spotlight_data$Unit) == "%") {
    ymax <- round_any(
      max(df$Value, na.rm = TRUE) * 1.3,
      10,
      f = ceiling
    )

    ymax <- ifelse(ymax > 90, 100, ymax)
    limits <- c(0, ymax)
    breaks <- c(0, ymax / 2, ymax)

    # values over 500
  } else if (max(df$Value, na.rm = TRUE) > 500) {
    min <- min(df$Value, na.rm = TRUE)
    max <- max(df$Value, na.rm = TRUE)
    ymin <- round_any(min - max * 0.2, 100, f = floor)
    ymax <- round_any(max * 1.2, 100, f = ceiling)
    limits <- c(ymin, ymax)
    breaks <- c(ymin, (ymin + ymax) / 2, ymax)

    # everything else
  } else {
    min <- min(df$Value, na.rm = TRUE)
    max <- max(df$Value, na.rm = TRUE)
    ymin <- round_any(min - max * 0.3, 10, f = floor)
    ymax <- round_any(max * 1.3, 10, f = ceiling)
    limits <- c(ymin, ymax)
    breaks <- c(ymin, (ymin + ymax) / 2, ymax)
  }

  # create chart
  fig <- ggplot(df, aes(x = .data$Timeperiod, y = .data$Value, group = 1)) +
    geom_line(color = "#12436D", linewidth = 2) +
    geom_point(color = "#12436D") +
    scale_x_discrete(
      breaks = c(df$Timeperiod[1], utils::tail(df$Timeperiod, 1)),
      # expand chart area to prevent labels being cut off
      expand = expansion(mult = exp),
      # set x axis label text in html code
      labels = c(
        paste0(
          "<span style = 'font-size:11pt;color:#505a5f;'>",
          start_timeperiod,
          "</span><p><p>",
          "<span style = 'font-size:16pt;color:#12436D;'>",
          formatC(
            df$Value[1],
            format = "f",
            big.mark = ",",
            digits = unique(df$Decimal_places)
          ),
          unit,
          "</span>"
        ),
        paste0(
          "<span style = 'font-size:11pt;color:#505a5f;'>",
          end_timeperiod,
          "</span><p><p>",
          "<span style = 'font-size:16pt;color:#12436D;'>",
          formatC(
            utils::tail(df$Value, 1),
            format = "f",
            big.mark = ",",
            digits = unique(df$Decimal_places)
          ),
          unit,
          "</span>"
        )
      )
    ) +
    scale_y_continuous(limits = limits, breaks = breaks) +
    theme_void() +
    theme(
      # element_markdown allows the html code to work
      axis.text.x = ggtext::element_markdown(margin = margin(t = 2)),
      axis.text.y = element_text(margin = margin(r = 2), colour = "#505a5f")
    )
  ## need to get only 2 labels on y axis
  sex <- str_replace_all(unique(df$Sex), " ", "_")

  # Save to an svg image file
  file <- paste0(
    "healthtrendsfiles/Images/",
    str_replace_all(area_name, " ", "_"),
    "_",
    spotlight_ind,
    "_",
    sex,
    ".svg"
  )
  ggsave(
    file,
    plot = fig,
    device = "svg",
    width = 3.5,
    height = 3,
    create.dir = TRUE
  )

  file <- fs::path_rel(file, start = "healthtrendsfiles")

  file
}

# ------------------------------------------------------------------------------
#' Line chart for details page
#'
#' `details_chart` creates line chart for the details pages
#'
#' @param ind_data dataframe, indicator data for England or region and England
#' @param area_name character string, name of selected area
#' @param n_sex numeric, whether there are 1 or 2 sexes per indicator
#' @return plotly line chart for an indicator
#' @noRd
details_chart <- function(ind_data, area_name, n_sex) {
  # set colours for line charts
  if (n_sex == 1) {
    ind_data <- ind_data |>
      mutate(Category = .data$AreaName)

    if (area_name == "England") {
      line_colours <- c("#12436D")
    } else {
      # make area name a factor to control line colour in regions
      ind_data <- ind_data |>
        mutate(
          Category = factor(
            .data$Category,
            levels = c(area_name, "England"),
            ordered = TRUE
          )
        )
      line_colours <- c("#F46A25", "#12436D")
    }
  } else if (n_sex == 2) {
    if (area_name == "England") {
      ind_data <- ind_data |>
        mutate(Category = .data$Sex)

      line_colours <- c("#12436D", "#28A197")
    } else {
      ind_data <- ind_data |>
        mutate(Category = paste0(.data$Sex, ", ", .data$AreaName))

      # make category (sex-area) a factor to control line colour
      ind_data <- ind_data |>
        mutate(
          Category = factor(
            .data$Category,
            levels = c(
              paste0("Male, ", area_name),
              "Male, England",
              paste0("Female, ", area_name),
              "Female, England"
            ),
            ordered = TRUE
          )
        )
      line_colours <- c("#801650", "#28A197", "#F46A25", "#12436D")
    }
  } else {
    stop("there are more than 2 sexes for one indicator")
  }

  # determine gap between x-axis ticks
  # include fewer labels if they're > 9 characters
  # determine x-axis tick start to ensure latest date is always displayed
  number_unique_dates <- length(unique(ind_data$Timeperiod))

  max_char <- max(nchar(as.character(ind_data$Timeperiod)))

  if (max_char >= 9 && number_unique_dates >= 7) {
    xtickgap <- ceiling(number_unique_dates / 5)
  } else {
    xtickgap <- ceiling(number_unique_dates / 10)
  }

  # plotly numbering starts at 0, hence - 1
  xtickstart <- ((number_unique_dates - 1) %% xtickgap)

  # Get y_axis value type and unit
  y_axis <- paste0(
    unique(ind_data$ValueType),
    " (",
    str_to_lower(unique(ind_data$Unit)),
    ")"
  )

  # customise y axis range
  ymax <- max(ind_data$Value, na.rm = TRUE) |> signif(3)

  if (ymax < 100) {
    ylim <- ifelse(ymax %% 10 != 0, ymax * 1.5, ymax * 1.5 + 1.2)
  } else {
    ylim <- ifelse(ymax %% 10 != 0, ymax * 1.3, ymax * 1.3 + 1.2)
  }

  # life expectancy indicators have specific axes
  if (unique(ind_data$IndicatorID) %in% 90366) {
    yrange <- c(69, 91)
  } else if (unique(ind_data$IndicatorID) %in% 90362) {
    yrange <- c(49, 71)
    # limit percentage axes to stop at 100
  } else if (unique(ind_data$Unit) %in% "%" && ylim > 100) {
    yrange <- c(0, 101) # 101 so that the 100 line shows
  } else {
    yrange <- c(0, ylim)
  }

  # Extract unique decimal places from the data
  dp <- unique(ind_data$Decimal_places)

  fig <- plotly::plot_ly(
    ind_data,
    x = ~Timeperiod,
    y = ~ janitor::round_half_up(Value, unique(dp)),
    color = ~Category,
    split = ~Category,
    colors = line_colours,
    type = "scatter",
    mode = "lines+markers",
    text = ~Category,
    hovertemplate = paste0(
      "<b>%{x}</b>", # <b> is for bold text
      "<br>%{text}",
      "<br>",
      y_axis,
      " %{y:,.",
      dp,
      "f}", # Use decimal places for y axis values
      "<extra></extra>"
    ), # <extra></extra> removes text outside hover box
    line = list(width = 4),
    marker = list(size = 4)
  ) |>
    plotly::layout(
      legend = list(
        orientation = "h",
        xanchor = "center",
        traceorder = "reversed",
        x = 0.5,
        y = -0.2,
        font = list(size = 14)
      ),
      xaxis = list(
        title = "Time period",
        titlefont = list(size = 14),
        tickmode = "linear",
        dtick = xtickgap,
        tick0 = xtickstart,
        ticks = "outside",
        tickcolor = "#eee",
        fixedrange = TRUE,
        showgrid = FALSE # Removes x-axis grid lines
      ),
      yaxis = list(
        title = NA,
        tickformat = ",",
        range = yrange,
        fixedrange = TRUE,
        zerolinecolor = "#eee" # same colour as default gridlines
      )
    ) |>
    # add horizontal y axis label at top of axis
    plotly::add_annotations(
      text = y_axis,
      font = list(size = 14),
      align = "left",
      showarrow = FALSE,
      x = -0.05,
      y = 0.99,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      yshift = 10
    ) |>
    # hide modebar, which appears on top right of figure by default
    plotly::config(displayModeBar = FALSE)

  fig
}
