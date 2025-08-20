# Load necessary libraries
# If you don't have them installed, run install.packages("dplyr") and install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#' Generic Function to Plot Indicator Time Series
#'
#' This function generates a ggplot2 visualization for any time series data,
#' It includes a line plot, points, and horizontal lines for the mean and
#' +/- 1 standard deviation of the indicator values.
#'
#' @param data A data frame containing the time series data. It must have
#'             a 'Year' column and a column specified by 'value_col'.
#'
#' @param value_col A string specifying the name of the column in 'data' that
#'                  contains the indicator values to be plotted (e.g., "NAO_Index", "Estimate_Value").
#' @param plot_title A string for the plot title (default: ""). Set to NULL or "" for no title.
#' @param x_label A string for the x-axis label (default: "Year").
#' @param y_label A string for the y-axis label (default: "Indicator Value").
#' @param x_axis_breaks A numeric vector specifying desired x-axis breaks
#'                      (e.g., c(1960, 1980, 2000, 2020)). If NA, default breaks
#'                      will be used.
#' @param x_axis_limits A numeric vector of length 2 specifying the x-axis limits
#'                      (e.g., c(1950, 2025)). If NA, limits will be determined
#'                      automatically by ggplot2.
#' @param img_dir The directory where the plot image should be saved (default:
#'                current working directory).
#' @param base_font_size The base font size for the plot theme (default: 16).
#' @param include_axis_titles Logical, whether to include X and Y axis titles
#'                            (default: TRUE).
#' @param set_aspect_ratio Logical, whether to set a fixed aspect ratio for the
#'                         plot (default: FALSE).
#' @param line_color Color for the data line (default: "black").
#' @param point_color Color for the data points (default: "black").
#' @param hline_color Color for the mean and standard deviation lines (default: "darkgreen").
#'
#' @return The filename of the saved plot (if saved), otherwise prints the
#'         ggplot2 object.

plot_indicator_time_series <- function(
  data,
  value_col,
  facet_by = NULL,
  plot_title = "",
  x_label = "Year",
  y_label = "Indicator Value",
  x_axis_breaks = NA,
  x_axis_limits = NA,
  img_dir = ".",
  base_font_size = 16,
  include_axis_titles = TRUE,
  set_aspect_ratio = FALSE,
  line_color = "black",
  point_color = "black",
  hline_color = "darkgreen"
) {
  # Input validation
  if (!"Year" %in% names(data)) {
    stop("Data frame must contain a 'Year' column.")
  }
  if (!value_col %in% names(data)) {
    stop(paste0(
      "Data frame must contain a '",
      value_col,
      "' column for indicator values."
    ))
  }
  # Check for faceting column if specified
  if (!is.null(facet_by) && !(facet_by %in% names(data))) {
    stop(paste0(
      "Data frame must contain a '",
      facet_by,
      "' column for faceting."
    ))
  }
  # Calculate mean and standard deviation of the specified value column
  # Use .data[[]] for dynamic column access within dplyr/ggplot contexts
  mean_value <- mean(data[[value_col]], na.rm = TRUE)
  sd_value <- sd(data[[value_col]], na.rm = TRUE)

  # Generate a unique filename using the plot title and current date
  # Sanitize title for filename
  clean_title <- gsub("[^[:alnum:]]", "_", plot_title)
  if (nchar(clean_title) == 0) {
    clean_title <- value_col
  }
  #short_name <- paste0(clean_title, "_", Sys.Date(), ".png")
  short_name <- paste0(clean_title, ".png")
  fname <- file.path(img_dir, short_name)

  # Create the ggplot object
  fig <- ggplot(data, aes(x = Year, y = .data[[value_col]])) + # Use .data[[]] for dynamic y aesthetic
    geom_hline(
      yintercept = mean_value + sd_value,
      color = hline_color,
      linetype = "solid"
    ) +
    geom_hline(
      yintercept = mean_value - sd_value,
      color = hline_color,
      linetype = "solid"
    ) +
    geom_hline(
      yintercept = mean_value,
      color = hline_color,
      linetype = "dotted"
    ) +
    geom_line(color = line_color) +
    geom_point(color = point_color) +
    labs(title = plot_title, x = x_label, y = y_label) +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(hjust = 0.5), # Center the title
      axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis labels
      plot.background = element_rect(
        fill = "transparent",
        color = "transparent"
      ) #,
      # panel.background = element_rect(fill = "transparent", color = NA) # Ensure panel is also transparent
    )

  # Add faceting if a column is specified
  if (!is.null(facet_by)) {
    fig <- fig + facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y")
  }

  # Apply x-axis limits if specified
  if (!is.na(x_axis_limits[1])) {
    fig <- fig + coord_cartesian(xlim = x_axis_limits)
  }

  # Apply x-axis breaks if specified
  if (!is.na(x_axis_breaks[1])) {
    fig <- fig + scale_x_continuous(breaks = x_axis_breaks)
  }

  # Optionally remove axis titles
  if (!include_axis_titles) {
    fig <- fig + theme(axis.title = element_blank())
  }

  # Optionally set aspect ratio
  if (set_aspect_ratio) {
    fig <- fig + theme(aspect.ratio = 1 / 4)
  }

  # Save the plot
  ggplot2::ggsave(
    fname,
    plot = fig,
    width = 6, # Fixed width from previous examples
    height = 2, # Fixed height from previous examples for non-faceted plot
    units = "in",
    dpi = 300 #,
    # bg = "transparent"
  ) # Ensure transparent background for saved file

  message(paste0("Plot saved as '", fname, "'"))
  return(fname) # Return the full path of the saved file
}

## second plotting function, more similar to what was used for bsb ----

plt_herring <- function(
  data,
  ind_name, # indicator name to filter by, will also be part of the file name
  new_breaks = NA,
  img_dir,
  years = 1989:2025
) {
  this_dat <- data |>
    dplyr::filter(stringr::str_detect(INDICATOR_NAME, ind_name))

  this_dat <- this_dat |>
    # add NA for missing years
    dplyr::full_join(expand.grid(
      YEAR = years,
      INDICATOR_NAME = unique(this_dat$INDICATOR_NAME)
    )) |>
    dplyr::arrange(YEAR)

  short_name <- paste0(ind_name, "_", Sys.Date(), ".png")
  fname <- paste(img_dir, short_name, sep = "/")

  if (max(this_dat$DATA_VALUE, na.rm = TRUE) > 10^6) {
    this_dat <- this_dat |>
      dplyr::mutate(
        DATA_VALUE = ifelse(!is.na(DATA_VALUE), DATA_VALUE / 10^6, DATA_VALUE),
        INDICATOR_NAME = paste(INDICATOR_NAME, "millions")
      )
    fname <- paste(
      image_dir,
      paste0(ind_name, "_millions_", Sys.Date(), ".png"),
      sep = "/"
    )
  }

  # print(fname)
  fig <- NEesp2::plt_indicator(this_dat, include_trends = FALSE) +
    ggplot2::xlim(c(min(years), max(years))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      )
    )

  if (!is.na(new_breaks[1])) {
    fig <- fig +
      ggplot2::scale_x_continuous(breaks = new_breaks)
  } else {
    fig <- fig +
      ggplot2::scale_x_continuous(
        breaks = c(1980, 1990, 2000, 2010, 2020, 2025)
      )
  }

  ggplot2::ggsave(fname, width = 6, height = 2)

  return(short_name)
}
