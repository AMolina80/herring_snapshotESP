# Load necessary libraries
# If you don't have them installed, run install.packages("dplyr") and install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#' Plot Haddock Predation Index
#'
#' This function generates a ggplot2 visualization of a time series, specifically
#' formatted to match the style of the NAO index plot. It includes a line plot, points,
#' and horizontal lines for the mean and +/- 1 standard deviation of the index.
#'
#' @param data A data frame containing at least 'Year' and a column for the 'Estimate_Value'.
#'             The function expects the value column to be named 'Estimate_Value'.
#' @param plot_title The title for the plot (default: ""). Set to NULL or "" for no title.
#' @param x_label The label for the x-axis (default: "Year").
#' @param y_label The label for the y-axis (default: "Predation Index Estimate").
#' @param x_axis_breaks A numeric vector specifying desired x-axis breaks (e.g., c(1960, 1980, 2000, 2020)).
#'                      If NA, default breaks will be used.
#' @param x_axis_limits A numeric vector of length 2 specifying the x-axis limits (e.g., c(1960, 2025)).
#'                      If NA, limits will be determined automatically by ggplot2.
#' @param img_dir The directory where the plot image should be saved (default: current working directory).
#' @param base_font_size The base font size for the plot theme (default: 16).
#' @param include_axis_titles Logical, whether to include X and Y axis titles (default: TRUE).
#' @param set_aspect_ratio Logical, whether to set a fixed aspect ratio for the plot (default: FALSE).
#'
#' @return The filename of the saved plot (if saved), otherwise prints the ggplot2 object.
#' @examples
#' # First, load and prepare your data:
#' # haddock_data <- read.csv("haddock_eat_herring_eggs_index.csv")
#' # Prepare the data to match the expected column names for the function:
#' # haddock_prepared_data <- haddock_data %>%
#' #   rename(Year = YEAR, Estimate_Value = est) %>%
#' #   select(Year, Estimate_Value) # Select only the necessary columns
#'
#' # Then, use the plotting function without a title:
#' # haddock_predation_index(haddock_prepared_data,
#' #                        plot_title = "", # Set plot_title to an empty string for no title
#' #                        x_axis_breaks = c(1960, 1980, 2000, 2020),
#' #                        x_axis_limits = c(1962, 2005),
#' #                        img_dir = tempdir(), # Saves to a temporary directory
#' #                        include_axis_titles = FALSE,
#' #                        set_aspect_ratio = TRUE)
plot_haddock_predation_index <- function(data,
                                    plot_title = "", # Default changed to an empty string for no title
                                    x_label = "Year",
                                    y_label = "Predation Index Estimate",
                                    x_axis_breaks = NA,
                                    x_axis_limits = NA,
                                    img_dir = ".", # Current working directory by default
                                    base_font_size = 16,
                                    include_axis_titles = TRUE,
                                    set_aspect_ratio = FALSE) {
  
  # Ensure the data has the expected column names
  if (!"Year" %in% names(data) || !"Estimate_Value" %in% names(data)) {
    stop("Data must contain 'Year' and 'Estimate_Value' columns.")
  }
  
  # Calculate mean and standard deviation for reference lines
  mean_value <- mean(data$Estimate_Value, na.rm = TRUE)
  sd_value <- sd(data$Estimate_Value, na.rm = TRUE)
  
  # Generate a unique filename using the current date
  short_name <- paste0("haddock_predation_index_", Sys.Date(), ".png")
  fname <- file.path(img_dir, short_name) # Use file.path for cross-platform compatibility
  
  # Create the ggplot object
  fig <- ggplot(data, aes(x = Year, y = Estimate_Value)) +
    geom_hline(yintercept = mean_value + sd_value,
               color = "darkgreen",
               linetype = "solid",
               size = 0.8) +
    geom_hline(yintercept = mean_value - sd_value,
               color = "darkgreen",
               linetype = "solid",
               size = 0.8) +
    geom_hline(yintercept = mean_value,
               color = "darkgreen",
               linetype = "dotted",
               size = 0.8) +
    geom_point() +
    geom_line() +
    labs(title = plot_title,
         x = x_label,
         y = y_label) +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(hjust = 0.5), # Center the title
      axis.text.x = element_text(angle = 30, hjust = 1), # Rotate x-axis labels
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = NA) # Ensure panel is also transparent
    )
  
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
    fig <- fig + theme(aspect.ratio = 1/4)
  }
  
  # Save the plot
  ggplot2::ggsave(fname,
                  plot = fig,
                  width = 6, # Fixed width from example
                  height = 2, # Fixed height from example for non-faceted plot
                  bg = "transparent") # Ensure transparent background for saved file
  
  message(paste0("Plot saved as '", fname, "'"))
  return(short_name)
}

# Example of how to use this function with your haddock data:
# (Uncomment the following lines to run)

# haddock_data_raw <- read.csv("haddock_eat_herring_eggs_index.csv")
#
# # Prepare the data: rename columns to 'Year' and 'Estimate_Value'
# # to match the function's expectations.
# haddock_prepared_data <- haddock_data_raw %>%
#   rename(Year = YEAR, Estimate_Value = est) %>%
#   select(Year, Estimate_Value) # Selects only the required columns for plotting
#
# # Call the plotting function
# haddock_predation_index(haddock_prepared_data,
#                         plot_title = "", # Now defaults to no title, or you can explicitly set it to ""
#                         y_label = "Predation Index (Estimate)",
#                         x_axis_breaks = c(1960, 1970, 1980, 1990, 2000),
#                         x_axis_limits = c(1962, 2003),
#                         img_dir = ".", # Saves to current working directory
#                         include_axis_titles = FALSE,
#                         set_aspect_ratio = TRUE)
