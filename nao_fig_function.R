# Load necessary libraries
# If you don't have them installed, run install.packages("dplyr") and install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#' Plot Winter Average NAO Index
#'
#' This function generates a ggplot2 visualization of the winter average NAO index,
#' styled to match the examples in 'fig_functions.R'.
#' It includes a line plot, points, and horizontal lines for the mean and +/- 1 standard deviation.
#'
#' @param data A data frame containing at least 'Winter_Year' and 'Winter_NAO_Average' columns.
#' @param x_label The label for the x-axis (default: "Winter Year").
#' @param y_label The label for the y-axis (default: "NAO Index Average").
#' @param x_axis_breaks A numeric vector specifying desired x-axis breaks (e.g., c(1990, 2000, 2010, 2020, 2024)).
#'                      If NA, default breaks will be used.
#' @param x_axis_limits A numeric vector of length 2 specifying the x-axis limits (e.g., c(1989, 2025)).
#'                      If NA, limits will be determined automatically by ggplot2.
#' @param img_dir The directory where the plot image should be saved (default: current working directory).
#' @param base_font_size The base font size for the plot theme (default: 16).
#' @param include_axis_titles Logical, whether to include X and Y axis titles (default: TRUE).
#' @param set_aspect_ratio Logical, whether to set a fixed aspect ratio for the plot (default: FALSE).
#'
#' @return The filename of the saved plot (if saved), otherwise prints the ggplot2 object.
#' @examples
#' # First, ensure 'winter_avg_nao' data is available:
#' # nao_data <- read.table("norm.nao.monthly.b5001.current.ascii.txt",
#' #                        col.names = c("Year", "Month", "NAO_Index"))
#' # nao_winter <- nao_data %>%
#' #   mutate(Winter_Year = ifelse(Month == 12, Year + 1, Year)) %>%
#' #   filter(Month %in% c(12, 1, 2, 3))
#' # winter_avg_nao <- nao_winter %>%
#' #   group_by(Winter_Year) %>%
#' #   summarise(Winter_NAO_Average = mean(NAO_Index, na.rm = TRUE)) %>%
#' #   filter(Winter_Year <= max(nao_data$Year))
#'
#' # Then, use the plotting function:
#' # plot_winter_nao(winter_avg_nao,
#' #                 x_axis_breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
#' #                 x_axis_limits = c(1949, 2025),
#' #                 img_dir = tempdir(), # Saves to a temporary directory
#' #                 include_axis_titles = FALSE,
#' #                 set_aspect_ratio = TRUE)
plot_winter_nao <- function(data,
                            x_label = "Winter Year",
                            y_label = "NAO Index Average",
                            x_axis_breaks = NA,
                            x_axis_limits = NA,
                            img_dir = ".", # Current working directory by default
                            base_font_size = 16,
                            include_axis_titles = TRUE,
                            set_aspect_ratio = FALSE) {
  
  # Calculate mean and standard deviation for reference lines
  mean_nao <- mean(data$Winter_NAO_Average, na.rm = TRUE)
  sd_nao <- sd(data$Winter_NAO_Average, na.rm = TRUE)
  
  # Generate a unique filename using the current date
  short_name <- paste0("winter_nao_", Sys.Date(), ".png")
  fname <- file.path(img_dir, short_name) # Use file.path for cross-platform compatibility
  
  # Create the ggplot object
  fig <- ggplot(data, aes(x = Winter_Year, y = Winter_NAO_Average)) +
    geom_hline(yintercept = mean_nao + sd_nao,
               color = "darkgreen",
               linetype = "solid",
               size = 0.8) +
    geom_hline(yintercept = mean_nao - sd_nao,
               color = "darkgreen",
               linetype = "solid",
               size = 0.8) +
    geom_hline(yintercept = mean_nao,
               color = "darkgreen",
               linetype = "dotted",
               size = 0.8) +
    geom_point() +
    geom_line() + # geom_path() is equivalent to geom_line() for this purpose
    labs(x = x_label,
         y = y_label) +
    theme_classic(base_size = base_font_size) +
    theme(
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
