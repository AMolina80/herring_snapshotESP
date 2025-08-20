
# Metadata ----

### Project name:
### Code purpose:

### Author:
### Date started:

### Code reviewer:
### Date reviewed:

# Libraries & functions ----
library(dplyr)
library(ggplot2)

# Data ----

# Provided by Micah dean (MADMF)
haddock_predation_index <-  read.csv('01_inputs/haddock_eat_herring_eggs_index.csv') %>%
  dplyr::rename(Year=YEAR,
                haddock_pred=log_est) %>% # rename to be more informative, use the logged index value
  filter(Year>=1983)

# Analyses ----
# Plot and save output
plot_indicator_time_series(
  data = haddock_predation_index,
  value_col = "haddock_pred", 
  plot_title = "",
  y_label = "",
  x_label = "",
  img_dir = "05_images",
  x_axis_limits = c(1980, 2025),set_aspect_ratio = 1/4)

### Mark lines where outputs are saved ----

