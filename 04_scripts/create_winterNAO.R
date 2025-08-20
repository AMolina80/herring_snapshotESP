
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

# Downloaded monthly NAO from climate.gov
nao_data <- read.table("01_inputs/norm.nao.monthly.b5001.current.ascii.txt",
                       col.names = c("Year", "Month", "NAO_Index"))

# Analyses ----
# Calculate the winter average NAO index (Dec of previous year, Jan-Mar of current year)

# Create a 'winter_year' column to group the winter months correctly
# December (12) will be assigned to the *next* year for winter averaging.
nao_winter <- nao_data %>%
  mutate(Winter_Year = ifelse(Month == 12, Year + 1, Year)) %>%
  # Filter for the winter months: December (of previous year), January, February, March
  filter(Month %in% c(12, 1, 2, 3))

# Calculate the mean NAO index for each winter year
winter_avg_nao <- nao_winter %>%
  group_by(Winter_Year) %>%
  summarise(Winter_NAO = mean(NAO_Index, na.rm = TRUE))%>%
  filter(Winter_Year>=1983)%>%
  rename(Year=Winter_Year)

# Save output
plot_indicator_time_series(
  data = winter_avg_nao,
  value_col = "Winter_NAO", 
  plot_title = "",
  y_label = "",
  x_label = "",
  img_dir = "05_images",
  x_axis_limits = c(1980, 2025),set_aspect_ratio = 1/4
  )
