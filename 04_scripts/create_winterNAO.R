
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
  summarise(Winter_NAO_Average = mean(NAO_Index, na.rm = TRUE))

# Save output
# need to update the year range
plot_winter_nao(winter_avg_nao, include_axis_titles = FALSE)
### Mark lines where outputs are saved ----

