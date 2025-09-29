# Metadata ---- 

### Project name: Atlantic herring Snapshot ESP
### Code purpose: Create, plot, and save out indicator time series. 
###               This includes all ecosystem and socioeconomic indicators used in the snapshot ESP

### Author: Adelle Molina
### Date started: 9/29/25

### Code reviewer:
### Date reviewed:

# libraries and functions
library(dplyr)
library(tidyverse)
# must have ecodata and NEesp2
# Make sure 'plt_herring_function.R' is in your working directory.
source(here::here("04_scripts/plt_herring_function.R"))

# Create ecosystem indicators ---------------------------------------------
# temperature duration indicator is updated in a separate script and saved out as a plot and intermediate data
# Fish Condition ----------------------------------------------------------
# Load in fish condition from ecodata and reformat for NEesp2 plotting syntax
condition_data <- ecodata::condition %>%
  dplyr::rename(YEAR=Time,
                Species=Var,
                DATA_VALUE=Value)

# Plot and save out
plt <- NEesp2::plot_condition(data=condition_data, var = "Atlantic herring")

plt +
  ggplot2::geom_point(ggplot2::aes(shape = EPU), size = 2) +
  ggplot2::guides(
    shape = ggplot2::guide_legend(title = "EPU"),
    color = ggplot2::guide_legend(ncol = 1) # Force color legend to be a single column to stack
  ) +
  ggplot2::theme(
    aspect.ratio = 9/16,
    legend.title = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 16),
    legend.background = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12), # Make legend text smaller
    legend.box = "vertical", # Stack legends vertically
    legend.spacing.y = ggplot2::unit(0.000001, "cm"), # Adjust this value to reduce space
    plot.background = ggplot2::element_rect(color = "black",
                                            linewidth = 2))+
  ggplot2::ylab("Relative condition") +
  ggplot2::scale_y_continuous(breaks = seq(0.9, 1.05, by = 0.05),
                              limits = c(0.88, 1.05))

ggplot2::ggsave(here::here("05_images/herring_condition_final.png"),
                width = 9, height = 5,dpi = 300, units = "in",
                bg = "transparent") # Ensure transparent background for saved file

# create data frame to combine with others
herring_condition_data <- ecodata::condition %>%
  dplyr::filter(Var == "Atlantic herring")%>%
  dplyr::rename(Year=Time,
                MeanCond=Value)

# NAO ----------------------------------------------------------

# Downloaded monthly NAO from climate.gov, read in here
nao_data <- read.table(
  "01_inputs/norm.nao.monthly.b5001.current.ascii.txt",
  col.names = c("Year", "Month", "NAO_Index"))

# Calculate the winter average NAO index (Dec of previous year, Jan-Mar of current year)
# December (12) will be assigned to the *next* year for winter averaging.
nao_winter <- nao_data %>%
  mutate(Winter_Year = ifelse(Month == 12, Year + 1, Year)) %>% # Create a 'winter_year' column to regroup winter months

  # Filter for the winter months: December (of previous year), January, February, March
  filter(Month %in% c(12, 1, 2, 3))

# Calculate the mean NAO index for each winter year
winter_avg_nao <- nao_winter %>%
  group_by(Winter_Year) %>%
  summarise(Winter_NAO = mean(NAO_Index, na.rm = TRUE)) %>%
  filter(Winter_Year >= 1983) %>%
  rename(Year = Winter_Year)

# update format to work with plt_herring function and save plot
winter_avg_nao |>
  dplyr::mutate(INDICATOR_NAME = "winter_nao") |>
  dplyr::rename(YEAR = Year, DATA_VALUE = Winter_NAO) |>
  plt_herring(
    img_dir = here::here("05_images"),
    ind_name = "winter_nao",
    years = 1980:2025
  )

# Haddock Predation index ----------------------------------------------------------

# Data provided by Micah dean (MADMF)
haddock_predation_index <- read.csv(
  '01_inputs/haddock_eat_herring_eggs_index.csv'
) %>%
  dplyr::rename(Year = YEAR, haddock_pred = log_est) %>% # rename to be more informative, use the logged index value
  filter(Year >= 1983)

# Plot and save output
haddock_predation_index |>
  dplyr::mutate(INDICATOR_NAME = "haddock_predation_index") |>
  dplyr::rename(YEAR = Year, DATA_VALUE = haddock_pred) |>
  plt_herring(
    img_dir = here::here("05_images"),
    ind_name = "haddock_predation_index",
    years = 1980:2025
  )



# Bring in socioeconomic indicators --------------------------------------------
# Load and clean the data
herr_comm_dat <- read.csv(here::here("01_inputs/SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL.csv"), header = T) %>%
  dplyr::mutate(
    INDICATOR_NAME = dplyr::case_when(
      str_detect(INDICATOR_NAME, "Commercial_HERRING_Landings_LBS") ~ "Commercial_Landings",
      str_detect(INDICATOR_NAME, "TOTALANNUALREV_HERRING_2024Dols") ~ "Total_Commercial_Revenue",
      str_detect(INDICATOR_NAME, "AVGVESREVperYr_HERRING_2024_DOLlb") ~ "Vessel_Revenue_per_Year",
      str_detect(INDICATOR_NAME, "N_Commercial_Vessels_Landing_HERRING") ~ "Active_Vessels",
      str_detect(INDICATOR_NAME, "AVGPRICE_HERRING_2024_DOLlb") ~ "Price_per_Pound",
      str_detect(INDICATOR_NAME, "AVGANNUAL_DIESEL_PRICE2024dols") ~ "Fuel_Price",
      TRUE ~ INDICATOR_NAME # Keep original if no match
    )
  )

# Get the new, clean list of unique indicator names
unique_indicators_clean <- unique(herr_comm_dat$INDICATOR_NAME)

# Loop through the new indicator names and save out the plots
for (ind_name in unique_indicators_clean) {
  # Create a plot using the clean data. This assumes plt_herring is accessible.
  plot_path <- plt_herring(
    data = herr_comm_dat,
    ind_name = ind_name,
    img_dir = getwd(),
    years = 1989:2025,
    show_plot = F
  )
  # Check for millions filename as a fallback
  plot_path_millions <- here::here("05_images", paste0(ind_name, "_millions_", Sys.Date(), ".png"))
  
  if (file.exists(plot_path)) {
    knitr::include_graphics(plot_path)
  } else if (file.exists(plot_path_millions)) {
    knitr::include_graphics(plot_path_millions)
  } else {
    cat("Error: Plot file not found.")
  }
  
}

# Combine ecosystem & socioeconomic indicators --------------------------------------------

# Pivot herring condition data from long to wide format so each EPU is its own variable
herring_condition_wide <- herring_condition_data %>%
  tidyr::pivot_wider(
    id_cols = Year,
    names_from = EPU,
    values_from = MeanCond,
    names_prefix = "condition_"
  ) %>%
  dplyr::select(Year, dplyr::starts_with("condition_"))

# Combine ecosystem indicators
# Oops need to read back in the updated duration indicator
opt_duration_indicator <- readRDS(here::here('02_intermediates/opt_duration_indicator_2025-09-29.rds'))

ecosystem_indicators <- herring_condition_wide %>%
  dplyr::full_join(winter_avg_nao, by = "Year") %>%
  dplyr::full_join(opt_duration_indicator, by = "Year") %>%
  dplyr::left_join(haddock_predation_index, by = "Year") %>%
  dplyr::select(Year, dplyr::starts_with("condition_"), Winter_NAO, haddock_pred, duration, start.day)%>%
  dplyr::rename(Winter_NAO = Winter_NAO, Haddock_Predation = haddock_pred, 
                Optimal_temp_duration= duration, Optimal_temp_start=start.day)

# Pivot the socioeconomic data from long to wide format
socioeconomic_indicators_wide <- herr_comm_dat %>%
  dplyr::select(YEAR, INDICATOR_NAME, DATA_VALUE) %>%
  dplyr::rename(Year=YEAR) %>%
  tidyr::pivot_wider(
    names_from = INDICATOR_NAME,
    values_from = DATA_VALUE
  )

# Combine all indicators into a single data frame
all_indicators <- ecosystem_indicators %>%
  dplyr::full_join(socioeconomic_indicators_wide, by = "Year")

# Export combined data as RData and CSV
save(all_indicators, file = here::here("03_outputs/all_herring_snapshot_indicators.rda"))
write.csv(all_indicators, file = here::here("03_outputs/all_herring_snapshot_indicators.csv"), row.names = FALSE)
