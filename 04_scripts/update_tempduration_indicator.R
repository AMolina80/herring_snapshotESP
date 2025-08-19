
# Metadata ----

### Project name: Herring Snapshot ESP
### Code purpose: Update optimal duration indicator with 2024 data

### Author: Adelle Molina
### Date started: 8/12/25

### Code reviewer:
### Date reviewed:
# Not sure what is going on but this isn't working right I don't think b/c raster names and time are different
# Libraries & functions ----
library(dplyr)
library(ggplot2)
library(ncdf4)
library(terra)
# Data ----

# Daily OISST for update year (in EDAB_Datasets)
nc_path <- "Z:/OISST/V2/SOURCE_DATA/SST/sst.day.mean.2024.nc"

# read in the CSV of the indicator timeseries
opt_duration_indicator <-  read.csv('01_inputs/Duration.Optimal.SST.Sept-Dec.csv')

#Set up shapefiles -------------------------------------------------------
bt_strata <- sf::st_read('01_inputs/Shapefiles/NES_BOTTOM_TRAWL_STRATA.shp')
her_strata_fall <- bt_strata %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs  ") %>% 
  dplyr::select(STRATUMA, geometry) %>%
  filter(STRATUMA %in% c('01050', '01060', '01070', '01080', '01090', '01100', '01110', '01120', '01130', 
                         '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210', '01220', 
                         '01230', '01240', '01250', '01260', '01270', '01280', '01290', '01300', '01360', 
                         '01370', '01380', '01390', '01400'))%>%
  dplyr::summarise(geometry = sf::st_union(geometry))

# make vector for the loops
strata.vec <- terra::vect(her_strata_fall)

# Analyses ----

# crop to the fall strata and fall dates 
data.rast <- terra::rast(nc_path)
data.rast <- terra::rotate(data.rast)
data.crop <- terra::crop(terra::mask(data.rast,strata.vec, touches = T),strata.vec)
data.crop <- data.crop[[time(data.crop) >= "2024-09-01"]] #  Start on 1st-september through end of december
  # this has mean daily temp (also min and max) for all cells in fall strata from sept-dec
  # the layers are called sst_dayofyear and each one has temp values for each cell in the shapefile
  
# do calculations 
  # 1st number of days above limit
  # then also figure out start date of suitable temps for larvae: what is the first day (which doy) when lethal area=0
  
data.crop [data.crop  < 21] <- NA # # select only pixels with temp > 21 # keep pixels with lethal temps, remove optimal habitat
names(data.crop) <- lubridate::yday(time(data.crop)) # rename the layers to be just day of year
hot.area <- terra::expanse(data.crop, "km", usenames=T) # area of lethal blob in each day, 
  
  # Which day is the first with 0 area or first day of suitable fall temperatures
result <- hot.area %>%
  dplyr::summarize(start.day = first(layer[area==0]), # this is the start of suitable temps in the "fall"  
                   duration=sum(area==0))%>%# this marks the duration of suitable habitat
  mutate(Year=2024) 

# combine with indicator timeseries
opt_duration_indicator <-opt_duration_indicator%>%
  rename(Year=year)%>%
  select(!blob.end)%>%
  rbind(result)

# add snippet of code to save out csv

# Save the plot
plot_indicator_time_series(
  data = opt_duration_indicator,
  value_col = "duration", 
  plot_title = "",
  y_label = "",
  x_label = "",
  img_dir = "05_images",
  x_axis_limits = c(1980, 2025),set_aspect_ratio = 3/8
  )

### Mark lines where outputs are saved ----
