# Atlantic herring snapshot ESP

This repository is a clone of the template for creating R projects for each stock-specific ESP.
This repository is used to create the Atlantic herring snapshot ESP (the repository for the original ESP is elsewhere)

The repository contains the following folders:

### 01_inputs

Indicator time series from working group members
  Haddock predation index
  Optimal Duration
Monthly NAO data
Shapefiles
Socioeconomic/Commercial data

### 02_intermediates

Updated indicator time series data...only duration was updated at this time

### 03_outputs

Combined CSV of all data used in the snapshot

### 04_scripts

There are individual scripts to create the various indicators....don't use those
Use the script called "" to create and save plots and indicator time series
Function script for the plotting - use plt_herring_function

### 05_images

Images to use in the snapshot ESP

### 06_docs

Contains files to create snapshot ESP


#### Legal Disclaimer

*This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.*
