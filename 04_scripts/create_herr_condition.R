# Load necessary libraries
library(ecodata)
library(dplyr)
library(ggplot2)

# Assuming ecodata::condition is a data frame, filter it for "Atlantic herring"
# and the desired region if needed.
# Let's assume the 'New England' region is what you want.
herring_condition_data <- ecodata::condition %>%
  dplyr::filter(Var == "Atlantic herring")%>%
  dplyr::rename(Year=Time,
                MeanCond=Value)

# reformat for using NEesp2 plot fx 
condition_data <- ecodata::condition %>%
  dplyr::rename(YEAR=Time,
                Species=Var,
                DATA_VALUE=Value)


# Using NEesp2 function - use this---------------------------------------------------
plt <- NEesp2::plot_condition(data=condition_data, var = "Atlantic herring")

plt +
  ggplot2::geom_point(ggplot2::aes(shape = EPU), size = 2) +
  ggplot2::guides(
    shape = ggplot2::guide_legend(title = "EPU"),
    color = ggplot2::guide_legend(ncol = 1) # Force color legend to be a single column to stack
  ) +
  ggplot2::theme(
    #aspect.ratio = 1/4,
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

ggplot2::ggsave(here::here("05_images/new_condition.png"),
                  #plot = fig,
                  #width = 16, # Fixed width from previous examples
                  #height = 9, # Fixed height from previous examples for non-faceted plot
                  bg = "transparent") # Ensure transparent background for saved file

# Using my function -------------------------------------------------------

# Now, call your generic plotting function with the filtered data.
# The `scaled_condition` column is the one to be plotted.
plot_indicator_time_series(
  data = herring_condition_data,
  facet_by = "EPU",
  value_col = "MeanCond",
  plot_title = "Scaled Condition of Atlantic Herring",
  x_label = "Year",
  y_label = "Scaled Condition",
  x_axis_breaks = seq(min(herring_condition_data$Year), max(herring_condition_data$Year), by = 5),
  set_aspect_ratio = 9/16,
  line_color = "steelblue",
  point_color = "darkblue",
  hline_color = "darkgreen",
  img_dir = "05_images",
)

# but this i guess picks an EPU...or plots all of them

# Using ggplot ------------------------------------------------------------


# Just use GGplot
ggplot()

ggplot(herring_condition_data, aes(x = Year, y = MeanCond, col = EPU))  + 
  theme_bw() +  
  geom_point(na.rm=T, size=1.5)+
  geom_line(na.rm = T, linewidth = 1)+
  scale_x_continuous(breaks = seq(0,2022,5),minor_breaks = seq(0,2022,1))+
  xlab("Year") +
  ylab("Relative Condition")

