
plt_herring <- function(
    data,
    ind_name, # indicator name to filter by, will also be part of the file name
    new_breaks = NA,
    img_dir,
    years = 1989:2025,
    show_plot = FALSE
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
      img_dir,
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
  
  if (show_plot) {
    print(fig)
  }
  
  return(short_name)
}
