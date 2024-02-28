# Quick and dirty script to pull soil moisture and met data for MSM in 2022
# BBL February 2024

library(dplyr)
library(tidyr)

site <- "MSM"
year <- "2022"
LEVEL1_DATA_ROOT <- "~/v0-9/"

out <- list()
metout <- list()

# We need May-October

for(month in c("05", "06", "07", "08", "09", "10")) {
  message(month)
  # Identify file to read
  f <- list.files(file.path(LEVEL1_DATA_ROOT, paste(site, year, sep = "_")),
                  pattern = paste0(year, month, ".*csv$"),
                  full.names = TRUE)
  stopifnot(length(f) == 1)

  library(readr)
  x <- read_csv(f, col_types = cols(
    TIMESTAMP = col_datetime(format = ""),
    research_name = col_character(),
    ID = col_character(),
    design_link = col_character(),
    value = col_double(),
    F_OOB = col_double(),
    F_OOS = col_double()
  ))

  # Plot
  # Date YHYMMDD
  # Time HH:MM:SS
  # SWC shallow
  # sd shallow
  # n shallow
  # SWC deep
  # sd deep
  # n deep


  # Soil moisture data

  plots <- c("UP" = "Upland", "TR" = "Transition")
  x %>%
    filter(grepl("soil_vwc", research_name), !is.na(value)) %>%
    mutate(TIMESTAMP = round(TIMESTAMP, "hours"),
           depth = if_else(research_name == "soil_vwc_30cm", "deep", "shallow")) %>%
    separate(design_link, into = c("what", "which", "site", "plot"), sep = "-") %>%
    filter(plot %in% c("UP", "TR")) %>%
    group_by(TIMESTAMP, depth, plot) %>%
    summarise(mn = round(mean(value, na.rm = TRUE), 3),
              sd = round(sd(value, na.rm = TRUE), 3),
              n = n(),
              .groups = "drop") %>%
    pivot_wider(id_cols = c("TIMESTAMP", "plot"),
                names_from = "depth",
                values_from = c("mn", "sd", "n")) %>%
    mutate(time = format(TIMESTAMP, format = "%H:%M:%S"),
           date = format(TIMESTAMP, format = "%Y%m%d")) %>%
    select(plot, date, time, mn_shallow, sd_shallow, n_shallow, mn_deep, sd_deep, n_deep) %>%
    arrange(plot, date, time) %>%
    mutate(plot = plots[plot]) ->
    out[[month]]

  # Met data

  met <- c("wx_vappress15", "wx_maxws15", "wx_par_den15", "wx_tempavg15", "wx_rh15", "wx_rain15")
  x %>%
    filter(research_name %in% met, !is.na(value)) %>%
    mutate(TIMESTAMP = round(TIMESTAMP, "hours")) %>%
    group_by(TIMESTAMP, research_name) %>%
    summarise(mn = round(mean(value, na.rm = TRUE), 3),
              .groups = "drop") %>%
    pivot_wider(id_cols = c("TIMESTAMP"),
                names_from = "research_name",
                values_from = "mn") %>%
    mutate(time = format(TIMESTAMP, format = "%H:%M:%S"),
           date = format(TIMESTAMP, format = "%Y%m%d")) %>%
    select(date, time, wx_rain15, wx_rh15, wx_vappress15, wx_tempavg15, wx_par_den15, wx_maxws15) %>%
    # NOTE when pasting into the PSInet spreadsheet need to cut and paste
    # the windspeed columns over to the far right--
    arrange(date, time)  ->
    metout[[month]]

}

out <- bind_rows(out)
metout <- bind_rows(metout)

write_csv(out, "out.csv")
write_csv(metout, "metout.csv")
