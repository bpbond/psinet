
site <- "TMP"
year <- "2021"
month <- "09"

# Identify file to read
f <- list.files(file.path("~/v0-9/", paste(site, year, sep = "_")),
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

library(dplyr)
library(tidyr)
library(lubridate)

plots <- c("C" = "Control", "F" = "Freshwater", "S" = "Saltwater")

x %>%
  filter(grepl("soil_vwc", research_name), !is.na(value)) %>%
  mutate(TIMESTAMP = round(TIMESTAMP, "hours"),
         depth = if_else(research_name == "soil_vwc_30cm", "deep", "shallow")) %>%
  separate(design_link, into = c("what", "site", "plot", "which")) %>%
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
  out

write_csv(out, "out.csv")

