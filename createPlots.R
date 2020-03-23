# USING CODE FROM:
#
# https://github.com/JonMinton/COVID-19 # @JonMinton
# https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b
# @christoph_sax
# https://github.com/gorkang/2020-corona/blob/master/2020-corona-plot.R#L18-L20

# Libraries
library.path <- .libPaths()

library(dplyr, lib.loc = library.path)
library(ggplot2, lib.loc = library.path)
library(ggrepel, lib.loc = library.path)
library(readr, lib.loc = library.path)
library(tidyr, lib.loc = library.path)
library(scales, lib.loc = library.path)

# Data Repo Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
url_c <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url_d <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
url_r <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

selection <- c(
               "Canada",
               "China",
               #"France",
               "Italy",
               "Iran",
               #"Japan",
               "Mexico",
#               "Germany",
#               "Spain",
               "Korea, South",
               "United Kingdom",
               "US"
)

dta_raw_c <- read_csv(url_c, col_types = cols()) %>% select(-Lat, -Long)

dta_c <- dta_raw_c %>%

  # tidy data
  rename(province = `Province/State`, country = `Country/Region`) %>%
  pivot_longer(c(-province, -country), "time") %>%
  mutate(time = as.Date(time, "%m/%d/%y")) %>%

  # rename some countries
  mutate(
    country = case_when(
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Hong Kong SAR"  ~ "Hong Kong",
      country == "Republic of Korea" ~ "South Korea",
      TRUE ~ country
    )) %>%

  # selection
  filter(country %in% !! selection) %>%

  # ignore provinces
  group_by(country, time) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%

  # calculate new infections
  arrange(time) %>%
  group_by(country) %>%
  mutate(diff = value - lag(value)) %>%
  ungroup() %>%
  filter(!is.na(diff)) %>%
  arrange(country, time)

DF_plot_c = dta_c %>%
  filter(value >= 1) %>%
  group_by(country) %>%
  mutate(days_after_1 = 0:(length(country)-1)) %>%

  # Create labels for last instance for each country
  group_by(country) %>%
  mutate(
    name_end =
      case_when(
        days_after_1 == max(days_after_1) - 3 ~ paste0(as.character(country), " (", max(days_after_1), " days)"),
        TRUE ~ "")
  )

dta_raw_d <- read_csv(url_d, col_types = cols()) %>% select(-Lat, -Long)

dta_d <- dta_raw_d %>%

  # tidy data
  rename(province = `Province/State`, country = `Country/Region`) %>%
  pivot_longer(c(-province, -country), "time") %>%
  mutate(time = as.Date(time, "%m/%d/%y")) %>%

  # rename some countries
  mutate(
    country = case_when(
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Hong Kong SAR"  ~ "Hong Kong",
      country == "Republic of Korea" ~ "South Korea",
      TRUE ~ country
    )) %>%

  # selection
  filter(country %in% !! selection) %>%

  # ignore provinces
  group_by(country, time) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%

  # calculate new infections
  arrange(time) %>%
  group_by(country) %>%
  mutate(diff = value - lag(value)) %>%
  ungroup() %>%
  filter(!is.na(diff)) %>%
  arrange(country, time)

DF_plot_d = dta_d %>%
  filter(value >= 1) %>%
  group_by(country) %>%
  mutate(days_after_1 = 0:(length(country)-1)) %>%

  # Create labels for last instance for each country
  group_by(country) %>%
  mutate(
    name_end =
      case_when(
        days_after_1 == max(days_after_1) - 3 ~ paste0(as.character(country), " after ", max(days_after_1), " days"),
        TRUE ~ "")
  )



# PLOT --------------------------------------------------------------------

plot1 = DF_plot_d %>%
  ggplot(aes(x = days_after_1, y = value, color = country)) +
  scale_colour_brewer(
    type = "qual",
    palette = "Paired"
  ) +
  geom_line() +
  ggrepel::geom_label_repel(
    aes(label = name_end),
    #direction = "y",
    #nudge_y = -0.25,
    #nudge_x = 0.25,
    #box.padding = 3,
    #point.padding = 5,
    #force = 3,
    show.legend = FALSE,
    #segment.color = "grey",
    segment.size  = .2,
    #arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_continuous(breaks = seq(0, max(DF_plot_d$value), 2)) +
  labs(
    title = "Confirmed Covid deaths",
    subtitle = "Days = # days since 1+ deaths (or since 20200122 if first death before then)",
    x = "Days after 1 confirmed death",
    y = "Confirmed deaths (log scale)",
    caption = paste0("Source: Johns Hopkins CSSE (fetched ", date(), " ", Sys.timezone(), ")")
  ) +
  theme_minimal() +
  theme(legend.position = "none")
plot1

plot2 = DF_plot_c %>%
  ggplot(aes(x = days_after_1, y = value, color = country)) +
  scale_colour_brewer(
    type = "qual",
    palette = "Paired"
  ) +
  geom_line() +
  ggrepel::geom_label_repel(
    aes(label = name_end),
    #direction = "y",
    #nudge_y = -0.25,
    #nudge_x = 0.25,
    #box.padding = 3,
    #point.padding = 5,
    #force = 3,
    show.legend = FALSE,
    #segment.color = "grey",
    segment.size  = .2,
    #arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_continuous(breaks = seq(0, max(DF_plot_c$value), 2)) +
  labs(
    title = "Confirmed Covid cases",
    subtitle = "Days = # days since 1+ confirmed cases (or since 20200122 if first case before then)",
    x = "Days after 1 confirmed cases",
    y = "Confirmed cases (log scale)",
    caption = paste0("Source: Johns Hopkins CSSE (fetched ", date(), " ", Sys.timezone(), ")")
  ) +
  theme_minimal() +
  theme(legend.position = "none")
plot2
