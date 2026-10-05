library(tidyverse)
library(janitor)
library(readxl)
library(here)

library(tsibble)
library(fable)
library(feasts)
library(ggtime)

## Comment these lines out if you're not me.
library(kjhmisc)
setup_socviz()

source(here("R", "read-data.R"))

births <- read_births(here(
  "data",
  "nz-births-monthly-VSB356801.xlsx"
))

population <- read_population(here(
  "data",
  "nz-est-quarterly-population.xlsx"
))

population_monthly <- interpolate_population_monthly(population)

birth_rates <- births |>
  inner_join(population_monthly, by = "date") |>
  mutate(
    days = days_in_month(date),
    birth_rate = births / population * 1000,
    # Daily average births scaled to a 365.25-day year, per 1,000
    birth_rate_annualized = births / days * 365.25 / population * 1000
  )

birth_rates_ts <- birth_rates |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month)

birth_rates_stl <- birth_rates_ts |>
  model(stl = STL(birth_rate_annualized)) |>
  components()

p_stl <- autoplot(birth_rates_stl)

ggsave(
  here("figures", "birth-rates-stl.png"),
  p_stl,
  width = 8,
  height = 6,
  dpi = 300
)

birth_rates_stl_log <- birth_rates_ts |>
  model(stl = STL(log(birth_rate_annualized))) |>
  components()

p_stl_log <- autoplot(birth_rates_stl_log)

ggsave(
  here("figures", "birth-rates-stl-log.png"),
  p_stl_log,
  width = 8,
  height = 6,
  dpi = 300
)

p_births <- births |>
  ggplot(aes(x = date, y = births)) +
  geom_line() +
  scale_x_date(expand = expansion(mult = 0.005)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = "Month",
    y = "Births",
    title = "Monthly births in New Zealand, 1980–2025, raw counts",
    caption = "Data: Statistics New Zealand."
  )

ggsave(
  here("figures", "births-monthly.png"),
  p_births,
  width = 20,
  height = 4,
  dpi = 300
)
