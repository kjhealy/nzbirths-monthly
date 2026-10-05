read_births <- function(path) {
  read_statsnz(path, "births", "^\\d{4}M\\d{2}$") |>
    mutate(date = ym(period), .before = 1) |>
    select(-period)
}

read_population <- function(path) {
  read_statsnz(path, "population", "^\\d{4}Q\\d$") |>
    mutate(date = yq(period), .before = 1) |>
    select(-period)
}

# The quarterly figures are means over each quarter, so each one is
# anchored at the middle month of its quarter. Months in between are
# linearly interpolated; the first and last months of the series lie
# outside the anchors and are linearly extrapolated.
interpolate_population_monthly <- function(population) {
  anchors <- population |>
    mutate(date = date + months(1))

  tibble(
    date = seq(
      min(population$date),
      max(population$date) + months(2),
      by = "month"
    )
  ) |>
    left_join(anchors, by = "date") |>
    mutate(population = interpolate_linear(population))
}

# Stats NZ Infoshare exports have title rows above the data and table
# notes below. Keep only rows whose first column is a period label.
read_statsnz <- function(path, value_name, period_pattern) {
  read_excel(
    path,
    col_names = c("period", value_name),
    col_types = "text",
    na = c("", "..")
  ) |>
    filter(str_detect(period, period_pattern)) |>
    mutate(across(all_of(value_name), as.integer))
}

# Linear interpolation over evenly spaced values, with linear
# extrapolation of leading and trailing NAs.
interpolate_linear <- function(y) {
  x <- seq_along(y)
  known <- which(!is.na(y))
  first <- head(known, 2)
  last <- tail(known, 2)

  case_when(
    x < first[1] ~ line_through(x, first, y[first]),
    x > last[2] ~ line_through(x, last, y[last]),
    .default = approx(known, y[known], xout = x)$y
  )
}

line_through <- function(x, x0, y0) {
  y0[1] + (x - x0[1]) * diff(y0) / diff(x0)
}
