library(tidyverse)

setwd('circle-calendar/')

# plot all days of year in a line

my_year <- 2025

year_start <- as_date(
  paste0(
    my_year,
    '-01-01'
    )
)

year_end <- as_date(
  paste0(
    my_year + 1,
    '-01-01'
  )
)

this_year_dates <-
  seq(
    year_start,
    year_end,
    '1 day'
  )

n_days_in_year <- length(this_year_dates) - 1

dates_df <- data.frame(
  date = this_year_dates,
  var2 = rnorm(n = length(this_year_dates), mean = 0.5, sd = 0.01)
) %>%
  mutate(
    pct_around = (interval(year_start, date) %/% days(1)) / n_days_in_year,
    angle = ifelse(
      pct_around <= 0.5,
      90 - pct_around * 180/0.5,
      90 - (pct_around - 0.5) * 180/0.5
    )
  )

month_lines_df_1 <- data.frame(
  x_line = ceiling_date(
    seq(
      year_start,
      year_end,
      by = "1 month"),
    "month") - hours(12),
  y_line = 0
)

month_lines_df_2 <- data.frame(
  x_line = ceiling_date(
    seq(
      year_start,
      year_end,
      by = "1 month"),
    "month") - hours(12),
  y_line = 1
)

month_lines_df <- rbind(
  month_lines_df_1,
  month_lines_df_2
)

ggplot(
  data = dates_df,
  mapping = aes(
    x = date
  )
) +
  coord_polar() +
  geom_line(
    data = month_lines_df,
    mapping = aes(
      x = x_line,
      y = y_line,
      group = factor(x_line)
    )
  ) +
  scale_x_date(
    name = '',
    breaks = '1 day',
    date_labels = '%b %e'
  ) +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      angle = dates_df$angle,
      size = 3
    )
  )
