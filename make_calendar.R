library(tidyverse)

setwd('circle-calendar/')

# plot all days of year in a line

my_year <- 2025

year_start <- as_datetime(
  paste0(
    my_year,
    '-01-01 12:00:00'
    )
)

year_end <- as_datetime(
  paste0(
    my_year,
    '-12-31 12:00:00'
  )
)

this_year_dates <-
  seq(
    year_start,
    year_end,
    '1 day'
  )

n_days_in_year <- length(this_year_dates)

dates_df <- data.frame(
  date = this_year_dates
) %>%
  mutate(
    pct_around = (interval(year_start, date) %/% days(1)) / n_days_in_year,
    angle = ifelse(
      pct_around <= 0.5,
      90 - pct_around * 180/0.5,
      90 - (pct_around - 0.5) * 180/0.5
    ),
    date_text = format(date, '%b %e'),
    x_text = date,
    y_text = 0.95,
    x_day_seg_start = this_year_dates - hours(12),
    x_day_seg_end = this_year_dates - hours(12),
    y_day_seg_start = 0.92,
    y_day_seg_end = 1.01
  )

month_lines_df <- data.frame(
  x_start = ceiling_date(
    seq(
      year_start,
      year_end,
      by = "1 month"),
    "month"),
  y_start = 0.1,
  y_end = 1.02
) %>%
  mutate(
    x_end = x_start
  )


ggplot(
  data = dates_df,
  mapping = aes(
    x = date
  )
) +
  coord_polar() +
  geom_text(
    mapping = aes(
      x = x_text,
      y = y_text,
      angle = angle,
      label = date_text
    ),
    size = 0.8
  ) +
  
  # months segments
  geom_segment(
    data = month_lines_df,
    mapping = aes(
      x = x_start,
      xend = x_end,
      y = y_start,
      yend = y_end
    ),
    size = 0.1,
    color = '#aaaaaa'
  ) +
  
  # days segments
  geom_segment(
    mapping = aes(
      x = x_day_seg_start,
      xend = x_day_seg_end,
      y = y_day_seg_start,
      yend = y_day_seg_end
    ),
    size = 0.1,
    color = '#bbbbbb'
  ) +
  
  scale_x_datetime(
    name = '',
    limits = as_datetime(c(
      '2025-01-01 00:00:00',
      '2026-01-01 00:00:00'
    )),
    # breaks = '1 day',
    # date_labels = '%e'
  ) +
  scale_y_continuous(
    limits = c(0, 1.02)
  ) +
  theme_bw() + 
  theme(
    plot.margin = unit(c(-50,-50,-50,-50), unit='pt'),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
  )
