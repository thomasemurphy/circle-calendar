library(tidyverse)
library(plotly)

setwd('circle-calendar/')

# plot all days of year in a line

my_year <- 2025

inner_circle_radius <- 0.2

typical_weather <- read_csv(
  'oakland_daily_high_lows.csv'
)

year_start_ts <- as_datetime(
  paste0(
    my_year,
    '-01-01 12:00:00'
    )
)

year_end_ts <- as_datetime(
  paste0(
    my_year,
    '-12-31 12:00:00'
  )
)

this_year_dt <-
  seq(
    year_start_ts,
    year_end_ts,
    '1 day'
  )

n_days_in_year <- length(this_year_dates)

dates_df <- data.frame(
  date = this_year_dates
) %>%
  mutate(
    pct_around = (
      interval(year_start, date) %/% days(1)
      ) / n_days_in_year,
    angle = ifelse(
      pct_around <= 0.5,
      90 - pct_around * 180/0.5,
      90 - (pct_around - 0.5) * 180/0.5
    ),
    date_text = format(date, '%e'),
    x_text = date,
    y_text = 0.965,
    x_day_seg_start = this_year_dates - hours(12),
    x_day_seg_end = this_year_dates - hours(12),
    y_day_seg_start = 0.94,
    y_day_seg_end = 0.99,
    wday = ifelse(
      wday(date) %in% c(1,7),
      'weekend',
      ''
      )
  )

month_lines_df <- data.frame(
  x_start = ceiling_date(
    seq(
      year_start,
      year_end,
      by = "1 month"),
    "month"),
  y_start = inner_circle_radius,
  y_end = 1
) %>%
  mutate(
    x_end = x_start
  )

month_labels_df <- data.frame(
  dt = seq(
    as_datetime('2025-01-15 12:00:00'),
    as_datetime('2025-12-15 12:00:00'),
    '1 month'
  )
) %>%
  mutate(
    label = format(dt, '%b'),
    y_mo = 0.8,
    y_temp = 0.6
  ) %>%
  mutate(
    daily_high = typical_weather$high,
    daily_low = typical_weather$low
  ) %>%
  mutate(
    temp_string = 
    paste0(
      daily_low,
      '\u00B0 - ',
      daily_high,
      '\u00B0F'
    ),
    temp_color = (daily_high + daily_low) / 2
  )

ggplot(
  data = dates_df,
  mapping = aes(
    x = date
  )
) +
  
  coord_polar() +
  
  # day labels
  geom_text(
    mapping = aes(
      x = x_text,
      y = y_text,
      angle = angle,
      label = date_text
    ),
    size = 0.8,
    color = '#999999',
    hjust = 0.5
  ) +
  
  # weekend days
  geom_tile(
    aes(
      x = date,
      y = y_text,
      fill = wday
    ),
    # alpha = 0.2,
    # width = 0.000001
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
  
  # month labels
  geom_text(
    data = month_labels_df,
    mapping = aes(
      x = dt,
      y = y_mo,
      label = label,
      color = temp_color
    ),
    size = 4
  ) +
  
  # temperature labels
  geom_text(
    data = month_labels_df,
    mapping = aes(
      x = dt,
      y = y_temp,
      label = temp_string,
      color = temp_color
    ),
    size = 2.5
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
  
  # inner circle
  geom_hline(
    yintercept = inner_circle_radius,
    size = 0.1,
    color = '#aaaaaa'
  ) +
  
  # format axes
  scale_x_datetime(
    name = '',
    limits = as_datetime(c(
      '2025-01-01 00:00:00',
      '2026-01-01 00:00:00'
    ))
  ) +
  
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  
  scale_color_gradient(
    low = '#55cae3',
    high = '#f87530'
  ) +
  
  annotate(
    geom = 'text',
    x = year_start,
    y = 0,
    label = '2025',
    size = 3.5,
    color = '#d4c46d'
  ) +
  
  annotate(
    geom = 'text',
    x = as_datetime('2025-07-02'),
    y = .1,
    label = 'Oakland',
    size = 2.5,
    color = '#1b880a',
    alpha = 0.8
  ) +
  
  # plot formatting
  theme_bw() + 
  theme(
    plot.margin = unit(
      c(-40, 0, -50, 0),
      unit='pt'
      ),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'none'
  )

