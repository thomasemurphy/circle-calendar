library(tidyverse)

setwd('../circle-calendar/')

my_year <- 2025

holidays <- read_csv(
  'holidays_2025.csv'
  ) %>%
  filter(
    !is.na(date)
  ) %>%
  mutate(
    date = paste(date, '2025 12:00:00')
    ) %>%
  mutate(
    date_dt = as_datetime(
      date,
      format = '%b %e %Y %H:%M:%S'
      )
  ) %>%
  filter(
    !(name %in% c(
      'Inauguration Day',
      'Presidents\' Day',
      'Columbus Day',
      'Veterans Day'
    ))
  ) %>%
  print(n = 20)

# shorten names
holidays[2, 'name'] <- 'MLK Day'
holidays[7, 'name'] <- 'Thanksgiving'
holidays[8, 'name'] <- 'Christmas'

inner_circle_radius <- 0.2

default_color <- '#d4c46d'
hot_color <- '#e2874c'
cold_color <- '#6cdda6'
day_text_color <- '#999999'
weekend_color <- default_color
weekday_color <- '#ffffff'
holiday_color <- '#6caaff'

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

n_days_in_year <- length(this_year_dt)

days_df <- data.frame(
  day_dt = this_year_dt
) %>%
  mutate(
    pct_around = (
      interval(year_start_ts, day_dt) %/% days(1)
      ) / n_days_in_year,
    angle = ifelse(
      pct_around <= 0.5,
      90 - pct_around * 180/0.5,
      90 - (pct_around - 0.5) * 180/0.5
    ),
    date_text = format(day_dt, '%a %e'),
    x_text = day_dt,
    y_text = 0.965,
    x_day_seg_start = this_year_dt - hours(12),
    x_day_seg_end = x_day_seg_start,
    y_day_seg_start = 0.94,
    y_day_seg_end = 0.99,
    wday = ifelse(
      wday(day_dt) %in% c(1,7),
      'weekend',
      ''
      ),
    y_holiday_text = 0.92
  ) %>%
  
  # add holidays
  left_join(
    holidays,
    by = c('day_dt' = 'date_dt')
  ) %>%
  
  # make one column for weekends and holidays
  mutate(
    weekend_holiday = ifelse(
      is.na(name),
      ifelse(
        wday == 'weekend',
        'weekend',
        'weekday'
        ),
      'holiday'
    )
  )

months_df <- data.frame(
  month_center_dt = seq(
    as_datetime('2025-01-15 12:00:00'),
    as_datetime('2025-12-15 12:00:00'),
    '1 month'
  )
) %>%
  mutate(
    x_start_line = ceiling_date(
      seq(
        year_start_ts,
        year_end_ts,
        by = "1 month"),
      "month"),
    y_start_line = inner_circle_radius,
    y_end_line = 1,
    x_end_line = x_start_line
  ) %>%
  mutate(
    month_label = format(month_center_dt, '%B'),
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
    month_and_temp_str =
      paste0(
        month_label,
        '\n',
        temp_string
      ),
    temp_color = (daily_high + daily_low) / 2
  )

g <- ggplot(
  data = days_df,
  mapping = aes(
    x = day_dt
  )
) +
  
  coord_radial(
    # rotate.angle = TRUE
    expand = FALSE
  ) +
  
  # day labels
  geom_text(
    mapping = aes(
      x = x_text,
      y = y_text,
      angle = angle,
      label = date_text
    ),
    size = 1.6,
    color = day_text_color,
    hjust = 0.5
  ) +
  
  # weekends and holidays
  geom_tile(
    aes(
      y = y_text,
      fill = weekend_holiday
    ),
    height = (
      days_df$y_day_seg_end[1] - 
      days_df$y_day_seg_start[1]
      ),
    alpha = 0.3
  ) +
  
  # holiday labels
  geom_text(
    aes(
      y = y_holiday_text,
      label = name,
      angle = angle
    ),
    size = 1.2,
    color = holiday_color
  ) +
  
  scale_fill_manual(
    values = c(
      holiday_color,
      weekday_color,
      default_color
      )
  ) +
  
  # months segments
  geom_segment(
    data = months_df,
    mapping = aes(
      x = x_start_line,
      xend = x_end_line,
      y = y_start_line,
      yend = y_end_line
    ),
    size = 0.1,
    color = default_color,
  ) +
  
  # month labels
  geom_text(
    data = months_df,
    mapping = aes(
      x = month_center_dt,
      y = y_mo,
      label = month_and_temp_str,
      color = temp_color
    ),
    size = 3,
    hjust = 0.5,
    vjust = 0.5
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
    color = '#bbbbbb',
  ) +
  
  # inner circle
  geom_hline(
    yintercept = inner_circle_radius,
    size = 0.1,
    color = default_color,
  ) +
  
  # format axes
  scale_x_datetime(
    name = '',
    limits = as_datetime(c(
      '2025-01-01 00:00:00',
      '2026-01-01 00:00:00'
    )),
    # expand = rep(0, 4)
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.02),
    expand = rep(0, 4)
  ) +
  
  # for monthly temperature highs and lows
  scale_color_gradient2(
    low = cold_color,
    mid = default_color,
    high = hot_color,
    midpoint = mean(months_df$temp_color)
  ) +
  
  # write year
  annotate(
    geom = 'text',
    x = year_start_ts,
    y = 0.08,
    label = my_year,
    size = 10,
    color = default_color,
  ) +
  
  # write city
  annotate(
    geom = 'text',
    x = as_datetime('2025-07-02'),
    y = .1,
    label = 'Oakland, California',
    size = 5,
    color = default_color,
    alpha = 0.8
  ) +
  
  # plot formatting
  theme_bw() + 
  theme(
    # plot.margin = unit(
    #   c(0, 0, 0, 0),
    #   unit = 'in'
    #   ),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'none'
  )

# save image
png(
  filename = 'circle_cal_oakland_2025_2x2_ft.png',
  width = 24,
  height = 24,
  units = 'in',
  res = 400
  )
print(g)
dev.off()

# png() works better
# ggsave(
#   filename = 'circle_cal_oakland_2025_2x2_ft.png',
#   width = 24,
#   height = 24,
#   units = 'in'
# )
