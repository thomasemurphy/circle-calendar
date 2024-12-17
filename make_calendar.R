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

dates_df <- data.frame(
  date = this_year_dates,
  var2 = rnorm(n = length(this_year_dates), mean = 0.5, sd = 0.01)
)

ggplot(
  data = dates_df,
  mapping = aes(
    x = date
  )
) +
  coord_polar() +
  scale_x_date(
    name = '',
    breaks = '1 day',
    date_labels = '%b %d'
  ) +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      angle = seq(
        90,
        -270,
        -360 / (length(this_year_dates))
      ),
      size = 3
    )
  )
