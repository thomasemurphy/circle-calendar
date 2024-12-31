library(tidyverse)

side_length_ft <- 2

side_length_in <- side_length_ft * 12

full_sheet_sq_in <- side_length_in ^ 2

min_marg_in <- 1

circle_diam_in <- side_length_in - 2 * min_marg_in

circle_rad_in <- circle_diam_in / 2

circle_circumference_in <- circle_diam_in * pi

n_days <- 365

in_per_day <- circle_circumference_in / n_days

in_per_day_in_16ths <- in_per_day * 16
