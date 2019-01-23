
library(tidyverse)

df <- read_csv("data/fixations_on_surface_cela_plocha_1548242386.2310758.csv")
df <- df %>% filter(on_srf == "True")
df %>% ggplot(aes(x = norm_pos_x, y = norm_pos_y)) + geom_point()

df %>% ggplot(aes(x = x_scaled, y = y_scaled)) + geom_path()

mean(df$duration)

df_events <- read_csv("data/surface_events.csv")
df_events %>% ggplot(aes(y = surface_name, x = frame_number)) + geom_point()
