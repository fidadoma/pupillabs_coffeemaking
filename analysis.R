
library(tidyverse)
theme_set(theme_bw(24))

df_celaplocha <- read_csv("data/fixations_on_surface_cela_plocha_1548242386.2310758.csv") %>% mutate(type = "cela_plocha")
df_kava <- read_csv("data/fixations_on_surface_kava_1548243103.0474198.csv") %>% mutate(type = "zkava")
df_mlynek <- read_csv("data/fixations_on_surface_mlynek_1548242682.182878.csv") %>% mutate(type = "mlynek")
df_odkladiste <- read_csv("data/fixations_on_surface_odkladiste_1548242458.9446976.csv") %>% mutate(type = "odkladiste")
df_pracovniplocha <- read_csv("data/fixations_on_surface_pracovni_plocha_1548242953.4903674.csv") %>% mutate(type = "pracovniplocha")

df <- rbind(df_celaplocha, df_kava, df_mlynek, df_odkladiste, df_pracovniplocha) %>% 
  filter(on_srf == "True") %>%                                        # nechceme mrkani a divani se mimo
  mutate(start_timestamp = start_timestamp-min(start_timestamp)) %>%  # normalizujeme cas
  arrange(start_timestamp, desc(type)) %>%                            # protoze cela plocha je nadrazene AOI vsem ostatnim, tak seradime, 
  group_by(id) %>%                                                    # seskupime stejne fixace
  top_n(1) %>%                                                        # a nechame jen to prvni (vzhledem k usporadani v abecede bude cela_plocha vzdy nize) 
  ungroup() %>%                                                       # a odskupime
  mutate(type = recode(type, zkava = "kava"))                         # prejmenujeme kavu

df %>% 
  ggplot(aes(x = type)) +
  stat_count() +
  theme(aspect.ratio = 1) +
  ggtitle("Počet fixací")

df %>% 
  ggplot(aes(x = start_timestamp, y = type, group = 1)) +
  geom_point(aes(size = duration)) +
  geom_path() +
  theme(aspect.ratio = 0.5)


df %>% 
  ggplot(aes(x = type, y = duration)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1.1) +
  theme(aspect.ratio = 1)


# finer fixations

df_celaplocha <- read_csv("data/finer_data/fixations_on_surface_cela_plocha_1548242386.2310758.csv") %>% mutate(type = "cela_plocha")
df_kava <- read_csv("data/finer_data/fixations_on_surface_kava_1548243103.0474198.csv") %>% mutate(type = "zkava")
df_mlynek <- read_csv("data/finer_data/fixations_on_surface_mlynek_1548242682.182878.csv") %>% mutate(type = "mlynek")
df_odkladiste <- read_csv("data/finer_data/fixations_on_surface_odkladiste_1548242458.9446976.csv") %>% mutate(type = "odkladiste")
df_pracovniplocha <- read_csv("data/finer_data/fixations_on_surface_pracovni_plocha_1548242953.4903674.csv") %>% mutate(type = "pracovniplocha")

df <- rbind(df_celaplocha, df_kava, df_mlynek, df_odkladiste, df_pracovniplocha) %>% 
  filter(on_srf == "True") %>% 
  mutate(start_timestamp = start_timestamp-min(start_timestamp)) %>% 
  select(-on_srf) %>% 
  arrange(start_timestamp, desc(type)) %>%    # protoze cela plocha je nadrazene AOI vsem ostatnim, tak seradime, 
  group_by(id) %>%                            # seskupime stejne fixace
  top_n(1) %>%                                # a nechame jen to prvni (vzhledem k usporadani v abecede bude cela_plocha vzdy nize) 
  ungroup() %>%                               # a odskupime
  mutate(type = recode(type, zkava = "kava")) # prejmenujeme kavu


df %>% 
  ggplot(aes(x = type, y = duration)) +
  stat_summary(fun.data = "mean_cl_boot", size = 1.1) +
  theme(aspect.ratio = 1)

df_celaplocha %>% 
  filter(on_srf == "True") %>% 
  ggplot(aes(x = norm_pos_x, y = norm_pos_y)) + 
  geom_point() +
  geom_density2d() + 
  theme(aspect.ratio = 1)

