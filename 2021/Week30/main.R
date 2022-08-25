library(tidyverse)
library(maps)

# Data

states <- map_data("state")

drought <- readr::read_csv("drought.csv")

drought <- drought %>% 
  filter(state_abb %in% state.abb) %>% 
  mutate_at(.vars = "state_abb",
            .funs = function(x) state.name[match(x, state.abb)]) %>% # From abb. to names.
  mutate(state_abb = str_to_upper(state_abb)) 


Passage <- drought %>%
  mutate(start = lubridate::year(valid_start)) %>% 
  select(state_abb, start, drought_lvl, pop_total) %>%
  group_by(state_abb, start, drought_lvl) %>% 
  summarise(pop = sum(pop_total)) %>% 
  group_by(state_abb, start) %>% 
  mutate(pop= pop / sum(pop)) %>%
  filter(across(everything(), ~!is.na(.)))



Passage <- Passage %>% 
  group_by(state_abb, drought_lvl) %>% 
  mutate(issue = start*lm(pop ~ start)$coefficients[2] + lm(pop ~ start)$coefficients[1]) %>% 
  filter(start == min(start) | start == max(start))


df_min <- Passage %>% 
  filter(start == 2001)

df_max <- Passage %>% 
  filter(start != 2001) %>% 
  rename(coef = issue)

df <- full_join(df_max %>% 
                  select(state_abb,
                         drought_lvl,
                         coef),
                df_min %>% 
                  select(state_abb,
                         drought_lvl,
                         issue),
                by = c("state_abb", "drought_lvl"))


final <- df %>%
  mutate(variation = coef - issue) %>% 
  select(-coef, - issue) %>% 
  mutate(across(everything(), ~ifelse(is.nan(.),0,.))) %>% 
  mutate(variation = variation * 100)

## Graphs

palette <- rev(RColorBrewer::brewer.pal(9,"Spectral"))

# None

plot_test <- df %>% 
  filter(drought_lvl == "None")

states <- states %>% 
  mutate(region = str_to_upper(region))

carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")

carte <- arrange(carte, group, order)

p1 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colours = palette,
                       breaks =c(50, 25,0,-25,-50,-75),
                       labels = c("50 %",
                                  "25 %",
                                  "0 %",
                                  "-25 %",
                                  "-50 %",
                                  "-75 %")) +
  labs(x = element_blank(),
      y = element_blank(),
      fill = "Evolution of population\nconcerned by\nthis type of drought",
      title = "Evolution of people not concerned by drought - 2001-2021",
      caption = "TidyTuesday - Jean DUPIN") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# D0

plot_test <- df %>% 
  filter(drought_lvl == "D0")

states <- states %>% 
  mutate(region = str_to_upper(region))

carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")

carte <- arrange(carte, group, order)

p2 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colours = palette,
                       breaks =c(40,20,0,-20),
                       labels = c("40 %",
                                  "20 %",
                                  "0 %",
                                  "-25 %")) +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Evolution of population\nconcerned by\nthis type of drought",
       title = "Evolution of people concerned by an abnormally dry weather - 2001-2021",
       caption = "TidyTuesday - Jean DUPIN") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



# D1

plot_test <- df %>% 
  filter(drought_lvl == "D1")

states <- states %>% 
  mutate(region = str_to_upper(region))

carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")

carte <- arrange(carte, group, order)

p3 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colours = palette,
                       breaks =c(30,20,10,0,-10),
                       labels = c("30 %",
                                  "20 %",
                                  "10 %",
                                  "0 %",
                                  "-10 %")) +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Evolution of population\nconcerned by\nthis type of drought",
       title = "Evolution of people concerned by moderate drought - 2001-2021",
       caption = "TidyTuesday - Jean DUPIN") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# D2

plot_test <- df %>% 
  filter(drought_lvl == "D2")

states <- states %>% 
  mutate(region = str_to_upper(region))

carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")

carte <- arrange(carte, group, order)

p4 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colours = palette,
                       breaks =c(10,0,-5,-10,-15,-20),
                       labels = c("10 %",
                                  "0 %",
                                  "-5 %",
                                  "-10 %",
                                  "-15 %",
                                  "-20 %")) +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Evolution of population\nconcerned by\nthis type of drought",
       title = "Evolution of people concerned by severe drought - 2001-2021",
       caption = "TidyTuesday - Jean DUPIN") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


# D3

plot_test <- df %>% 
  filter(drought_lvl == "D3")

states <- states %>% 
  mutate(region = str_to_upper(region))

carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")

carte <- arrange(carte, group, order)

p5 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
  geom_polygon(colour = "black") +
  scale_fill_gradientn(colours = palette,
                       breaks =c(0,-5,-10),
                       labels = c("0 %",
                                  "-5 %",
                                  "-10 %")) +
  labs(x = element_blank(),
       y = element_blank(),
       fill = "Evolution of population\nconcerned by\nthis type of drought",
       title = "Evolution of people concerned by extreme drought - 2001-2021",
       caption = "TidyTuesday - Jean DUPIN") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


# D4 : Irrelevant

# plot_test <- df %>% 
#   filter(drought_lvl == "D4")
# 
# states <- states %>% 
#   mutate(region = str_to_upper(region))
# 
# carte <- merge(states, plot_test, by.x = "region", by.y = "state_abb")
# 
# carte <- arrange(carte, group, order)
# 
# p6 <- ggplot(carte, aes(x = long, y = lat, group = group, fill = variation)) +
#   geom_polygon(colour = "black") +
#   scale_fill_gradientn(colours = palette) +
#   labs(x = element_blank(),
#        y = element_blank(),
#        fill = "Evolution of population\nconcerned by\nthis type of drought",
#        title = "Evolution of people concerned by exceptional drought - 2001-2021",
#        caption = "TidyTuesday - Jean DUPIN") +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())



png(filename = "Cartes.png", width = 1024, height = 730)
p1 + p2 + p3 + p4 + p5 +
  patchwork::plot_layout(ncol = 2, nrow = 3)
dev.off()