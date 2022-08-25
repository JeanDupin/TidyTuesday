## Tidytuesday - Week 33 ##

setwd(here::here("2022","Week33"))

# Packages & Données ----

load("donnees.RData")

library(tidyverse)
library(patchwork)


# Personnages ----

personnages <- characters |> 
  filter(uni_name == "How I Met Your Mother" | uni_name == "Friends")


# Personnalités ----


principaux_traits <- function(pers_id) {
  psych_stats |> 
    filter(char_id == pers_id) |> 
    arrange(desc(avg_rating)) |> 
    slice_head(n = 5) |> 
    select(char_name,
           uni_name,
           personality,
           avg_rating,
           rank)
}


personnalites <- map_dfr(levels(factor(personnages$id)), principaux_traits) |> 
  arrange(personality) |> 
  mutate(uni_name = factor(uni_name,
                           levels = c("Friends", "How I Met Your Mother")))


# Des personnalités qui reviennent et que l'on peut comparer
ls_traits <- personnalites |> 
  group_by(personality) |> 
  summarise(a = n()) |> 
  arrange(desc(a)) |> 
  filter(a > 1) |> 
  pull(personality)


# Des personnalités qui reviennent


# Plots pour comparer ----

create_plot <- function(trait) {
 personnalites |> 
    filter(personality == trait) |>
    arrange(desc(avg_rating)) |> 
    mutate(char_name = fct_inorder(factor(char_name))) |> 
    ggplot(aes(y = char_name,
               x = avg_rating,
               fill = uni_name)) +
    geom_col() +
    theme_minimal() +
    labs(title = trait)
}

plot_ls <- map(ls_traits, create_plot)


wrap_plots(plot_ls,
           ncol = 5, nrow = 2, byrow = T) 
