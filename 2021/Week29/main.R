library(tidyverse)
library(janitor)
library(patchwork)
library(extrafont)
library(cowplot)

# Load Data ---------------------------------------------------------------

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

glimpse(scoobydoo)



# Data Shaping ------------------------------------------------------------




ss_shaggy <- scoobydoo %>% 
  select(starts_with("caught"),
         starts_with("snack")) %>% 
  filter(. != "NULL") %>%
  select(-caught_other, - caught_not) %>% 
  select(ends_with("shaggy")) %>% 
  mutate_all(.funs = ~as.numeric(as.logical(.x))) %>%
  group_by(snack_shaggy) %>% 
  summarise(nombre_shaggy = n()) %>% 
  ungroup() %>% 
  rename(YN = snack_shaggy)
  
ss_fred <- scoobydoo %>% 
  select(starts_with("caught"),
         starts_with("snack")) %>% 
  filter(. != "NULL") %>%
  select(-caught_other, - caught_not) %>% 
  select(ends_with("fred")) %>% 
  mutate_all(.funs = ~as.numeric(as.logical(.x))) %>%
  group_by(snack_fred) %>% 
  summarise(nombre_fred = n()) %>% 
  ungroup() %>% 
  rename(YN = snack_fred)


ss_daphnie <- scoobydoo %>% 
  select(starts_with("caught"),
         starts_with("snack")) %>% 
  filter(. != "NULL") %>%
  select(-caught_other, - caught_not) %>% 
  select(ends_with("daphnie")) %>% 
  mutate_all(.funs = ~as.numeric(as.logical(.x))) %>%
  group_by(snack_daphnie) %>% 
  summarise(nombre_daphnie = n()) %>% 
  ungroup() %>% 
  rename(YN = snack_daphnie)


ss_velma <- scoobydoo %>% 
  select(starts_with("caught"),
         starts_with("snack")) %>% 
  filter(. != "NULL") %>%
  select(-caught_other, - caught_not) %>% 
  select(ends_with("velma")) %>% 
  mutate_all(.funs = ~as.numeric(as.logical(.x))) %>%
  group_by(snack_velma) %>% 
  summarise(nombre_velma = n()) %>% 
  ungroup() %>% 
  rename(YN = snack_velma)


ss_scooby <- scoobydoo %>% 
  select(starts_with("caught"),
         starts_with("snack")) %>% 
  filter(. != "NULL") %>%
  select(-caught_other, - caught_not) %>% 
  select(ends_with("scooby")) %>% 
  mutate_all(.funs = ~as.numeric(as.logical(.x))) %>%
  group_by(snack_scooby) %>% 
  summarise(nombre_scooby = n()) %>% 
  ungroup() %>% 
  rename(YN = snack_scooby)



ss <- inner_join(ss_scooby,
                 inner_join(ss_velma,
                           inner_join(ss_fred,
                                      inner_join(ss_shaggy, ss_daphnie, by =  "YN"),
                                      by = "YN"),
                           by = "YN"),
                 by = "YN")

noms <- str_sub(colnames(ss),start = 8, end = 15)[2:6]
noms <- c("YN",noms)
colnames(ss) <- noms

ss <- ss %>% 
  pivot_longer(cols = c(2:6), names_to = "Personnages", values_to = "Nombres") %>%
  mutate(Personnages = factor(Personnages)) %>% 
  group_by(YN) %>% 
  arrange(desc(Nombres))

temp1 = ss %>% 
  ungroup() %>% 
  filter(YN == 1) %>% 
  select(Personnages, Nombres) %>% 
  rename(N2 = Nombres) %>% 
  mutate(N2 = - N2)

temp2 = ss %>% 
  ungroup() %>% 
  filter(YN == 0) %>% 
  mutate(Nombres = exp(Nombres/100))
  

df = inner_join(temp2, temp1, by = "Personnages") %>% 
  select(-YN) %>% 
  mutate(Nombres = - Nombres,
         N2 = - N2)



# Plotting ----------------------------------------------------------------



plot1 <- ggplot(df, aes(x = Personnages)) +
  geom_col(aes(y = Nombres), fill = c('#a8883f','#5fb0d0','#93ba5c',"#e7b249","#ae93c0")) +
  geom_col(aes(y = N2), fill = c('#ad7901','#0099d7','#73c301',"#f0a000","#6b4291")) +
  scale_x_discrete(limits = c("daphnie", "velma", "shaggy", "fred", "scooby"),
                   labels = NULL,
                   breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(y = element_blank(),
       x = element_blank(),
       caption = "Vilains caught by our heroes with and without Scooby Snacks\nAuthors: Jean DUPIN & Camilia KASHI",
       title = "Scooby's the better to catch monsters...\nunless he's got Scooby Snacks !") +
  annotate("text", x = "daphnie", y = -1.5, label = "Daphnée", col = "black") +
  annotate("text", x = "velma", y = -1.5, label = "Véra", col = "black") +
  annotate("text", x = "shaggy", y = -1.5, label = "Samy", col = "black") +
  annotate("text", x = "fred", y = -1.5, label = "Fred", col = "black") +
  annotate("text", x = "scooby", y = -1.5, label = "Scooby", col = "black") +
  annotate("text", x = "fred", y = 35, label = "With Scooby Snacks", col = "#7b50a0", size = 5, fontface = "italic") +
  annotate("text", x = 1.5, y = -45, label = "Without Scooby Snacks", col = "#7b50a0", size = 5, fontface = "italic") +
  theme(panel.background = element_rect(fill = "#cfe13f"),
        plot.background = element_rect(fill = "#cfe13f"),
        plot.title = element_text(colour = "#7b50a0", size=20, hjust = 0.5),
        plot.caption = element_text(colour = "#7b50a0"))
  


png("Output.png", width = 700)
ggdraw() +
  draw_plot(plot1) +
  draw_image(image = "scoo2.png", x = 0.81, y = 0.17, width = 0.15)
dev.off()