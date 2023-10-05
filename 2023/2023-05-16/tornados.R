# Title: Tornados data
# Author: Greg Chism
# Date: 2023-05-21
# Description: Modification of the plot by Georgios Karamanis: 
# https://github.com/gkaramanis/tidytuesday/tree/master/2023/2023-week_20

# All packages used in this script:
if(!require(pacman))
  install.packages("pacman")

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ComplexHeatmap")

pacman::p_load(tidyverse, 
               camcorder,
               cetcolor,
               ggrepel,
               spiralize,
               circlize,
               here)

# Read in and groom data
tornados <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

glimpse(tornados)

td <- tornados |>
  add_count(date) |>
  group_by(date) |>
  drop_na() |>
  reframe(
    date, mo, n,
    m = max(mag, na.rm = TRUE),
    m = ifelse(m == -Inf, NA, as.character(m))
  ) |>
  distinct()

dates <- data.frame(date = seq(min(td$date, na.rm = TRUE),
                               max(td$date, na.rm = TRUE), 1)) |>
  left_join(td) |>
  mutate(i = row_number())

sp <- data.frame(
  t = seq(0, 2 * pi * 73, length.out = 365 *  73) + 10.5 * pi
) %>% 
  mutate(
    x = -t * cos(t),
    y = t * sin(t),
    i = row_number()
  ) %>% 
  left_join(dates)

mo_l <- data.frame(
  i = c(month.abb, ""),
  t = seq(0, 2 * pi, length.out = 13) + 2 * pi/3
) %>% 
  mutate(
    x = - 550 * cos(t),
    y = 550 * sin(t)
  )

tn_max <- sp %>% 
  slice_max(order_by = n, n = 3)

f1 <- "Montserrat"
f2 <- "Raleway"
pal <- cetcolor::cet_pal(6, "l8")

p <- sp |>
ggplot(aes(x = x, y = y)) +
  geom_text_repel(data = tn_max, aes(x, y, label = paste0(n, " tornados\n(", format(date, "%b %d, %Y"), ")")), family = f1, nudge_x = c(200, 420, 200), nudge_y = c(-100, -100, 40),  alpha = 0.75, lineheight = 0.9, size = 3, segment.size = 0.4, color = "purple4") +
  geom_path(alpha = 0.05) +
  geom_point(aes(size = n, fill = m), alpha = 0.7, shape = 22, stroke = 0) +
  geom_text(data = mo_l, aes(x, y, label = i), family = f1, size = 4, color = "cornflowerblue") +
  scale_size_continuous(range = c(0.5, 5), guide = "none") +
  scale_fill_manual(values = pal, guide = guide_legend(title = "Highest magnitude\n(*F scale used until 2007, afterwards EF)", title.position = "top", label.position = "bottom", nrow = 1, override.aes = list(size = 8))) +
  scale_x_continuous(limits = c(-620, 620)) +
  coord_fixed() +
  labs(
    title = "US Tornadoes, 1950-2022",
    subtitle = str_wrap("The size of each point represents the number of tornadoes recorded per day, and its color the highest magnitude* for that day"),
    caption = "Source: Storm Prediction Center\nGraphic: Based on original by Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#fbf9f5", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(15, 0, 0, 0), family = f2),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, margin = margin(15, 0, 5, 0))
  )
  
ggsave(plot = last_plot(), "test.png")
