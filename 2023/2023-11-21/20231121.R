
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(gchismtt)
library(ggtext)
library(glue)
library(ggside)
library(ggthemes)
library(ggpubr)
library(here)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-21")
rladies <- tuesdata$rladies_chapters
rladies |> 
  glimpse()

 # Load fonts --------------------------------------------------------------

font_add_google("Open Sans")
font_add_google("Roboto Condensed")
showtext_auto()

# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#88398a"
highlight_col <- "grey40"

body_font <- "Open Sans"
title_font <- "Roboto Condensed"


# Data wrangling ----------------------------------------------------------

chapt <- rladies |>
  group_by(chapter) |>
  summarise(min = min(year)) |>
  group_by(min) |>
  summarise(n = n()) |>
  mutate(cat = "numChaps") |>
  rename(year = min)

nums <- rladies |>
  group_by(year) |>
  summarise(n = n()) |>
  mutate(cat = "numEvents")

combined_data <- full_join(chapt, nums) 

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-21", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- gchismtt::social(
  icon_col = highlight_col,
  text_col = text_col,
  body_font = body_font
)


title <- "<span style='color:#88398a;'>R-Ladies</span> events and chapters"
st <- "The number of <span style='color:#88398a;'>R-Ladies</span> events correlates with the number of new chapters (<span style='color:#666666;'>top bars</span>)"

cap <- glue(
  "**Data**: Federica Gazzelloni | **Viz**: Greg Chism")


# Plot --------------------------------------------------------------------

plot <- combined_data |>
  filter(cat == "numEvents") |>
  ggplot(aes(year, n)) +
  geom_col(fill = "#88398a") +
  geom_xsidecol(data = combined_data |>
                  filter(cat == "numChaps"), 
                aes(year, n),
                fill = "#666666") +
  theme_pubclean(base_size = 35, base_family = body_font) +
  theme_ggside_void()  +
  scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020, 2022, 2024)) +
  labs(x = "Year", 
       y = "Number of events",
       title = title,
       subtitle = st,
       caption = cap) +
  theme(plot.title = element_markdown(family = title_font, size = 80),
        plot.subtitle = element_markdown(size = 35),
        plot.caption = element_markdown(),
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        axis.title = element_text(hjust = 1),
        panel.grid.major = element_line(linewidth = 0.5))

logo <- png::readPNG(here("2023", "2023-11-21", "images", "logo.png"), native = TRUE)

plot + 
  inset_element(logo,
                left = 0.9,
                bottom = 0.89,
                right = 1,
                top = 1.80,
                on_top = FALSE)

ggsave(plot = plot, here("2023", "2023-11-21", "20231121.png"),
       height = 5, width = 7, units = "in", dpi = 300)

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-21", paste0("20231121", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
