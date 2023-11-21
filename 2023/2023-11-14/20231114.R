
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(janitor)
library(hrbrthemes)
library(scales)
library(here)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-14")
diwali <- tuesdata$diwali_sales_data |>
  clean_names()

# Load fonts --------------------------------------------------------------

font_add_google("Dancing Script", "Dancing Script")
font_add_google("Merriweather", "Merriweather")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "gray98"
text_col <- ""
highlight_col <- ""

body_font <- "Merriweather"
title_font <- "Dancing Script"


# Data wrangling ----------------------------------------------------------

data <- diwali |>
  group_by(product_category) |>
  summarise(min = min(amount),
            max = max(amount),
            median = median(amount)) |>
  drop_na() |>
  mutate(product_category = fct_reorder(product_category, -median))
  
ggplot(data) +
  geom_segment(aes(x = product_category, xend = product_category, y = min, yend = max, group = product_category), color="grey") +
  geom_point(aes(x = product_category, y = min), color = "#FFD700", size = 3) +
  geom_point(aes(x = product_category, y = max), color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal(base_family = body_font, base_size = 30) +
  labs(title = title,
       caption = cap,
       x = "Category",
       y = "Amount") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(family = title_font, size = 60),
    plot.subtitle = element_markdown(size = 40, lineheight = 0.5),
    axis.title.x = element_markdown(size = 40, hjust = 1),
    axis.title.y = element_markdown(size = 40, hjust = 1),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_line(linewidth = 0.25)
  ) +
  scale_y_continuous(labels = label_dollar())

diwali |>
  ggplot(aes(x = amount, y = fct_reorder(product_category, amount), fill = fct_reorder(product_category, amount))) +
  geom_density_ridges() +
  theme(legend.position = "none")

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-14", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Diwali festival sales"
st <- ""
cap <- paste0(
  "Data: Saad Haroon, Kaggle | Viz: Greg Chism"
)


# Plot --------------------------------------------------------------------

ggplot(data) +
  geom_segment(aes(x = product_category, xend = product_category, y = min, yend = max, group = product_category), color="grey") +
  geom_point(aes(x = product_category, y = min), color = "#FFD700", size = 3) +
  geom_point(aes(x = product_category, y = max), color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal(base_family = body_font, base_size = 30) +
  labs(title = title,
       caption = cap,
       x = "Category",
       y = "Amount") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(family = title_font, size = 60),
    plot.subtitle = element_markdown(size = 40, lineheight = 0.5),
    axis.title.x = element_markdown(size = 40, hjust = 1),
    axis.title.y = element_markdown(size = 40, hjust = 1),
    panel.grid.major = element_line(linewidth = 0.25),
    panel.grid.minor = element_line(linewidth = 0.25),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  ) +
  scale_y_continuous(labels = label_dollar())

ggsave(plot = last_plot(), here("2023", "2023-11-14", "20231114.png"),
       height = 5, width = 7, units = "in", dpi = 300)

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-14", paste0("20231114", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
