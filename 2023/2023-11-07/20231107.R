
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(tigris)
library(geofacet)
library(scales)
library(pracma)
library(NISTunits)
library(tidycensus)
library(rmapshaper)
library(waffle)
library(here)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-07")
house <- tuesdata$house

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "gray95"
text_col <- "#202A44"
blue_col <- "#0015BC"
red_col <- "#C41E3A"

body_font <- "roboto"
title_font <- "roboto"

# Data wrangling ----------------------------------------------------------

house_clean <- house |> 
  select(year, state_po, stage, party, candidatevotes) |>
  mutate(party = case_when(
    party == "DEMOCRAT" ~ "Democrat",
    party == "REPUBLICAN" ~ "Republican",
    TRUE ~ "Other"
  )) |>
  filter(stage == "GEN", 
         year == 2022) |>
  group_by(year, state_po, party) |>
  summarise(sumVotes = sum(candidatevotes), .groups = "drop") |>
  group_by(state_po) |>
  mutate(total_state_votes = sum(sumVotes)) |>
  ungroup() |>
  mutate(proportion = sumVotes / total_state_votes) |>
  mutate(party = fct_reorder(party, proportion, .fun = sum, .desc = TRUE))

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-07", "recording"),
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

st <- glue("Bars show the proportion of votes for <span style='color:{blue_col};'>Democrat</span>, <span style='color:{red_col};'>Republican</span>, and 
           <span style='color:#7f7f7f;'>Other</span><br>parties in general elections for 2022.<br><br>")

cap <- glue("Data: U.S. House 1976–2022 | Viz: Greg Chism"
)

title <- "US House Election Results for 2022"


# Plot --------------------------------------------------------------------

ggplot(house_clean, aes(x = party, y = proportion, fill = party)) +
  geom_col(aes(fill = party), size = 0.3, width = 0.5) +  # Adding a border for clarity
  facet_geo(~ state_po, grid = "us_state_grid2") +
  theme_void(base_size = 14, base_family = "Roboto") +
  scale_fill_manual(
    values = c("Democrat" = blue_col, "Republican" = red_col, "Other" = "gray50")
  ) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), unit = "cm"),
        plot.title = element_markdown(vjust = 5),
        plot.subtitle = element_markdown(vjust = 1),
        plot.caption = element_markdown(),
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col))

ggsave(plot = last_plot(), here("2023", "2023-11-07", "20231107.png"), height = 9, width = 10, units = "in")


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-07", paste0("20231107", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
