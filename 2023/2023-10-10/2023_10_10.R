# Title: Supernatural States
# Author: Greg Chism
# Date: 10/10/2022
# Description: Quantifying the number of hauntings in both states and cities in the U.S. using the 10-Oct-2023 #tidytuesday dataset

# Load required packages
if(!require(pacman))
  install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               USAboundaries,
               biscale,
               patchwork,
               RColorBrewer,
               here)

# Read in data
haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')
haunted_places |> glimpse()
crs_use <- "+proj=laea +lat_0=30 +lon_0=-95"

counts_city <- haunted_places |> 
  filter(state_abbrev != "AK" & state_abbrev != "HI") |>
  rename(long = city_longitude, lat = city_latitude) |>
  select(long, lat, city, state) |> 
  drop_na() |>
  group_by(city) |>
  summarise(count = n())

counts_state <- haunted_places |> 
  filter(state_abbrev != "AK" & state_abbrev != "HI") |>
  rename(long = city_longitude, lat = city_latitude) |>
  select(long, lat, city, state) |> 
  drop_na() |>
  group_by(state) |>
  summarise(count = as.numeric(n()))
haunted_places |> View()
coords <- haunted_places |> 
  filter(state_abbrev != "AK" & state_abbrev != "HI") |>
  rename(long = city_longitude, lat = city_latitude) |>
  select(long, lat, city, state) |> 
  left_join(counts_city) |>
  filter(count > 10) |>
  drop_na() |>
  distinct() |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

# Build the map 
pal <- "DkViolet2"

usa_sf <- us_states(resolution = "low") %>% 
  dplyr::filter(!state_abbr %in% c("PR", "AK", "HI")) %>% 
  st_transform(crs = crs_use) |>
  rename(state = state_name) |>
  left_join(counts_state) 

ggplot(usa_sf, aes(fill = count)) +
  geom_sf() +
  geom_sf(data = coords, aes(size = count, alpha = 0.005), shape = 21, fill = "black") +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  scale_size_continuous(guide = guide_legend(override.aes = list(fill = "transparent", color = "white", stroke = 1))) + # Override the aesthetics
  scale_fill_gradient(low = "white", high = "#800000") +
  scale_shape_identity() +
  guides(alpha = "none",
         fill = guide_colourbar(barwidth = 14, barheight = 0.5, 
                                frame.colour = "black", ticks.colour = "black", ticks.linewidth = 0.75,
                                order = 2)) +
  labs(size = "Hauntings (city)",
       fill = "Hauntings (state)",
       title = "Supernatural States",
       subtitle = "Eastern U.S. is the most haunted, but CA takes the prize") +
  ggthemes::theme_map(base_family = "Supernatural Knight", base_size = 14) +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        text = element_text(color = "white"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_blank(),
        plot.title = element_text(size = 20))

ggsave(plot = last_plot(), here("2023", "2023-10-10", "hauntedMap.png"), height = 9, width = 13, units = "in", dpi = 400)


