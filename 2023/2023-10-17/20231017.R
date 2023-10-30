# Title: Get on Taylor's Radar...
# Author: Greg Chism
# Date: 10/17/2022
# Description: Describing the album profiles of Taylor Swift's albums

# Load required packages
if(!require(pacman))
  install.packages("pacman")

p_load(colorRamp2,
       extrafont,
       ggalt,
       ggmosaic,
       ggpubr, 
       ggtext,
       here, 
       patchwork,
       PNWColors,
       RColorBrewer,
       scales,
       showtext, # To import Google fonts
       taylor,
       tidymodels,
       tidyverse)

# Import data
taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')


# Order of the x-axis for the main ggplot
order <- c("acousticness", 
           "instrumentalness", 
           "tempo", 
           "loudness",
           "valence",
           "energy",
           "danceability"
) 

# Data for the radial plots
all_trim <- taylor_album_songs |>
  mutate(
    danceability = danceability / (max(danceability, na.rm = TRUE) / 100),
    energy = energy / (max(energy, na.rm = TRUE) / 100),
    loudness = (loudness + 40) / (max(loudness + 40, na.rm = TRUE) / 100),
    speechiness = speechiness / (max(speechiness, na.rm = TRUE) / 100),
    acousticness = acousticness / (max(acousticness, na.rm = TRUE) / 100),
    valence = valence / (max(valence, na.rm = TRUE) / 100),
    tempo = tempo / (max(tempo, na.rm = TRUE) / 100),
    liveness = liveness / (max(liveness, na.rm = TRUE) / 100),
    instrumentalness = instrumentalness / (max(instrumentalness, na.rm = TRUE) / 100),
    album_name = case_when(
      album_name == "Fearless (Taylor's Version)" ~ "Fearless",
      album_name == "Red (Taylor's Version)" ~ "Red",
      TRUE ~ album_name
    )
  ) |>
  gather(key = metric, c(danceability,
                        energy,
                        # speechiness,
                        acousticness,
                        instrumentalness,
                        #liveness,
                        valence,
                        loudness,
                        tempo
  ),
  value = value) |>
  #  distinct(playlist_name)
  # filter(playlist_name %in% c("Groove")) %>%
  #arrange(metric) %>%
  arrange(factor(metric, levels = order)) |>
  select(-speechiness,-liveness)


# Main plot showing each album
main <- 
  all_trim |>
  ggplot(aes(x = metric, y = value, group = interaction(album_name, metric),fill = album_name, color = album_name)) +
  geom_polygon(aes(group = track_name), alpha = 0.01, linewidth = 0.2, fill = NA, show.legend = F) + 
  scale_x_discrete(limits = order,
                   labels = c("Acoustic", "Instrumental", "Fast", "Loud", "Happy", "Energetic", "Danceable")) +
  scale_fill_taylor_d(album = "Red") +
  scale_color_taylor_d(album = "Red") +
  coord_polar(clip = "off")+
  theme_minimal() +
  labs(title = "What's on Taylor's Radar?",
  subtitle = "Taylor Swift's albums appear pretty similar...") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family = "IM Fell DW Pica",
                                      hjust = 0.5,
                                      size = 26),
        plot.subtitle = element_markdown(family = "IM Fell DW Pica",
                                         size = 18,
                                         hjust = 0.5),
        axis.text.x = element_text(family = "IM Fell DW Pica",
                                   size = 16),
        #plot.background = element_rect(color="grey20",fill="transparent")
  ) +
  ylim(0, 100) +
  geom_polygon(data = . %>%  group_by(album_name, metric) %>% 
                 summarise_at(c("value"),mean) %>%
                 arrange(factor(metric, levels = order)) %>%
                 ungroup(),
               aes(x = metric, y = value, group = album_name, color = album_name, fill = album_name),
               alpha = 0.2, linewidth = 1.5, show.legend = F
  )

# Facet plot splitting the three artists
facet <- 
  all_trim |>
  ggplot(aes(x = metric, y = value, group = interaction(album_name, metric),
             fill = album_name,
             color = album_name)) +
  geom_polygon(aes(group = track_name), linewidth = 0.2, alpha = 0.03, show.legend = F)+ 
  scale_x_discrete(limits = order,
                   labels = c("Acoustic", "Instrumental", "Fast", "Loud", "Happy", "Energetic", "Danceable")) +
    scale_fill_taylor_d(album = "Red") +
    scale_color_taylor_d(album = "Red") +
    labs(subtitle = "... but each album has its own sound",
         caption = "Data: taylor package | Viz: Greg Chism") +
  coord_polar(clip = "off")+
  theme_minimal() +
  theme(strip.text = element_text(family = "IM Fell DW Pica",
                                  size = 12,
                                  hjust = 0.5),
        axis.title = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_markdown(family = "IM Fell DW Pica",
                                         size=18,
                                         hjust=.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_markdown(family = "IM Fell DW Pica",
                                        hjust = 0.5,
                                        size = 10))+
  ylim(0, 100) +
  facet_wrap(~ album_name
             , ncol = 3, nrow = 4
             ) 

# Final plot
(panel <- (main + facet) + plot_layout(ncol = 2))

# Save the plot
ggsave(here("2023", "2023-10-17", "20231017.png"), plot = panel,
       width = 13.333, height = 7.5, units = "in", dpi = 800, limitsize = F)
