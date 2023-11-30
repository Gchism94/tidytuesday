
# Load packages -----------------------------------------------------------

if (!require(pacman))
  install.packages("pacman")

pacman::p_load(camcorder,
               extrafont,
               gchismtt,
               glue,
               ggtext,
               here,
               patchwork,
               scales,
               showtext,
               tidyverse
)

pacman::p_load_gh("AllanCameron/geomtextpath")

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-28")
episodes <- tuesdata$drwho_episodes
episodes |> glimpse()

# Load fonts --------------------------------------------------------------

font_import(here("/2023/2023-11-28/fonts/Futura_Bold.otf"), prompt = FALSE)
Futura <- font_import(here("2023", "2023-11-28", "fonts", "Futura_Regular.ttf"), prompt = FALSE)
Gallifreyan <- font_import(here("2023", "2023-11-28", "fonts", "ws_simple_gallifreyan.ttf"), prompt = FALSE)

# Define colours and fonts-------------------------------------------------

bg_col <- ""
text_col <- "#003b6f"
highlight_col <- ""

body_font = "Futura"
title_font = "Futura"


# Data wrangling ----------------------------------------------------------
data |> 
  group_by(doctor) |>
  summarise(max = max(uk_viewers))
data <- episodes |> 
  group_by(season_number) |>
  mutate(season_number = case_when(
    season_number == 4.14 | 
      season_number == 4.15 | 
      season_number == 4.16 | 
      season_number == 4.17 |
      season_number == 4.18 ~ 4,
    TRUE ~ season_number
    ),
    doctor = case_when(
    season_number == 1 ~ "Christopher Eccleston",
    season_number %in% 2:4 ~ "David Tennant",
    season_number == 2009 | season_number == 2010 ~ "David Tennant",  # For special episodes
    season_number %in% 5:7 ~ "Matt Smith",
    season_number %in% 8:10 ~ "Peter Capaldi",
    season_number %in% 11:13 ~ "Jodie Whittaker"
  ),
  color = case_when(
    doctor == "Christopher Eccleston" ~ "#000000",
    doctor == "David Tennant" ~ "#8B4513",
    doctor == "Matt Smith" ~ "#000080",
    doctor == "Peter Capaldi" ~ "#2F4F4F",
    TRUE ~ "#1E90FF"
  ),
  label = case_when(doctor == "Christopher Eccleston" ~ "<span style='color:#000000;font-size:10pt; font-family:Futura'>Christopher Eccleston</span>",
                    doctor == "David Tennant" ~ "<span style='color:#8B4513;font-size:10pt; font-family:Futura'>David Tennant</span>",
                    doctor == "Matt Smith" ~ "<span style='color:#000080;font-size:10pt; font-family:Futura'>Matt Smith</span>",
                    doctor == "Peter Capaldi" ~ "<span style='color:#2F4F4F;font-size:10pt; font-family:Futura'>Peter Capaldi</span>",
                    TRUE ~ "<span style='color:#1E90FF;font-size:10pt; font-family:Futura'>Jodie Whittaker</span>")
  )

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-28", "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- gchismtt::social(
  bg_col = bg_col,
  icon_col = highlight_col,
  text_col = text_col,
  body_font = body_font
)

title <- "Doctor Who Viewers" 
st <- "Doctor Who has had gradually fewer viewers over time<br>and peaked with <span style='color:#8B4513;'>David Tennant</span> at 13.3M (even more than the specials)"  # Add subtitle text here
cap <- "Data: datardis package | Viz: Greg Chism"

# Plot --------------------------------------------------------------------

data |> 
  ggplot() +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:4) * 3),
    color = "lightgrey"
  ) +
  geom_col(
    aes(x = season_number, 
        y = uk_viewers,
        fill = color),
    position = "dodge2",
    show.legend = TRUE,
    alpha = 0.9
  ) +
  geom_segment(
    aes(x = season_number,
        y = 0,
        xend = season_number,
        yend = 10),
    linetype = "dashed",
    color = "gray80"
  ) +
  labs(title = title,
       subtitle = st,
       y = "UK Viewers",
       caption = cap) +
  geom_textline(aes(x = season_number, y = max(uk_viewers) + 1, group = doctor, label = label),
                arrow = arrow(length = unit(3, "mm")),
                lineend = "round", 
                linejoin = "mitre",
                rich = TRUE) +
  scale_fill_identity() +
  scale_y_continuous(labels = unit_format(unit = "M")) +
  coord_polar() +
  theme_light(base_family = body_font, base_size = 14) +
  theme(plot.margin = margin(5, 5, 5, 5),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = text_col),
        panel.grid.major = element_blank(),
        plot.title = element_markdown(family = title_font, face = "bold", color = text_col),
        plot.subtitle = element_markdown(lineheight = 1.25),
        plot.caption = element_markdown(family = "WS Simple Gallifreyan", size = 5, color = text_col),
        plot.title.position = "plot")


ggsave(plot = last_plot(), here("2023", "2023-11-28", "20231121.png"),
       height = 7, width = 8, units = "in", dpi = 300)

# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(here("2023", "2023-11-28"), paste0("20231128", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
