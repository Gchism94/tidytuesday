
# Load packages -----------------------------------------------------------

if (!require(pacman))
  install.packages("pacman")

pacman::p_load(tidyverse,
               showtext,
               patchwork,
               camcorder,
               ggtext,
               glue,
               here,
               sysfonts,
               tidytext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-31")
horror_articles <- tuesdata$horror_articles
horror_articles |> glimpse()
# Load fonts --------------------------------------------------------------

font_add_google(name = "Syne Mono")
showtext_auto()
family <- "Syne Mono"
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "path-to-font/Font-Awesome-6-Brands-Regular-400.otf")
showtext::showtext_auto()

# Define colours and fonts-------------------------------------------------

bg_col <- "#00090D"
text_col <- "#F2F2F2"
highlight_col <- "#FF1515"
title_color <- "#FF1515"

body_font <- "Syne Mono"
title_font <- "Syne Mono"


# Data wrangling ----------------------------------------------------------

# Title words
snopes_horror_title <- horror_articles %>%
  filter(rating == "true" | rating == "false" | rating == "legend") %>%
  unnest_tokens(word, title) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(year = year(published), 
         month = month(published)) %>%
  count(word, month, year, rating) 

# Sentiment of titles
snopes_horror_title_s <- snopes_horror_title %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(month, year, sentiment, rating) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative) 

# Subtitle words
snopes_horror_subtitle <- horror_articles %>%
  filter(rating == "true" | rating == "false" | rating == "legend") %>%
  unnest_tokens(word, subtitle) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(year = year(published), 
         month = month(published)) %>%
  count(word, month, year, rating) 

# Sentiment of subtitles
snopes_horror_subtitle_s <- snopes_horror_subtitle %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(month, year, sentiment, rating) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative) 

# Claim words
snopes_horror_claim <- horror_articles %>%
  filter(rating == "true" | rating == "false" | rating == "legend") %>%
  unnest_tokens(word, claim) %>%
  filter(!is.na(word)) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(year = year(published), 
         month = month(published)) %>%
  count(word, month, year, rating) 

# Sentiment of claims
snopes_horror_claim_s <- snopes_horror_claim %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(month, year, sentiment, rating) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-31", "recording"),
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
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)


# Plot --------------------------------------------------------------------

ggplot(data = snopes_horror_claim_s,
       mapping = aes(x = year,
                     y = sentiment,
                     fill = sentiment > 0)) +
  geom_hline(mapping = aes(yintercept = 0),
             color = "#F2F2F2",
             size = 0.2) +
  geom_col() +
  facet_grid(rating ~ month,
             switch = "y") +
  scale_x_continuous(position = "top") +
  scale_fill_manual(values = c("#F22727", "white"), guide = "none") +
  labs(x = "Month",
       y = "Sentiment",
       title = str_to_upper("Snopes Horror Claims"),
       subtitle = "Sentiment of the text from horror claims that snopes considers",
       caption  = "Data: Snopes.com | Viz: Greg Chism") + 
  # coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 40, angle = 0, vjust = 1, hjust = 0,
                                    color = text_col, family = family),
        axis.title.x = element_text(size = 40, hjust = 0, 
                                    color = text_col, family = family),
        
        strip.background = element_blank(),
        strip.text.y.left = element_text(size = 40, angle = 0, vjust = 1,
                                         color = text_col, family = family),
        strip.text.x = element_text(size = 40, color = text_col, 
                                    family = family),
        
        panel.grid = element_blank(),
        
        plot.background = element_rect(fill = bg_col, color = NA),
        panel.background = element_rect(fill = bg_col, color = NA),
        
        plot.title.position = "plot",
        plot.title = element_text(size = 70, color = title_color,
                                  lineheight = 0.3, face = "bold", family = family),
        plot.subtitle = element_text(size = 40, color = text_col,
                                     lineheight = 0.3, face = "bold", family = family),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 30, color = text_col, 
                                    lineheight = 0.3, hjust = 1, family = family),
        
        plot.margin = margin(10, 10, 10, 10))


ggsave(plot = last_plot(), here("2023", "2023-10-31", "20231031.png"), height = 9, width = 13, units = "in", dpi = 400)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-31", paste0("20231031", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

