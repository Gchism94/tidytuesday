
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytext)
library(patchwork)
library(camcorder)
library(ggtext)
library(ggwordcloud)
library(glue)
library(here)
library(imager)
library(wordcloud2)
library(webshot2)
library(htmlwidgets)
library(extrafont)
webshot::install_phantomjs()
font_import()
# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2020-09-08")
friends <- tuesdata$friends
friends |> glimpse()

# Load fonts --------------------------------------------------------------

font_add_google(name = "Rock Salt")
showtext_auto()

# Define colours and fonts-------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

body_font <- "Gabriel Weiss' Friends Font"
title_font <- "Gabriel Weiss' Friends Font"

pal <- c("#ff5238", "#ffdc00", "#42A2D6", "#9A0006", "#FFF580", "#00009E")

# Prepare image mask ------------------------------------------------------

# Load the image
img <- load.image("./2020/2020-09-08/images/chandler.png")

# Binarize the image
threshold_value <- 0.5  # adjust based on your image for best results
img_binary <- img
img_binary[img > threshold_value] <- 1  # Set to white
img_binary[img <= threshold_value] <- 0  # Set to black

# Make the binary image a mask
mask <- img_binary

# Save the binary image if you want to see the result
save.image(img_binary, "./2020/2020-09-08/images/binary_image.png")

# Data wrangling ----------------------------------------------------------

chandler <- friends |>
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  mutate(word = str_replace_all(word, "<a[^>]*>(.*?)</a>", "\\1")) %>%
  arrange(desc(n)) %>%
  slice(1:500)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2020", "2020-09-08", "recording"),
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

set.seed(42)

ggplot(chandler, aes(label = word, size = n)) +
  geom_text_wordcloud(family = "Rock Salt") +
  theme_minimal() +
  scale_size_area(max_size = 30)

ggplot(chandler, aes(label = word)) +
  geom_text_wordcloud_area(mask = mask) + 
  theme_minimal()
url = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/twitter_wordcloud/handmaiden.jpeg"
handmaiden <- "handmaiden.jpg"
download.file(url, handmaiden) # download file

test = wordcloud2(chandler, figPath = "./2020/2020-09-08/images/binary_image.png",
           color = "black",
           size = 1.5, #widgetsize = 2,
           minRotation = -pi/4, maxRotation = -pi/4,
           fontFamily = "Rock Salt")
test
# save it in html
library("htmlwidgets")
saveWidget(test,"tmp.html",selfcontained = F)
# and in pdf

webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480)

theme(
  plot.margin = margin(5, 5, 5, 5),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  plot.caption = element_textbox_simple(
    colour = text_col,
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 10, t = 10),
    lineheight = 0.5,
    family = body_font
  )
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2020", "2020-09-08", paste0("20200908", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
