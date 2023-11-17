library(tidyverse)
library(sf)
library(grid)
library(magick)
library(tidytext)
library(here)

# Input image options
i <- "./2020/2020-09-08/images/chandler.jpeg"
black_white_threshold <- 0.5 # below threshold is dark, above is light
text_in_dark <- TRUE # Should text be placed in dark or light area of image?
size <- 500 # resize original image to this many pixles wide (just to make it more managable for testing)
image_buffer_pcnt <- -0.075 # percentage of output_w

# Output image options
output_w <- 14 # chosen image width in inches
background_fill <- "white"
border_percent <- 10 # percent of output_w
border_fill <- "white"
border_line_col <- "black"
border_line_sizes <- 0.5 # applies to page border and textbox border
page_bleed_pcnt <- 0.5

# Text options
font_size_point <- 5
font_face <- "bold" # "plain", "bold", "italic", "oblique", and "bold.italic"
font_col <- "black"
text_box_fill <- "transparent"
line_breathing_space <- 0.85
pal <- c("#ff5238", "#ffdc00", "#42A2D6", "#9A0006", "#FFF580", "#00009E")

i <- image_read(i) |> image_resize(paste0(size, "x"))

info <- image_info(i)
w <- info$width
h <- info$height
ar <- h/w
output_h <- output_w * ar # compute output height in inches
i

i_sf <- 
  i |> 
  image_convert(type = "grayscale") |>
  image_flip() |>
  image_raster() |>
  mutate(
    x = scales::rescale(x, to = c(0, output_w)),
    y = scales::rescale(y, to = c(0, output_h)),
    col2rgb(col) |> t() |> as_tibble(),
    col = scales::rescale(red, to = c(0,1)), # 0 is dark, 1 is light
    col = case_when(col > black_white_threshold ~ 1, TRUE ~ 0)) |> 
  select(x, y, col) |> 
  stars::st_as_stars() |> 
  st_as_sf(as_points = FALSE, merge = TRUE) |> 
  st_make_valid() |> 
  st_set_agr("constant")

box_area <- i_sf |> filter(col == ifelse(text_in_dark, 0, 1)) |> st_combine() 
box_area_buffered <- 
  box_area |> 
  st_buffer((output_w/100)*image_buffer_pcnt, endCapStyle = "FLAT")

font_size_inches <- (font_size_point/72.27)*line_breathing_space # point

ls <- 
  seq(
    from = output_h-(font_size_inches/2), 
    to = 0+(font_size_inches/2), 
    by = -font_size_inches) |> 
  map(~st_linestring(matrix(c(0, .x, output_w, .x), ncol = 2, byrow = TRUE))) |> 
  st_sfc() |> 
  st_intersection(box_area_buffered) |> 
  st_buffer(font_size_inches/2, endCapStyle = "FLAT") |>
  st_cast("MULTIPOLYGON") |> 
  st_cast("POLYGON") |> 
  st_as_sf() |> 
  mutate(
    bbox = map(x, st_bbox),
    l = map_dbl(bbox, ~.x[1]),
    b = map_dbl(bbox, ~.x[2]),
    r = map_dbl(bbox, ~.x[3]),
    t = map_dbl(bbox, ~.x[4])) |>
  arrange(desc(round(t, 10)), l) |>
  mutate(
    rn = row_number(),
    box_width = r - l,
    box_height = t - b,
    box_grob = 
      pmap(
        .l = list(l, b, box_width, box_height), 
        .f = ~rectGrob(
          x = ..1, 
          y = ..2, 
          width = ..3, 
          height = ..4, 
          just = c(0,0),
          default.units = "inches",
          gp = gpar(lwd = border_line_sizes, 
                    fill = text_box_fill))))|> 
  select(-bbox) |> 
  rename(geom = x)

ls <- 
  seq(
    from = output_h-(font_size_inches/2), 
    to = 0+(font_size_inches/2), 
    by = -font_size_inches) |> 
  map(~st_linestring(matrix(c(0, .x, output_w, .x), ncol = 2, byrow = TRUE))) |> 
  st_sfc() |> 
  st_intersection(box_area_buffered) |> 
  st_buffer(font_size_inches/2, endCapStyle = "FLAT") |>
  st_cast("MULTIPOLYGON") |> 
  st_cast("POLYGON") |> 
  st_as_sf() |> 
  mutate(
    bbox = map(x, st_bbox),
    l = map_dbl(bbox, ~.x[1]),
    b = map_dbl(bbox, ~.x[2]),
    r = map_dbl(bbox, ~.x[3]),
    t = map_dbl(bbox, ~.x[4])) |>
  arrange(desc(round(t, 10)), l) |>
  mutate(
    rn = row_number(),
    box_width = r - l,
    box_height = t - b,
    box_grob = 
      pmap(
        list(l, b, box_width, box_height, rn), 
        ~rectGrob(
          x = ..1, 
          y = ..2, 
          width = ..3, 
          height = ..4, 
          just = c(0,0),
          default.units = "inches",
          gp = gpar(
            col = pal[(..5 %% length(pal)) + 1],  # Use modulo to cycle through colors
            lwd = border_line_sizes, 
            fill = text_box_fill
          )
        )
      )
  ) |> 
  select(-bbox) |> 
  rename(geom = x)

ggplot() +
  geom_sf(data = i_sf, col = NA, fill = NA) +
  geom_sf(data = box_area, col = NA, fill = 2) +
  geom_sf(data = ls$geom, fill = NA, col = 1) +
  coord_sf(xlim = c(2,6), ylim = c(10, 14))

tuesdata <- tidytuesdayR::tt_load("2020-09-08")
friends <- tuesdata$friends
friends
chandler <- friends |>
  mutate(text = str_replace_all(text, "<a[^>]*>(.*?)</a>", "\\1")) |>
  filter(speaker == "Chandler Bing")

sentences <- chandler$text
s <- paste0(sentences, " ") |> paste0(collapse = "")
all_chars <- str_split_1(s, "")
unique_chars <- unique(all_chars)

page_border_inches <- (output_w/100) * border_percent
page_bleed_inches <- (output_w/100) * page_bleed_pcnt

out_w <- output_w + page_border_inches + page_bleed_inches
out_h <- output_h + page_border_inches + page_bleed_inches

# Initialise device
png(here('2020', '2020-09-08', '20200908.png'), width = out_w, height = out_h, units = "in", res = 400,  bg = border_fill)

# Create viewport for drawing
grid.newpage()
pushViewport(
  viewport(
    x = out_w/2,
    y = out_h/2,
    width = output_w, 
    height = output_h,
    xscale = c(0, output_w),
    yscale = c(0, output_h),
    default.units = "inches",
    gp = gpar(fontsize = font_size_point, cex = 1, fontface = font_face,
              fontfamily = "Gabriel Weiss' Friends Font")))

# Add rectangle of the plotting area which can be filled with colour or given a border
grid.rect(x = output_w/2,
          y = output_h/2,
          width = output_w, 
          height = output_h,
          default.units = "inches",
          gp = gpar(fill = background_fill, col = border_line_col, lwd = border_line_sizes))

# Draw each textbox
map(ls$box_grob, grid.draw) |> invisible()

# Create a lookup of each unique character in the text and its width on the current device
lu <- 
  map_dbl(
    .x = unique_chars, 
    .f = ~convertWidth(grobWidth(textGrob(.x)), "inches")) |> 
  set_names(unique_chars)

# Create a vector of the width of every character in the text and its minimum value
char_widths <- lu[all_chars] |> unname()
min_char_width <- min(char_widths)
n_chars <- length(char_widths)

# Now iterate through the textboxes
start <- Sys.time()
for(i in 1:nrow(ls)){
  
  # If no text left, stop loop
  if(length(char_widths) == 0) break
  
  # Indices of all letters that will fit in this textbox
  char_selection <- which(cumsum(char_widths[1:ceiling(ls$box_width[i]/min_char_width)]) <= ls$box_width[i])
  
  # If no letters fit, go to next textbox
  if(length(char_selection) == 0) next
  
  # Text characters for this iteration
  text <- all_chars[char_selection]
  
  # If all empty go to next textbox TODO: here it should really pick new letters...
  if(all(text == " ")) next
  
  # Remove first and last character if they are a space
  if(text[1] == " ") text <- text[-1]
  if(text[length(text)] == " ") text <- text[-length(text)]
  
  # Number of characters to print and their widths
  ltext <- length(text)
  text_widths <- lu[text]
  
  # Compute additional width to add to each letter (not the last letter)
  additional_width <- (ls$box_width[i] - sum(text_widths))/(ltext-1)
  
  # Add the additional width to all but the last character and combine the last width to the vector
  new_text_widths <- c(text_widths[-ltext]+additional_width, text_widths[ltext])
  
  # Compute the x-position of the new (wider) characters    
  x_pos <-  ls$l[i] + c(0, cumsum(new_text_widths)[-ltext])
  
  # Draw the letters to the device
  for(j in seq_along(x_pos)){
    
    grid.text(
      label = text[j],
      x = x_pos[j],
      y = ls$b[i],
      just = c(0,0),
      gp = gpar(col = font_col),
      default.units = "inches")}
  
  # Remove the original full character selection from all characters
  all_chars <- all_chars[-char_selection]
  char_widths <- char_widths[-char_selection]
  
}

