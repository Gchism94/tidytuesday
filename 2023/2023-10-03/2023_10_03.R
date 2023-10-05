# Title: Comparison of Grant Opportunities: Native American Government Bodues vs All Others
# Author: Greg Chism
# Date: 03/10/2022
# Description: Inequity in U.S. Federal Government Grant access when considering Native American Government Bodies vs. State and Local Governments, 
# using the 03-Oct-2023 #tidytuesday dataset

suppressWarnings(library(tidyverse))

# pacman package loader
if(!require(pacman)) #<1>
  install.packages("pacman")

# Install and load required packages
pacman::p_load(cowplot, # Tools to add an image to a plot #<2>
               colorspace, # Color palettes
               dlookr, # Exploratory data analysis
               formattable, # HTML formatted outputs
               ggdist, # Visualize distributions and incertainty
               gghalves, # Cut boxplots in half
               ggpubr, # Publishable plots
               ggtext, # Text label geoms (particularly richtext)
               ggwaffle, # Waffle plots
               here, # For reproducible working directories
               knitr, # Rendering R chunks and RMarkdown
               patchwork, # For adding the image overlayed onto the plot
               png, # To read in an image
               scales, # Change scales in ggplot
               showtext, # To import Google fonts
               tidytuesdayR, # tidytuesday datasets
               tidyverse, # Data wrangling
               waffle) # Waffle plots

# Original data import
#tuesdata <- tidytuesdayR::tt_load('2023-10-03')

# Keep desired colummns
grant_opportunity_details <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv") |>
  select(opportunity_id, opportunity_category, posted_date, expected_number_of_awards, estimated_total_program_funding, contains("eligibility"), contains("category"), -category_explanation)

# Setup the data for plotting
data <- grant_opportunity_details |>
  pivot_longer(cols = starts_with("eligibility_"), 
               names_to = "eligibility_var", 
               values_to = "eligibility_value") |>
  pivot_longer(cols = starts_with("category_"), 
               names_to = "category", 
               values_to = "category_value") |>
  mutate(year = year(posted_date),
         date = floor_date(posted_date, "month"),
         category_group = case_when(
           category %in% c("category_business", "category_employment", "category_opportunity_zone") ~ "Economic Development and Employment",
           category %in% c("category_community_development", "category_arts", "category_humanities") ~ "Community Development and Arts",
           category == "category_disaster" ~ "Disaster Relief and Prevention",
           category == "category_education" ~ "Education and Training",
           category %in% c("category_energy", "category_environment", "category_natural_resources") ~ "Environmental Sustainability and Natural Resources",
           category %in% c("category_food", "category_health", "category_income_security") ~ "Food, Health, and Social Services",
           category %in% c("category_housing", "category_transportation") ~ "Housing and Transportation",
           category %in% c("category_iija", "category_science") ~ "Infrastructure and Technology",
           TRUE ~ "Other"  # Default category for anything else
         ),
         category_group = factor(
           category_group, 
           levels = c(
             "Community Development and Arts",
             "Disaster Relief and Prevention",
             "Economic Development and Employment",
             "Education and Training",
             "Environmental Sustainability and Natural Resources",
             "Food, Health, and Social Services",
             "Housing and Transportation",
             "Infrastructure and Technology",
             "Other"
           )
         ),
         eligibility_group = case_when(
           eligibility_var %in% c(
             "eligibility_state_governments",
             "eligibility_county_governments",
             "eligibility_independent_school_districts",
             "eligibility_city_or_township_governments",
             "eligibility_special_district_governments") ~ "Government Entities (Excluding Native American and Indian Entities)",
           
           eligibility_var %in% c(
             "eligibility_native_american_tribal_governments_federally_recognized",
             "eligibility_native_american_tribal_organizations_other",
             "eligibility_public_indian_housing_authorities") ~ "Native American and Indian Entities",
           
           eligibility_var %in% c("eligibility_nonprofits_501c3", "eligibility_nonprofits_non_501c3") ~ "Nonprofit Organizations",
           
           eligibility_var == "eligibility_for_profit" ~ "For-Profit Organizations",
           
           eligibility_var == "eligibility_small_businesses" ~ "Small Businesses",
           
           eligibility_var %in% c(
             "eligibility_private_institutions_of_higher_education",
             "eligibility_public_institutions_of_higher_education") ~ "Institutions of Higher Education",
           
           eligibility_var == "eligibility_others" ~ "Other Eligible Groups",
           
           TRUE ~ "Unspecified"  # Default category for anything else
         ),
         eligibility_var = str_replace(eligibility_var, "eligibility_", ""),
         category = str_replace(category, "category_", ""),
         amount_individ = ifelse(expected_number_of_awards != 0, estimated_total_program_funding / expected_number_of_awards, estimated_total_program_funding)) |>
  filter(!is.na(amount_individ) & eligibility_value == TRUE & category_value == TRUE & estimated_total_program_funding != 0) 

# Subplot 1 - bubble plot showing size and distribution of Federal Grants
set.seed(123)

pal <- c("#ff0000", "#40E0D0", "#fc8800", "#e5cc16", "#2891fc", "#964B00", "#800080", "black", "forestgreen")


# Wrap function
wrap_text <- function(text, width = 20) {
  str_wrap(text, width = width)
}

# Define the sorted wrapped labels
legend_labels <- c(
  "Community Development & Arts",
  "Disaster Relief & Prevention",
  "Economic Development & Employment",
  "Education & Training",
  "Environmental Sustainability & Natural Resources",
  "Food, Health, & Social Services",
  "Housing & Transportation",
  "Infrastructure & Technology",
  "Other"
)

# Calculate custom breaks based on the range
custom_breaks <- c(2.500e+04, 5e+06, 1e+08, 2.5e+09, 1.397e+10)

font_add_google(name = "Bungee", family = "Bungee")
font_add_google(name = "Nunito", family = "Nunito")

p1 <- data |>
  distinct(opportunity_id, .keep_all = TRUE) |>
  filter(eligibility_group != "Native American and Indian Entities") |>
  ggplot(aes(x = date, y = log10(expected_number_of_awards + 1), size = estimated_total_program_funding + 1)) +
  geom_jitter(height = 0.05, alpha = 0.15) +
  geom_jitter(data = data |> 
                filter(eligibility_group == "Native American and Indian Entities") |>
                distinct(opportunity_id, .keep_all = TRUE), 
              aes(x = date, y = log10(expected_number_of_awards + 1), size = estimated_total_program_funding, color = category_group),
              alpha = 1) +
  theme_minimal(base_size = 15, base_family = "sans") + 
  guides(size = guide_legend(title = NULL, nrow = 3, byrow = TRUE),
         color = guide_legend(nrow = 3, byrow = TRUE,
                              override.aes = list(size = 3))) + 
  labs(x = "Date", 
       y = expression(log[10]("Number of Grants")),
       title = "Comparison of Grant Opportunities:\nNative American Government Bodies vs All Others",
       subtitle = "On average, Native American government bodies have access to less Federal Grant funding than<br>State and Local governments in the United States. This largely stems from <span style='color:#964B00'>Food, Health, and Social Services</span> ($371,670,350),<br>but also from <span style='color:#ff0000'>Community Development and Arts</span> ($75,000,000). Here we see the grants available to Native American<br>Government bodies (colored), with size as the dollar amount.",
       caption = "Federal Food, Health and Social Services and Environmental Sustainability and Natural Resources grants\nprovide the largest amount through the most grants to Native American Government bodies.",
       color = NULL) + 
  scale_color_manual(
    values = pal,
    labels = wrap_text(legend_labels)) +
  scale_size_continuous(
    range = c(1.5, 15),
    breaks = custom_breaks, 
    labels = scales::dollar(custom_breaks),
    limits = c(min(custom_breaks), max(custom_breaks))) +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.25, "cm"),
        plot.title = element_text(family = "Bungee"),
        plot.subtitle = element_markdown(size = 14, color = "gray40",
                                         lineheight = 1.25),
        plot.caption = element_markdown(size = 14, color = "gray40", 
                                        hjust = 0.1, vjust = -1)) 

# Subplot 2 - Waffle charts showing the proportion of federal grants available to Native American Government bodies
# Values for Food, Health, and Social Services
data |>
  filter(eligibility_group == "Government Entities (Excluding Native American and Indian Entities)" | eligibility_group == "Native American and Indian Entities" & category_group == "Food, Health, and Social Services") |>
  group_by(eligibility_group) |>
  summarise(funding = mean(estimated_total_program_funding)) |>
  ungroup() |>
  mutate(prop = round(funding / sum(funding) * 100, 0)) |>
  select(eligibility_group, prop) 

# Values for Community Development and Arts
data |>
  filter(eligibility_group == "Government Entities (Excluding Native American and Indian Entities)" | eligibility_group == "Native American and Indian Entities" & category_group == "Community Development and Arts") |>
  group_by(eligibility_group) |>
  summarise(funding = mean(estimated_total_program_funding)) |>
  ungroup() |>
  mutate(prop = round(funding / sum(funding) * 100, 0)) |>
  select(eligibility_group, prop)

w1 <- waffle(c("Native American Govs" = 14, "State & Local Govs" = 86), rows = 10,
             colors = c("#964B00", "gray70"),
             legend_pos = "none") +
  #labs(subtitle = "Native American government bodies have access to 14%<br>of the amount of federal <span style='color:#964B00'>Food, Health, and Social Services</span><br>funding compared to State and Local governments and<br>only 1% of federal <span style='color:#ff0000'>Community Development and Arts</span> funds.<br>") +
  theme_void() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown(size = 12, color = "gray40",
                                         lineheight = 1.25, hjust = 0))

w2 <- waffle(c("Native American Govs" = 1, "State & Local Govs" = 99), rows = 10,
             colors = c("#ff0000", "gray70"),
             legend_pos = "none") +
  theme_void(base_size = 14) +
  theme(legend.position = "none")

# Combine the plots
p2 <- plot_grid(w1, w2, ncol = 2, align = "hv") +
  theme(plot.margin = margin(0, 0, 1, 0, "cm")) 

# Subplot 3: Circular bar plot showing the number of Federal Grants (and dollar amounts - log10 transformed) 
p3 <- data %>%
  filter(year == 2023 & eligibility_group == "Native American and Indian Entities") %>%
  mutate(estimated_total_program_funding = log10(estimated_total_program_funding)) %>%
  group_by(category_group) %>%
  summarise(
    sum_amount = sum(estimated_total_program_funding),
    n = n()
  ) |>
  ggplot() +
  geom_hline(
    yintercept = c(0:5) * 100,
    color = "lightgray"
  ) +
  geom_col(
    aes(
      x = reorder(str_wrap(category_group, 3), sum_amount),
      y = sum_amount,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = 0.9
  ) +
  geom_segment(
    aes(
      x = reorder(str_wrap(category_group, 5), sum_amount),
      y = 0,
      xend = reorder(str_wrap(category_group, 5), sum_amount),
      yend = 500
    ),
    linetype = "dashed",
    color = "gray12"
  ) +
  coord_polar() +
  theme_void() +
  scale_fill_continuous_sequential(palette = "ag_sunset") +
  guides(
    fill = guide_colorsteps(
      title = "Number of grants (2023)",
      barwidth = 15,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  scale_y_continuous(
    limits = c(-150, 550),
    expand = c(0, 0),
    breaks = c(100, 300, 500)
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      color = "gray12",
      size = 11,
      family = "sans",
      margin = margin(t = 0, r = -100, b = 0, l = -100, "pt"),
      vjust = -10
    ),
    legend.position = "bottom",
    legend.title = element_text(family = "sans", size = 12),
    plot.margin = margin(1, 1.5, 0, 1.5, "cm")
  )

# Final plot
p1.3 <- plot_grid(p1, p3, ncol = 1)

p1.3 + inset_element(p2, left = 0.05, bottom = 0.775, right = 0.35, top = 0.9)

ggsave(plot = last_plot(), here("2023", "2023-10-03", "final.pdf"), width = 7000, height = 9000, units = "px", dpi = 500)

