# Tidy Tuesday - 2022 W12 - Baby Names (USA) ---------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 12). 
#
# Date: 2022-03-22
#
# Author: Fiona Lees
#
# Data Source: Hadley Wickham's `{babynames}` R package.
#

### Load Packages ------------------------------------------------------------------------------------
library(ggridges)
library(rcartocolor)
library(showtext)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------

# Pull the data in directly from {babynames}.  
babynames_usa <- babynames::babynames


### Wrangle Data -------------------------------------------------------------------------------------

# Assign each year to a generation. Drop years before 1901.
baby_usa_1901_onwards <- babynames_usa %>% 
  filter(year >= 1901) %>% 
  mutate(generation = case_when(
    between(year, 1901, 1927) ~ "Greatest Generation",
    between(year, 1928, 1945) ~ "Silent Generation",
    between(year, 1946, 1964) ~ "Baby Boomer",
    between(year, 1965, 1980) ~ "Generation X",
    between(year, 1981, 1996) ~ "Millennial",
    between(year, 1997, 2012) ~ "Generation Z",
    between(year, 2013, 2025) ~ "Alpha"
  ))

# Summary table showing the top male and female name in each generation
top_gen_names_summary <- baby_usa_1901_onwards %>% 
  group_by(generation, sex, name) %>% 
  summarise(
    min_year = min(year),
    max_year = max(year),
    n_with_name = sum(n),
    ) %>% 
  slice(which(n_with_name == max(n_with_name))) %>% 
  arrange(min_year, sex) %>% 
  ungroup()

# Vector with the top female names
top_f <- top_gen_names_summary %>% 
  filter(sex == "F") %>% 
  pull(name)

# Vector with the top male names
top_m <- top_gen_names_summary %>% 
  filter(sex == "M") %>% 
  pull(name)

# Create babynames dataset showing only the most popular female names
top_gen_names_f <- baby_usa_1901_onwards %>% 
  filter(sex == "F" & name %in% top_f) %>% 
  mutate(name = factor(name, levels = unique(top_f))) %>% 
  arrange(name, year)

# Create babynames dataset showing only the most popular male names
top_gen_names_m <- baby_usa_1901_onwards %>% 
  filter(sex == "M" & name %in% top_m) %>% 
  mutate(name = factor(name, levels = unique(top_m))) %>% 
  arrange(name, year)


### Visualise Data --------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Prata")

# Annotation text
annotation_text <- top_gen_names_summary %>% 
  mutate(
    label = str_c(generation, "\n(", name, ")"),
    max_year = ifelse(max_year == 2017, 2025, max_year),
    x = min_year + (((max_year + 1) - min_year) / 2),
    y = ifelse(sex == "F", "Mary", "John")
  )

# Base plot
p_base <- top_gen_names_f %>% 
  ggplot() +
  # Ridge line plot
  geom_density_ridges_gradient(
    aes(x = year, y = fct_rev(name), height = prop, fill = prop), 
    stat = "identity", 
    scale = 1, 
    colour = "grey85"
    ) +
  # Scales
  scale_x_continuous(
    limits = c(1901, 2026), 
    breaks = c(1901, 1928, 1946, 1965, 1981, 1997, 2013, 2026),
    expand = c(0.02, 0, 0.02, 0)
    ) +
  scale_y_discrete(
    expand = expansion(add = c(0.1, 1.5)),
    position = "right"
    ) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", colour = "grey40", size = 12),
    plot.title = element_text(family = "Prata", colour = "black", size = 14, 
                              face = "bold", margin = margin(b = 10), hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 9, margin = margin(t = 10), hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", colour = "grey80"),
    axis.text = element_text(size = 10, colour = "grey40"),
    axis.title = element_text(size = 10, colour = "grey40", hjust = 0),
    legend.position = "top",
    legend.justification = 0.98,
    legend.key.width = unit(0.3,"inch"),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -20, r = 0, b = 5, l = 0)
    ) +
  # Titles
  labs(
    x = "Year",
    y = "",
    fill = "",
    caption = "Visualisation: Fiona Lees (@Fi_Lees) | Source: {babynames} R Package / US Social Security Administration | Tidy Tuesday: Week 12, 2022"
    )

# Chart for girls 
p_final_f <- p_base +
  # Fill colour for ridges
  scale_fill_carto_c(
    palette = "OrYel", 
    labels = scales::label_percent(accuracy = 1), 
    guide = guide_colourbar(reverse = TRUE)
  ) +
  # Annotation text
  geom_text(
    data = filter(annotation_text,sex == "F"),
    aes(x = x, y = y, label = label),
    family = "Lato",
    colour = "grey40",
    nudge_y = 1.3,
    size = 3.4,
    hjust = 0.5
  ) +
  # Titles
  labs(
    title = "The rise and fall of each generation's most popular name: Girls, USA",
    subtitle = "Percentage of girls, born in each year, with each of these names (1901 - 2017)"
  )

# Chart for boys
# Update base plot with data for males
p_final_m <- p_base %+% top_gen_names_m +
  # Fill colour for ridges
  scale_fill_carto_c(
    palette = "Teal", 
    labels = scales::label_percent(accuracy = 1), 
    guide = guide_colourbar(reverse = TRUE)
  ) +
  # Annotation text
  geom_text(
    data = filter(annotation_text,sex == "M"),
    aes(x = x, y = y, label = label),
    family = "Lato",
    colour = "grey40",
    nudge_y = 1.3,
    size = 3.4,
    hjust = 0.5
  ) +
  # Titles
  labs(
    title = "The rise and fall of each generation's most popular name: Boys, USA",
    subtitle = "Percentage of boys, born in each year, with each of these names (1901 - 2017)"
  )

# Tell showtext to use DPI of 320 so text size is correct when chart is saved as an image
showtext_opts(dpi = 320)

# Save plots as image
ggsave("girl_names.png", p_final_f, width = 9, height = 7, units = "in", dpi = 320)
ggsave("boy_names.png", p_final_m, width = 9, height = 8, units = "in", dpi = 320)

# Turn off showtext
showtext_auto(FALSE)


