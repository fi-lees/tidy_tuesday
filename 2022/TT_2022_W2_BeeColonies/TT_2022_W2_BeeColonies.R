# Tidy Tuesday - 2022 W2 - Honey Bee Colonies --------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 2). 
#
# Date: 2022-01-11
#
# Author: Fiona Lees
#
# Data Source: The United States Department of Agriculture (USDA) - Honey Bee Colonies
#
# Data Description: Information on honey bee colonies; quarterly information about the 
# number of colonies; lost, added and renovated colonies; and colony health stressors.  

### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(geojsonio)
library(broom) 
library(mapproj)
library(png)
library(patchwork)
library(tidyverse)


### Import Data -------------------------------------------------------------------------------------
colony <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv"
  )
  

### Wrangle Data ------------------------------------------------------------------------------------

# Change `year` to numeric and `months` and `state` to factors
colony_new <- colony %>% 
  mutate(
    year = as.numeric(year),
    months = as_factor(months),
    state = as_factor(state)
  ) %>% 
  select(year:colony_lost_pct)

# Create summary for each state across all quarters and years  
colony_summary_overall <- colony_new %>% 
  group_by(state) %>% 
  summarise(
    n_quarters = n(),
    mean_colony_n = mean(colony_n, na.rm = TRUE),
    mean_colony_max = mean(colony_max, na.rm = TRUE),
    mean_colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE)
  ) %>% 
  ungroup() 

# Create summary for each state for each quarter across all years 
colony_summary_quarter <- colony_new %>% 
  group_by(months, state) %>% 
  summarise(
    n_years = n(),
    mean_colony_n = mean(colony_n, na.rm = TRUE),
    mean_colony_max = mean(colony_max, na.rm = TRUE),
    mean_colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE),
  ) %>% 
  ungroup()

# Get overall summary data for USA + four states with highest `mean_colony_max`
top_states <- colony_summary_overall %>% 
  mutate(colony_max_rank = min_rank(desc(mean_colony_max))) %>% 
  filter(colony_max_rank <= 4 | state == "United States") %>% 
  arrange(state)

# Get the colony data for USA + four states with highest `mean_colony_max`
colony_top_states <- colony_new %>%
  filter(state %in% top_states$state) %>% 
  group_by(months, state) %>% 
  # Add min / max colony_lost_pct for each state / quarter
  mutate(
    min_colony_lost_pct = min(colony_lost_pct, na.rm = TRUE),
    max_colony_lost_pct = max(colony_lost_pct, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(state, year, months)

# Get the quarterly summary data for USA + four states with highest `mean_colony_max`
colony_top_states_summary <- colony_summary_quarter %>%
  filter(state %in% top_states$state) %>% 
  arrange(state, months)

# Read in hexbin map boundary data for USA (spdf = Spatial Polygons Data Frame)
# Source: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf <- geojson_read("us_states_hexgrid.geojson", what = "sp")

# Remove "(United States)" from name of each state
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Turn spdf into a tidy tibble for use within ggplot2
spdf_tidy <- tidy(spdf, region = "google_name")


### Visualise Data -----------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")

# Colours
honey_pale = "#f8d887"
honey_dark = "#c18d0b"

# Hexbin maps
# Base map showing all states in a pale honey colour
map_base <- ggplot() +
  geom_polygon(data = spdf_tidy, 
               aes(x = long, y = lat, group = group), 
               fill = honey_pale, color = "white") +
  theme_void() +
  coord_map()

# USA - show all states in a dark honey colour
map_usa <- map_base +
  geom_polygon(data = spdf_tidy, 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# California - show state in a dark honey colour
map_ca <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "California"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# Florida
map_fl <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "Florida"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# North Dakota
map_nd <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "North Dakota"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")
# Texas
map_tx <- map_base +
  geom_polygon(data = filter(spdf_tidy, id == "Texas"), 
               aes(x = long, y = lat, group = group), 
               fill = honey_dark, color = "white")

# Create list containing all maps created above
map_list <- list(map_usa, map_ca, map_fl, map_nd, map_tx)

# Create a .png file name for each map
map_paths <- str_c(c("map_usa", "map_ca", "map_fl", "map_nd", "map_tx"), ".png")

# Save each map as a .png image using the file names created above
pwalk(list(map_paths, map_list), ggsave, dpi = 300)

# Create list containing all map .png files (read back in using png::readPNG) 
map_image_list <- map(map_paths, readPNG, native = TRUE, info = TRUE)

# Store name of each image in the list for ease of access later
names(map_image_list) <- str_sub(map_paths, 1, -5)

# Cycle Plot
# Labels for facet strips
quarter_labels <- c("Jan - Mar", "Apr - Jun", "Jul - Sep", "Oct - Dec")
names(quarter_labels) <- levels(colony_new$months)

# Annotation text
annotation_text <- tibble(
  label = c("Average", "Range"),
  x = c(2015.4, 2018.7),
  y = c(7, 7),
  state = factor(c("United States", "United States")),
  months = factor(c("January-March", "January-March"))
)

# Annotation lines
annotation_line <- tibble(
  x = c(2015.6, 2018.9),
  xend = c(2015.6, 2018.9),
  y = c(7.7, 7.7),
  yend = c(15.28, 13),
  state = factor(c("United States", "United States")),
  months = factor(c("January-March", "January-March"))
)

# Initial cycle plot
p_initial <- colony_top_states %>%
  # Reorder states - USA first, followed by states in alphabetical order
  mutate(state = factor(state, levels = c("United States", "California", "Florida", "North Dakota", "Texas"))) %>% 
  ggplot(aes(x = year, y = colony_lost_pct)) +
  # Ribbon showing range of values for each quarter across all years
  geom_ribbon(aes(ymin = min_colony_lost_pct, ymax = max_colony_lost_pct), 
              fill = honey_pale, alpha = 0.5) +
  # Dashed horizontal line showing mean value for each quarter across all years
  geom_hline(data = colony_top_states_summary, 
             aes(yintercept = mean_colony_lost_pct), 
             colour = honey_dark, 
             linetype = "dashed"
             ) +
  # Line and points showing values for each year
  geom_line() +
  geom_point() +
  # Facet by state and quarter
  facet_grid(state ~ months, switch = "y", labeller = labeller(months = quarter_labels)) +
  # Annotation text
  geom_label(data = annotation_text, 
             aes(x = x, y = y, label = label),
             colour = "grey40",
             fill = "white",
             label.size = NA,
             label.padding = unit(0.0, "lines"),
             size = 3.5,
             hjust = 0.0,
             vjust = 0.7
             ) +
  # Annotation lines
  geom_segment(data = annotation_line,
               aes(x = x, xend = xend , y = y, yend = yend),
               colour = "grey40",
               size = 0.5
               ) +
  # Scale formatting
  scale_x_continuous(limits = c(2015, 2021), 
                     breaks = c(2015, 2021), 
                     position = "top"
                     ) +
  scale_y_continuous(limits = c(0, 22), 
                     breaks = seq(0, 20, 5),
                     position = "right",
                     labels = scales::label_percent(accuracy = 1, scale = 1)
                     ) +
  # Styling
  theme_minimal() +
  theme(text = element_text(family = "Lato", colour = "grey40"),
        panel.spacing = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(colour = "grey40", size = 11, face = "bold"),
        strip.text.y.left = element_text(angle = 0, vjust = 0.92, hjust = 0, margin = margin(r = 60)),
        strip.text.x.top = element_text(vjust = 1, margin = margin(b = 5)),
        axis.text = element_text(colour = "grey40", size = 10, hjust = c(0.05, 0.95)),
        axis.title.y.right = element_text(colour = "grey40", size = 10, hjust = 0.05, margin = margin(l = 10))
        ) +
  # Labels
  labs(x = "",
       y = "Colonies lost (%)"
       )

# Add hexbin map images to cycle plot
p_final <- p_initial +
    patchwork::inset_element(
    p = map_image_list$map_usa,
    t = 0.90,
    r = 0.190,
    l = 0.000,
    b = 0.77,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_ca,
    t = 0.71,
    r = 0.190,
    l = 0.000,
    b = 0.58,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_fl,
    t = 0.52,
    r = 0.190,
    l = 0.000,
    b = 0.39,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_nd,
    t = 0.33,
    r = 0.190,
    l = 0.000,
    b = 0.20,
    align_to = "plot"
  ) +
  patchwork::inset_element(
    p = map_image_list$map_tx,
    t = 0.14,
    r = 0.190,
    l = 0.000,
    b = 0.01,
    align_to = "plot"
  )
  
# Add plot titles
p_final <- p_final +
  plot_annotation(
    title = "Seasonal variation in honey bee colony losses differs across the USA",
    subtitle = "Percentage of honey bee colonies lost each calendar quarter (2015 - 2021), across the USA as a whole and in the four\nstates with the highest number of colonies. The average (mean) loss across all years is shown for each quarter, along\nwith the range (minimum / maximum losses).",
    caption = "Notes:\n1. A lost colony is a completely failed colony with loss of most worker bees, and possibly the queen. Lost colonies are no longer viable.\n2. No data are available for April - June 2019 (data collection was suspended by USDA).\n3. Data for July - December 2021 are not available yet.\n \nTidy Tuesday: 2022, Week 2 | Visualisation: Fiona Lees (@Fi_Lees) | Data source: United States Department of Agriculture (USDA)",
    theme = theme(
      plot.title = element_text(family = "Roboto Slab", colour = "black", size = 16, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lato", colour = "grey40", size = 11),
      plot.caption = element_text(family = "Lato", colour = "grey40", size = 10, hjust = 0),
      plot.margin = margin(10, 10, 10, 10)
      )
    )

# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("Bee_Colony_Loss.png", p_final, width = 9, height = 12, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)
