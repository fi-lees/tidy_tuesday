# Tidy Tuesday - W46 - Afrilearndata ------------------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2021, Week 46). 
#
# Date: 2021-11-22
#
# Author: Fiona Lees
#
# Data Source: AfriLearnData package (https://afrimapr.github.io/afrilearndata/)
#
# Data Description: Small African spatial datasets to help with the learning and teaching of 
# spatial techniques and mapping.


### Load Packages -------------------------------------------------------------------------------------
library(afrilearndata)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(magick)
library(scico)
library(showtext)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------

# The afrilearndata package permits lazy loading and can accessed directly

# Coerce the 2000 population density raster data into a data frame 
afripop_2000_df <- afripop2000 %>%
  as.data.frame(xy = TRUE) %>%
  rename(population = ppp_2000_1km_Aggregated) %>%
  filter(!is.na(population))

# Repeat for 2020 data
afripop_2020_df <- afripop2020 %>%
  as.data.frame(xy = TRUE) %>%
  rename(population = ppp_2020_1km_Aggregated) %>%
  filter(!is.na(population))

# Get river data from rnaturalearth package
rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")


### Wrangle Data ------------------------------------------------------------------------------------

# Bin population density into a discrete variable
afripop_2000_df <- afripop_2000_df %>%
  mutate(population_2 = cut(
    population,
    breaks = c(-0.1, 2, 20, 200, 2000, 22000),
    labels = c("0 - 2", "2 - 20", "20 - 200", "200 - 2,000", "2,000 - 22,000")
  ))

afripop_2020_df <- afripop_2020_df %>%
  mutate(population_2 = cut(
    population,
    breaks = c(-0.1, 2, 20, 200, 2000, 22000),
    labels = c("0 - 2", "2 - 20", "20 - 200", "200 - 2,000", "2,000 - 22,000")
  ))


### Visualise Data ----------------------------------------------------------------------------------

# Load fonts
font_add_google("Lato")
font_add_google("Playfair Display")

# Use showtext to render the text
showtext_auto(TRUE)

# Build a base plot
p_map_base <- ggplot(data = afripop_2000_df) +
  geom_raster(aes(x, y, fill = population_2)) +
  # Use only the lightest 80% of the acton colour palette for the population data
  scale_fill_scico_d(direction = -1, palette = "acton", begin = 0.2, end = 1) +
  geom_sf(data = rivers, colour = "#2D204C", size = 0.3) +
  # Trim longitude / latitude coordinates to fit Africa 
  coord_sf(xlim = c(-17.62625, 51.20708), ylim = c(-34.97542, 37.35792)) +
  theme_void() +
  theme(
    text = element_text(family = "Lato", colour = "#E5E5F0"),
    plot.caption = element_text(size = 10, margin = margin(b = 10), hjust = 0.98, lineheight = 1.1),
    plot.background = element_rect(colour = NA, fill = "#2D204C"),
    legend.position = c(0.21, 0.25),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.margin = margin(rep(8, 8)),
    ) +
  labs(
    caption = "Tidy Tuesday: Week 46, 2021\n Data source: afrilearndata\n Visualisation: @Fi_Lees",
    fill = "Population density\n(people / km²)"
    )

# Add year 2000 label to the plot
p_map_2000 <- p_map_base +
  annotate("text", -5, -5, label = "Africa\n2000", 
           size = 16, family = "Playfair Display", fontface = "bold", 
           colour = "#E5E5F0", lineheight = 0.8)

# Update base plot with 2020 data and add year 2020 label
p_map_2020 <- p_map_base %+% afripop_2020_df +
    annotate("text", -5, -5, label = "Africa\n2020", 
           size = 16, family = "Playfair Display", fontface = "bold", 
           colour = "#E5E5F0", lineheight = 0.8)

# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save both maps as images 
ggsave("p_map_2000.png", p_map_2000, width = 10, height = 10, units = "in", dpi = 300)
ggsave("p_map_2020.png", p_map_2020, width = 10, height = 10, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)

# Create animated GIF using magick package
list.files(path = ".", pattern = '*.png') %>% 
  image_read() %>%
  image_join() %>%
  image_animate(fps = 0.5) %>%
  image_write("africa_pop_density_animated.gif")

