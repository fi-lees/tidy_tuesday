# Tidy Tuesday - 2022 W8 - Freedom in the World ------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 8). 
#
# Date: 2022-02-22
#
# Author: Fiona Lees
#
# Data Source: Freedom House (https://freedom.org/report/freedom-world)
#
# Data Description: Freedom index scores and freedom statuses for 195 countries and 15 territories.

### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(ggbeeswarm)
library(MetBrewer)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------
# Data downloaded from Freedom House's website (https://freedom.org/report/freedom-world) 
freedom <- readxl::read_xlsx(
  "data/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2021.xlsx",
  range = "FIW06-21!A1:S3340",
  # Explicitly define the data type in each column to ensure it gets pulled in correctly
  col_types = c("text", "text", "text", "numeric", "text", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
) %>% 
  janitor::clean_names()


### Explore / Wrangle Data ---------------------------------------------------------------------------

# Drop data not required
countries_2020 <- freedom %>% 
  filter(c_t == "c" & edition == 2021) %>% 
  select(country_territory:cl_rating, pr, cl, total) %>% 
  rename(country = country_territory)

# Make status and region factors
countries_2020 <- countries_2020 %>% 
  mutate(year = edition - 1, .after = edition,
         status = factor(status, levels = c("NF", "PF", "F"), 
                         labels = c("Not Free", "Partly Free", "Free")),
         region_2 = factor(region, levels = c("MENA", "Eurasia", "SSA", "Asia", "Americas", "Europe"), 
                         labels = c("Middle East &\nNorth Africa", "Eurasia", 
                                    "Sub-Saharan\nAfrica", "Asia &\nAustralasia", 
                                    "North America,\nLatin America\n& the Caribbean", "Europe")),
         )

# Highest score(s) in each region
countries_2020_high <- countries_2020 %>% 
  group_by(region) %>% 
  slice(which(total == max(total))) %>% 
  ungroup() %>% 
  arrange(region_2, total)
  
# Lowest score(s) in each region
countries_2020_low <- countries_2020 %>% 
  group_by(region) %>% 
  slice(which(total == min(total))) %>% 
  ungroup() %>% 
  arrange(region_2, total)

# Create flag for the highest and lowest countries in `countries_2020` dataset
countries_2020 <- countries_2020 %>%
  mutate(hi_low_country = ifelse(country %in% countries_2020_high$country | country %in% countries_2020_low$country, TRUE, FALSE))


### Visualise Data -----------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")

# Chart
p_final <- countries_2020 %>% 
  ggplot(aes(x = total, y = region_2, fill = status, colour = hi_low_country)) +
  # Beeswarm
  geom_beeswarm(
    shape = 21, 
    cex = 1.8, 
    size = 3.8, 
    groupOnX = FALSE
  ) +
  # Dot fill colour and borders
  scale_fill_manual(values = met.brewer("Hiroshige", 3)) +
  scale_colour_manual(values = c("darkgrey", "black"), guide = "none") +
  # Text labels for lowest scores
  geom_label(
    data = countries_2020_low, 
    aes(label = country), 
    colour = "grey40",
    fill = "white",
    size = 3.88, 
    # Use position_beeswarm() to match labels to points
    position = position_beeswarm(groupOnX = FALSE, cex = 1.8),
    # Adjust label positions for individual points
    vjust = c(-0.8, -0.8, 1.5, -0.8, -0.8, 1.5, -0.8), 
    hjust = 1, 
    label.padding = unit(0.02, "cm"),
    label.size = NA
  ) +
  # Text labels for highest scores
  geom_label(
    data = countries_2020_high, 
    aes(label = country), 
    colour = "grey40", 
    fill = "white",
    size = 3.88, 
    # Use position_beeswarm() to match labels to points
    position = position_beeswarm(groupOnX = FALSE, cex = 1.8),
    # Adjust label positions for individual points
    hjust = c(0, 0, 0, 0, 0, 0, 0, -0.25, 0),
    vjust = c(1.8, 2.6, 1.6, 1.6, 1.6, -0.8, -1.9, 1.5, 1.6),
    label.padding = unit(0.02, "cm"),
    label.size = NA
  ) +
  # Horizontal lines between each region  
  geom_hline(yintercept = seq(0.5, 6.5, 1), colour = "grey90") +
  # Scales
  scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 10), position = "top") +
  scale_y_discrete(expand = expansion(add = 0.5)) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", colour = "grey40", size = 12),
    plot.title = element_text(family = "Roberto Slab", colour = "black", size = 16, 
                              face = "bold", margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 0), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(t = 10), hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = 5)),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -15, r = 0, b = -15, l = 0),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  # Titles
  labs(
    title = "Global Freedom Index, 2020",
    subtitle = "Freedom index / status of 195 countries, 100 = most free",
    x = "", 
    y = "",
    colour = "",
    fill = "",
    caption = "Global freedom statuses are calculated on a weighted scale, based on 10 political rights indicators and 15 civil liberties indicators.\nThe combination of the overall score awarded for political rights and the overall score awarded for civil liberties, after being equally\nweighted, determines the status of Not Free, Partly Free, or Free. Data based on status Jan 1 - Dec 31 2020.\n \nVisualisation: Fiona Lees (@Fi_Lees) | Source: Freedom House | Tidy Tuesday: Week 8, 2022"
  )

# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("freedom_2020.png", p_final, width = 12 , height = 9.5, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)




