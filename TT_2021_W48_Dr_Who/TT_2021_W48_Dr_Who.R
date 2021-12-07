# Tidy Tuesday - W48 - Dr Who ------------------------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2021, Week 48). 
#
# Date: 2021-12-06
#
# Author: Fiona Lees
#
# Data Source: datardis package](https://randomics.netlify.app/posts/2021-11-16-datardis/)
#
# Data Description: Data relating to episodes of Doctor Who (new era only).


### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------
episodes <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv"
)


### Wrangle Data -------------------------------------------------------------------------------------

# Filter out data that isn't required and create new variables
episodes_new <- episodes %>% 
  select(-era, -serial_title, -story_number, -production_code) %>% 
  # Only keep episodes / specials that are part of a season (exclude season 13) 
  filter(!is.na(season_number) & season_number < 13) %>% 
  # Add variable to flag special, first and other episodes  
  mutate(type_new = as_factor(case_when(
    type == "special" ~ "Christmas special",
    episode_number == 1 ~ "First episode",
    TRUE ~ "Other episode"
  )), .after = type)

# Create per season summary info
summary_season <- episodes_new %>% 
  group_by(season_number, type) %>% 
  summarise(
    n_episodes = n(),
    mean_viewers = mean(uk_viewers),
    mean_rating = mean(rating),
  ) %>% 
  ungroup()


### Visualise Data -----------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Playfair Display")

# Colours
episode_type_colours = c("#EFDE02", "#DE4046", "#FFFFFF")
dr_who_blue_light = "#265773"
dr_who_blue_mid = "#204860"
dr_who_blue_dark = "#193A4D"

# Theme
theme_set(theme_minimal())
theme_update(
  text = element_text(family = "Lato", size = 12, colour = dr_who_blue_dark),
  plot.title = element_text(family = "Playfair Display", size = 18, face = "bold", margin = margin(b = 10)),
  plot.subtitle = element_text(size = 12, margin = margin(b = 10), lineheight = 1.1),
  plot.caption = element_text(size = 10, margin = margin(t = 10), hjust = 0, lineheight = 1.),
  plot.background = element_rect(colour = NA, fill = "#FFFFFF"),
  plot.margin = margin(rep(10, 10)),
  strip.text = element_text(size = 12, colour = "#FFFFFF", face = "bold", hjust = 0.05),
  strip.background = element_rect(fill = dr_who_blue_light, colour = "#FFFFFF"),
  panel.background = element_rect(fill = dr_who_blue_light),
  panel.border = element_rect(fill = NA, colour = "#FFFFFF"),
  panel.grid.major = element_line(colour = dr_who_blue_mid, size = 0.5),
  panel.grid.minor = element_blank(),
  legend.position = "top",
  legend.justification = "left",
  legend.key = element_rect(fill = dr_who_blue_light, colour = dr_who_blue_light), 
  legend.key.size = unit(0.3, "inch"),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 12),
  axis.text = element_text(size = 12, colour = dr_who_blue_dark),
  axis.title.y = element_text(margin = margin(r = 10)),
  axis.title.x = element_text(margin = margin(t = 10, b = 10))
)

# Annotation text
annotation_text <- tibble(
  x = c(73, 84.2),
  y = c(13, 13),
  label = c("High viewers\nLow rating", "Average"),
  season_number = c(1, 1),
)

# Annotation lines
annotation_line <- tibble(
  x = c(84, 87.5),
  xend = c(82.8, 87.5),
  y = c(13, 12.3),
  yend = c(13, 8.25),
  season_number = c(1, 1),
)

# Labels for facet strips
season_labels <- str_c("Season ", 1:12)
names(season_labels) <- c(1:12)


# Create plot
p_final <- ggplot() +
  # Mean lines
  geom_vline(data = filter(summary_season, type == "episode"), 
             aes(xintercept = mean_rating), linetype = "dashed", colour = "#FFFFFF", size = 0.5) +
  geom_hline(data = filter(summary_season, type == "episode"), 
             aes(yintercept = mean_viewers), linetype = "dashed", colour = "#FFFFFF", size = 0.5) +
  # Points
  geom_point(data = filter(episodes_new, type_new == "Other episode"), 
             aes(y = uk_viewers, x = rating, colour = type_new), alpha = 0.6, size = 2.5) +
  geom_point(data = filter(episodes_new, type_new != "Other episode"), 
             aes(y = uk_viewers, x = rating, colour = type_new), size = 3) +
  scale_colour_manual(values = episode_type_colours) +
  # Annotation text
  geom_label(data = annotation_text, 
             aes(x = x, y = y, label = label),
             colour = "#FFFFFF",
             fill = dr_who_blue_light,
             label.size = NA,
             size = 3.88,
             hjust = 0,
             vjust = 0.5
             ) +
  # Annotation lines
  geom_segment(data = annotation_line,
               aes(x = x, xend = xend , y = y, yend = yend),
               colour = "#FFFFFF",
               arrow = arrow(length = unit(0.08, "inch")), 
               size = 0.5
               ) +
  # Scales
  scale_x_continuous(limits = c(73, 92), breaks = seq(75, 90, 5)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 14, 2)) +
  # Facet by season number and add season labels to each facet
  facet_wrap(~season_number, nrow = 3, labeller = labeller(season_number = season_labels)) +
  # Labels
  labs(title = "Doctor Who Christmas specials attract viewers, but are not highly rated",
       subtitle = "Rating received for each episode of Doctor Who when it first aired in the UK (Appreciation Index 0 - 100), versus the\nnumber of people, in millions, who viewed it. Each dot represents one episode in each season of the revived era.",
       x = "Rating (Audience Appreciation Index 0 - 100)",
       y = "Number of UK viewers (millions)",
       colour = "",
       caption = "Notes:\n(1) Average (mean) ratings and viewers exclude Christmas specials.\n(2) Season 1-10 Christmas specials aired on Christmas day. Season 11-12 Christmas specials aired on New Year's day.\n  \nTidy Tuesday: Week 48, 2021 | Data source: datardis package | Visualisation: @Fi_Lees"
       ) 

# Tell showtext to use DPI of 300 so text size is correct when plot is saved as an image
showtext_opts(dpi = 300)

# Save the plot as an image  
ggsave("dr_who.png", p_final, width = 12, height = 11, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)


