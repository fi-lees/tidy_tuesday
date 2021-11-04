# Tidy Tuesday - W44 - Ultra Trail Running ------------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2021, Week 44). 
#
# Date: 2021-10-26
#
# Author: Fiona Lees
#
# Data Source: International Trail Running Association: https://itra.run/Races/FindRaceResults 
#              Made available to #TidyTuesday via Benjamin Nowak: https://twitter.com/BjnNowak. 
#
# Data Description: A compilation of results from trail races that took place between 2012 - 2021. 
#                   Focuses on 100 mile races (give or take a few miles). 
# -----------------------------------------------------------------------------------------------------

### Load Packages -------------------------------------------------------------------------------------
library(showtext)
library(sysfonts)
library(ggtext)
library(patchwork)
library(magick)
library(sf)
library(rnaturalearth)
library(tidyverse)

### Import Data ---------------------------------------------------------------------------------------

# Read in race data from Tidy Tuesday github page
race <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv"
  )

# For the ranking data, use Benjamin's link so that time values above 24hrs are not lost
# (reading time as a character string)
rankings <- readr::read_csv(
  "https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv",
  col_types = list(Time = "c")
)


### Wrangle Data --------------------------------------------------------------------------------------

# Clean up `ranking` column names and convert time variable to a period showing hours, minutes and seconds
rankings <- rankings %>% 
  janitor::clean_names() %>% 
  mutate(time = lubridate::hms(time)) %>% 
  mutate(time_in_seconds = lubridate::duration(as.numeric(time), unit = "seconds"), 
         finish_hour = lubridate::hour(time),
         .after = time
         )

# Join the `race` data to the `ranking` data  
race_rankings <- rankings %>% 
  left_join(race, by = "race_year_id")

# Create filtered dataset for the West Highland Way Race, 2019  
whwr_race_rankings <- race_rankings %>% 
  filter(event == "West Highland Way Race" & date == lubridate::ymd(20190622))

# Create new variables  
whwr_race_rankings <- whwr_race_rankings %>% 
  # Get runner's approx age at time of race (current age is age in 2021, not age at the time of race)
  mutate(race_age = age - (2021 - lubridate::year(date)), .after = age) %>%
  mutate(race_age = case_when((!between(race_age, 16, 100)) ~ NA_real_, TRUE ~ race_age)) %>% 
  # Make gender a factor
  mutate(gender = factor(gender, levels = c("M", "W"), labels = c("Man", "Woman"))) %>% 
  # Calculate rank of each runner within each finishing hour
  group_by(finish_hour) %>% 
  mutate(hour_rank = min_rank(time), .after = finish_hour) %>% 
  ungroup()

# Get information about fastest / youngest / oldest runners for chart labelling.  
# Basic info for each runner
whwr_race_rankings <- whwr_race_rankings %>% 
  mutate(runner_label = str_c(tolower(gender), "\n", tolower(as.character(time)), "\nOverall position: ", rank)) %>% 
  group_by(finish_hour) %>% 
  mutate(
    max_hour_rank = max(hour_rank),
    min_hour_age = min(race_age),
    max_hour_age = max(race_age)
    ) %>%
  ungroup()

# Fastest
whwr_fastest <- whwr_race_rankings %>% 
  group_by(gender) %>% 
  slice(which.min(time_in_seconds)) %>% 
  select(runner, rank, time, finish_hour, hour_rank, 
         race_age, gender, nationality, max_hour_rank, runner_label) %>% 
  mutate(runner_label = str_c("Fastest ", runner_label)) %>% 
  ungroup()

# Youngest
whwr_youngest <- whwr_race_rankings %>% 
  group_by(gender) %>% 
  slice(which.min(race_age)) %>% 
  select(runner, rank, time, finish_hour, hour_rank, 
         race_age, gender, nationality, min_hour_age, runner_label) %>% 
  mutate(runner_label = str_c("Youngest ", runner_label)) %>% 
  ungroup()

# Oldest
whwr_oldest <- whwr_race_rankings %>% 
  group_by(gender) %>% 
  slice(which.max(race_age)) %>% 
  select(runner, rank, time, finish_hour, hour_rank, 
         race_age, gender, nationality, max_hour_age, runner_label) %>% 
  mutate(runner_label = str_c("Oldest ", runner_label)) %>% 
  ungroup()


### Visualise Data ------------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Playfair Display")

# Colours
runner_colours = c("Man" = "#32616a", "Woman" = "#e06508")

# Theme
theme_set(theme_minimal())
theme_update(
  text = element_text(colour = "gray30", family = "Lato"),
  plot.title = element_text(colour = "grey20", family = "Playfair Display", face = "bold", 
                            size = 18, margin = margin(t = 5, b = 5)),
  # Make subtitle ggtext::element_markdown()  - allows colour of key words to be changed
  plot.subtitle = element_markdown(size = 12.5, margin = margin(t = 10, b = 10), lineheight = 1.2),
  plot.caption = element_text(size = 10, margin = margin(t = 10), hjust = 0),
  plot.margin = margin(rep(4, 8)),
  plot.background = element_rect(color = "#ffffff", size = 0.5),
  axis.text = element_text(size = 12),
  axis.title.x = element_text(margin = margin(t = 10, b = 10)),
  axis.title.y = element_text(angle = 0, vjust = 1.01),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  legend.position = "none"
)


# Create chart showing number of runners who finished in each hour  
p_gender_final <- whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = hour_rank, colour = gender)) +
  geom_point(size = 3.5, alpha = 0.8) +
  scale_colour_manual(values = runner_colours) +
  # Make a bit of extra space along the x + y-axis for labels
  scale_y_continuous(limits = c(1, 25.7), breaks = seq(5, 25, 5)) +
  scale_x_continuous(limits = c(13.5, 34), breaks = seq(15, 34, 1)) +
  # Labels for fastest man and woman 
  geom_label(
    data = whwr_fastest,
    aes(x = finish_hour, y = (max_hour_rank + 2), label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 1,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  theme(axis.title.y = element_text(margin = margin(r = -100))) +
  labs(
    subtitle = "",
    x = " ",
    y = "Number of runners"
    )

# Create chart showing finishing hour versus the runner's age.  
p_age_final <- whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = race_age, colour = gender)) +
  geom_point(size = 3.5, alpha = 0.6) +
  scale_colour_manual(values = runner_colours) +
  # Make a bit of extra space along the x + y-axis for labels
  scale_y_continuous(limits = c(15, 82), breaks = seq(20, 80, 10)) +
  scale_x_continuous(limits = c(13.5, 34), breaks = seq(15, 34, 1)) +
  # Label for oldest man
  geom_label(
    data = filter(whwr_oldest, gender == "Man"),
    aes(x = finish_hour, y = max_hour_age + 5.5, label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 0,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  # Label for oldest woman
  geom_label(
    data = filter(whwr_oldest, gender == "Woman"),
    aes(x = finish_hour, y = max_hour_age + 5.5, label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 1,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  # Labels for youngest man + woman
  geom_label(
    data = whwr_youngest,
    aes(x = finish_hour, y = min_hour_age - 5.5, label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 1,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  theme(axis.title.y = element_text(margin = margin(r = -73))) +
  labs(
    subtitle = "",
    x = "Finish time (hours)",
    y = "Age of runner"
    )


# Patch the charts together  
p_final <- p_gender_final /
  p_age_final

# Add plot titles
p_final <- p_final + 
  plot_annotation(
    title = "The West Highland Way Race: Finishing Times (hours), 2019",
    subtitle = "In 2019, 196 runners completed the 95 mile race from Milngavie (just north of Glasgow) to
Fort William in the Scottish Highlands.<br>Each dot represents a runner who completed the race (
  <b style='color: #32616a;'>153 men</b>
    /
    <b style='color: #e06508;'>43 women</b>
).",
  caption = "Tidy Tuesday: Week 42, 2021 | Data source: International Trail Running Association  &  westhighlandwayrace.org | Visualisation: @Fi_Lees"
  )

  
# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save chart as an image
ggsave("whwr_2019.png", p_final, width = 12, height = 12, units = "in", dpi = 300)


# Get the West Highland Way Race gpx data
whwr_route <- st_read("West_Highland_Way_Race_2019.gpx", layer = "track_points")

# Get start and end points of race
whwr_route_start_finish <- whwr_route %>% 
  mutate(
    start = min(track_seg_point_id),
    end = max(track_seg_point_id)
    ) %>% 
  filter(track_seg_point_id == start | track_seg_point_id == end) 

# Load boundary data for the UK
uk_sf <- ne_states(country = "united kingdom", returnclass = "sf")

# Filter data for Scotland
scot_sf <- filter(uk_sf, geonunit == "Scotland")

  
# Plot route of the West Highland Way Race on top of map of Scotland
p_whwr_map <- ggplot() +
  # Scotland boundary
  geom_sf(data = scot_sf, fill = "#cbe2e7",  colour = "#cbe2e7") +
  # Race route
  geom_sf(data = whwr_route, size = 0.05, colour = "#32616a") +
  # Start and end points
  geom_sf(data = whwr_route_start_finish, size = 2.5, colour = "#e06508") +
  # Trim longitude / latitude coordinates to fit mainland Scotland  
  coord_sf(xlim = c(-8, -1.5), ylim = c(54.8, 58.55)) +
  theme_void()

# Save map as an image  
ggsave("whwr_map.png", p_whwr_map, width = 2, height = 2.5, units = "in", dpi = 300)

# Get the final chart
chart <- image_read("whwr_2019.png")

# Get the map and then rescale it
map <- image_read("whwr_map.png")
map_scaled <- image_scale(map, "x500")

# Add the map to the chart and then save everything together
whwr_2019_with_map <- image_composite(chart, map_scaled, offset = "+3100")
whwr_2019_with_map <- image_border(whwr_2019_with_map, "#ffffff", "30x20")
image_write(whwr_2019_with_map, path = "whwr_2019_with_map.png", format = "png")

