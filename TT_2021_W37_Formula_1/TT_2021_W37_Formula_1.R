# Tidy Tuesday - Formula One Motor Racing -------------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisations (2021, week 37). 
#
# Date: 2021-09-07 - Week 37
#
# Author: Fiona Lees
#
# Data Source: Ergast API (https://ergast.com/mrd/db/#csv)
#
# Data Description: The Ergast Developer API is an experimental web service that provides a historical 
# record of motor racing data for non-commercial purposes. The API provides data for the Formula One 
# series, from the beginning of the world championships in 1950 until present day.  
# -----------------------------------------------------------------------------------------------------


### Load Packages -------------------------------------------------------------------------------------
library(png)
library(patchwork)
library(ggtext)
library(tidyverse)
 

### Import Data ---------------------------------------------------------------------------------------

# This week I'm importing the data directly from the Ergast website and storing it locally. 

# Download
download.file("http://ergast.com/downloads/f1db_csv.zip", destfile = "data/f1db.zip")
# Unzip
unzip("data/f1db.zip", overwrite = TRUE, exdir = "data")

# Pull the raw csv files into a list, removing path and file extension info
raw_data <- map(fs::dir_ls("data/", glob = "*.csv"), read_csv) %>%
  set_names(nm = (basename(names(.)) %>% tools::file_path_sans_ext()))

# What do the files look like?
raw_data %>% str(max.level = 1)

# Add the files from the list to the environment
list2env(raw_data, .GlobalEnv)

 
### Wrangle Data --------------------------------------------------------------------------------------

# Join information about races, circuits, drivers and constructors to the results table.
# (not necessarily going to use all selected variables - just looking to see what the possibilities are)
results_new <- results %>%
  select(resultId:constructorId, grid, positionText:laps) %>%
  left_join(select(races, -url), by = "raceId") %>%                       
  rename("raceName" = "name") %>%
  left_join(select(circuits, c(-circuitRef, -url)), by = "circuitId") %>%
  rename("circuitName" = "name") %>%
  left_join(select(drivers, c(-number, -driverRef, -url)), by = "driverId") %>%
  rename("driverNationality" = "nationality") %>%
  left_join(select(constructors, -constructorRef, -url), by = "constructorId") %>%
  rename("constructorName" = "name", "constructorNationality" = "nationality") 

# Tidy-up the race name variable and order data as desired
results_new <-  results_new %>%
  # Strip the words 'Grand Prix' out of race name.
  mutate(raceName = gsub("Grand Prix", "", raceName)) %>%
  # Create a variable that combines race name and round
  mutate(raceNameNum = str_c(raceName, ": ", as.character(round))) %>%
  select(
    resultId,
    raceId,
    year,
    round,
    raceName,
    raceNameNum,
    date:time,
    circuitId,
    circuitName:alt,
    grid:laps,
    driverId,
    code:driverNationality,
    constructorId,
    constructorName,
    constructorNationality
  ) %>%
  arrange(year, round, positionOrder)

# Add driver standing info
results_new <-  results_new %>%
  left_join(driver_standings, by = c("raceId", "driverId"), suffix = c("Race", "Season")) %>%
  rename("positionOrderRace" = "positionOrder",
         "positionSeason" = "position")

glimpse(results_new)

 
### Set-up External Images and Colour Palette  --------------------------------------------------------

# Import a public domain image of a checkered flag to use in the visualisations. 
# Sourced from [the Noun Project](https://thenounproject.com/).
check_flag_image <- readPNG(
  "noun_check_flag.png", 
  native = TRUE, 
  info = TRUE
  )

# Define a colour palette for drivers and teams.
# Mercedes only chart
driver_colours_1 = c("HAM" = "#004d49", "BOT" = "#00ccc2")

# Tweak the drivers colour slightly when using for labels 
# (make darker to give better contrast against white background)
ham_lab_colour = "#004d49"
bot_lab_colour = "#00b3aa"
Other_lab_colour = "#7e7d82"

# Mercedes vs. Red Bull chart
driver_colours_2 = c("HAM" = "#006661", "BOT" = "#83afac", "VER" = "#db340a", "PER" = "#f37859")


### Visualise Lewis Hamilton vs. Valtteri Bottas 2021 -------------------------------------------------

# Set the colour for the y-axis labels to match the driver who finished ahead in each race
label_colours <- results_new %>%
  filter(year == 2021, constructorName == "Mercedes") %>%
  group_by(round) %>%
  mutate(driver_rank = rank(positionOrderRace)) %>%
  filter(driver_rank == 1) %>%
  mutate(lab_colour = ifelse(code == "BOT", bot_lab_colour, ham_lab_colour)) %>%
  select(round, lab_colour) %>%
  arrange(desc(round)) %>%
  ungroup()

# Build the visualisation
Mercedes_2021 <- results_new %>%
  filter(year == 2021, constructorName == "Mercedes") %>%
  ggplot(aes(
    x = positionOrderRace,
    y = fct_reorder(raceNameNum, desc(round)),
    colour = code
  )) +
  # Add a 'finishing line'
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = "#db340a", #dark orange
    size = 1
  ) +
  geom_line(aes(group = raceNameNum), colour = Other_lab_colour) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = driver_colours_1) +
  # Reverse the finishing position scale so that the driver with the best 
  # finishing position looks like they are in the lead
  scale_x_reverse(limits = c(20, 1), breaks = seq(1, 20, 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(
      size = 18,
      colour = ham_lab_colour,
      face = "bold",
      margin = margin(t = 5, r = 0, b = 5, l = 0)
    ),
    plot.subtitle = element_markdown(
      size = 14,
      colour = Other_lab_colour,
      margin = margin(t = 0, r = 0, b = 10, l = 0)
    ),
    plot.caption = element_markdown(
      colour = Other_lab_colour
      ),
    # Colour code the y-axis label to match the driver who finished first
    axis.text.y = element_text (size = 12, colour = label_colours$lab_colour),
    axis.text.x = element_text (size = 12, colour = Other_lab_colour),
    axis.title.x = element_text(
      size = 12,
      colour = ham_lab_colour,
      hjust = 0.955,
      margin = margin(t = 5, r = 0, b = 10, l = 0)
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(rep(8, 4)),
  ) +
  labs(
    title = "Mercedes 2021: Hamilton is outperforming his teammate",
    subtitle = "Finishing positions of
       <b style='color:#004d49'>Lewis Hamilton</b>
  and
<b style='color:#00b3aa'>Valtteri Bottas</b> in each race this season",
    x = "Position\n(1 = win)",
    y = "",
    caption = "Tidy Tuesday: Week 37, 2021 | Data source: Ergast API | Visualisation: @Fi_Lees"
  ) +
  # Add the checkered flag image to sit above the 'finishing line'
  patchwork::inset_element(
    p = check_flag_image,
    l = 0.939,
    b = 0.89,
    r = 0.99,
    t = 0.97,
    align_to = "full"
  )

Mercedes_2021

# Save this visualisation:
ggsave("Mercedes_2021.png", Mercedes_2021, width = 12, height = 8, units = "in")

  
### Visualise Mercedes vs. Red Bull 2021 ----------------------------------------------------------

# Build the visualisation
Mercedes_Redbull_2021 <- results_new %>%
  filter(year == 2021, constructorName %in% c("Mercedes", "Red Bull")) %>%
  ggplot(aes(
    x = positionOrderRace,
    y = fct_reorder(raceNameNum, desc(round)),
    colour = code
  )) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = Other_lab_colour,
    size = 1
  ) +
  geom_line(aes(group = raceNameNum), colour = Other_lab_colour) +
  geom_point(size = 4) +
  scale_colour_manual(values = driver_colours_2) +
  scale_x_reverse(limits = c(20, 1), breaks = seq(1, 20, 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(
      size = 16,
      colour = "black",
      face = "bold",
      margin = margin(t = 5, r = 0, b = 5, l = 0)
    ),
    plot.subtitle = element_markdown(
      size = 12,
      colour = Other_lab_colour,
      margin = margin(t = 0, r = 0, b = 10, l = 0)
    ),
    plot.caption = element_text(colour = Other_lab_colour),
    axis.text.y = element_text (size = 12, colour = "black"),
    axis.text.x = element_text (size = 12, colour = "black"),
    axis.title.x = element_text(
      size = 12,
      colour = "black",
      hjust = 0.955,
      margin = margin(t = 5, r = 0, b = 10, l = 0)
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(rep(8, 4)),
  ) +
  labs(
    title = "Formula 1 2021: <b style='color:#006661'>Mercedes</b> and <b style='color:#db340a'>Red Bull</b> battle for the top spot",
    subtitle = "Finishing positions of Mercedes and Red Bull  drivers in each race this season<br><br> 
  <b style='color:#006661'> Lewis Hamilton </b> | <b style='color:#83afac'> Valtteri Bottas </b> |
  <b style='color:#db340a'> Max Verstappen </b> | <b style='color:#f37859'> Sergio Pérez </b>",
    x = "Position\n(1 = win)",
    y = "",
    caption = "Tidy Tuesday: Week 37, 2021 | Data source: Ergast API | Visualisation: @Fi_Lees"
  ) +
  patchwork::inset_element(
    p = check_flag_image,
    l = 0.939,
    b = 0.85,
    r = 0.99,
    t = 0.95,
    align_to = "full"
  )

Mercedes_Redbull_2021

# Save this visualisation:
ggsave("Mercedes_Redbull_2021.png", Mercedes_Redbull_2021, width = 12, height = 8, units = "in")


