# Tidy Tuesday - W37 - Formula One Motor Racing -------------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisations (2021, week 37). 
#
# Date: 2021-09-27
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

# Import the data directly from the Ergast website. 

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

 
### Set-up External Images, Colour Palette and Theme --------------------------------------------------

# Import a public domain image of a checkered flag to use in the visualisations. 
# Sourced from [the Noun Project](https://thenounproject.com/).
check_flag_image <- readPNG(
  "noun_check_flag.png", 
  native = TRUE, 
  info = TRUE)

# Define a colour palette for drivers and teams
# Mercedes only chart
driver_colours_1 = c("HAM" = "#004d46", "BOT" = "#00b3a4")
mer_lab_colour = "#001a17"
ham_lab_colour = "#004d46"
bot_lab_colour = "#00998c" # make one shade darker than point colour to make text easier to read
# Mercedes vs. Red Bull chart
driver_colours_2 = c("HAM" = "#006661", "BOT" = "#83afac", "VER" = "#db340a", "PER" = "#f58b70")

# Set the theme for the visualisations
theme_set(theme_minimal())

theme_update(text = element_text(colour = "grey50"),
             panel.grid.minor = element_blank(),
             plot.margin = margin(rep(8, 4)),
             axis.text = element_text(size = 12),
             axis.title = element_text(size = 12),
             axis.title.x = element_text(hjust = 0.955, margin = margin(t = 5, b = 10)),
             plot.title = element_text(size = 16, face = "bold", margin = margin(t = 5, b = 5)),
             plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
             legend.position = "none"
             )


### Visualise Data ------------------------------------------------------------------------------------

# Mercedes: Lewis Hamilton vs. Valtteri Bottas 2021

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
    x = fct_reorder(raceNameNum, desc(round)),
    y = positionOrderRace,
    colour = code
  )) +
  # Add a 'finishing line'
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "orangered2",
    size = 1
  ) +
  geom_line(aes(group = raceNameNum)) +
  geom_point(size = 4) +
  scale_colour_manual(values = driver_colours_1) +
  # Reverse the finishing position scale so that the driver with the best 
  # finishing position looks like they are in the lead
  scale_y_reverse(limits = c(20, 1), breaks = seq(1, 20, 1)) +
  theme(
    plot.title = element_text(colour = mer_lab_colour),
    plot.subtitle = element_markdown(),
    # Colour code the y-axis label to match the driver who finished first
    axis.text.y = element_text(colour = label_colours$lab_colour),
    axis.title.x = element_text(colour = mer_lab_colour)
  ) +
  labs(
    title = "Mercedes 2021: Hamilton is outperforming his teammate",
    subtitle = "Finishing positions of
        <b style='color:#004d46'>Lewis Hamilton</b>
        and
        <b style='color:#00998c'>Valtteri Bottas</b> in each race this season",
    y = "Position\n(1 = win)",
    x = "",
    caption = "Tidy Tuesday: Week 37, 2021 | Data source: Ergast API | Visualisation: @Fi_Lees"
  ) +
  coord_flip() +
  # Add the checkered flag image to sit above the 'finishing line'
  patchwork::inset_element(
    p = check_flag_image,
    l = 0.94,
    b = 0.89,
    r = 0.99,
    t = 0.99,
    align_to = "full"
  ) 

Mercedes_2021
# Save this visualisation
ggsave("Mercedes_2021.png", Mercedes_2021, width = 13, height = 9, units = "in", dpi = 300, scale = 0.8)


# Mercedes vs. Red Bull 2021

# Build the visualisation
Mercedes_Redbull_2021 <- results_new %>%
  filter(year == 2021, constructorName %in% c("Mercedes", "Red Bull")) %>%
  ggplot(aes(
    x = fct_reorder(raceNameNum, desc(round)),
    y = positionOrderRace,
    colour = code
  )) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "grey50",
    size = 1
  ) +
  geom_line(aes(group = raceNameNum), colour = "grey50") +
  geom_point(size = 4) +
  scale_colour_manual(values = driver_colours_2) +
  scale_y_reverse(limits = c(20, 1), breaks = seq(1, 20, 1)) +
  theme(
    plot.title = element_markdown(colour = "black"),
    plot.subtitle = element_markdown(),
    axis.text.y = element_text (colour = "black"),
    axis.text.x = element_text (colour = "black"),
    axis.title.x = element_text(colour = "black")
  ) +
  labs(
    title = "Formula 1 2021: <b style='color:#006661'>Mercedes</b> and <b style='color:#db340a'>Red Bull</b> battle for the top spot",
    subtitle = "Finishing positions of Mercedes and Red Bull  drivers in each race this season<br><br> 
       <b style='color:#006661'> Lewis Hamilton </b> | <b style='color:#639c98'> Valtteri Bottas </b> |
       <b style='color:#db340a'> Max Verstappen </b> | <b style='color:#f26440'> Sergio Pérez </b>",
    y = "Position\n(1 = win)",
    x = "",
    caption = "Tidy Tuesday: Week 37, 2021 | Data source: Ergast API | Visualisation: @Fi_Lees"
  ) +
  coord_flip() +
  patchwork::inset_element(
    p = check_flag_image,
    l = 0.94,
    b = 0.85,
    r = 0.99,
    t = 0.95,
    align_to = "full"
  )

Mercedes_Redbull_2021
# Save this visualisation
ggsave("Mercedes_Redbull_2021.png", Mercedes_Redbull_2021, width = 13, height = 9, units = "in", dpi = 300, scale = 0.8)







 