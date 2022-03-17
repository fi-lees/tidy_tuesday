# Tidy Tuesday - 2022, W1 -  Formula 1 (Bring Your Own Data) -----------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 1). 
#
# Date: 2022-03-15
#
# Author: Fiona Lees
#
# Data Source: Ergast API 
#              (https://ergast.com/mrd/)
#

### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(ggrepel)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------

# Download data to local drive
#download.file("http://ergast.com/downloads/f1db_csv.zip", destfile = "Data/f1db.zip")

# Unzip data
#unzip("data/f1db.zip", overwrite = TRUE, exdir = "Data")

# Pull all csv files into a list, removing path and file extension info
f1_data <- map(fs::dir_ls("Data/", glob = "*.csv"), read_csv) %>%
  set_names(nm = (basename(names(.)) %>% tools::file_path_sans_ext()))

# Load data of interest
races <- f1_data$races
results <- f1_data$results
driver_standings <- f1_data$driver_standings
drivers <- f1_data$drivers
constructor_standings <- f1_data$constructor_standings
constructors <- f1_data$constructors


### Wrangle Data -------------------------------------------------------------------------------------

# Get race details for 2021
races_2021 <- races %>% 
  filter(year == 2021) %>% 
  rename("raceName" = "name") %>% 
  mutate(
    # Strip 'Grand Prix' out of race name
    raceName = gsub(" Grand Prix", "", raceName),
    # Create variable that combines race name and round
    raceNameNum = str_c(raceName, ": ", as.character(round)) 
    ) %>% 
  select(-date, -time, -url) %>% 
  arrange(round)   

# Get driver/constructor mapping for each 2021 race
driver_mapping_2021 <- results %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  select(raceId:constructorId, positionOrder, points) %>% 
  rename(
    "racePosition" = "positionOrder",
    "racePoints" = "points"
    )
  
# Get driver standing info for 2021
driver_standings_2021 <- driver_standings %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  # Add driver info
  left_join(
    select(drivers, c(driverId, code, surname, nationality)), 
    by = "driverId"
    ) %>% 
  rename(
    "driverPoints" = "points",
    "driverPosition" = "position",
    "driverWins" = "wins",
    "driverCode" = "code",
    "driverSurname" = "surname",
    "driverNationality" = "nationality"
    ) %>% 
  # Add driver/constructor mapping for each race
  left_join(driver_mapping_2021, by = c("driverId", "raceId")) %>% 
  select(driverId, driverCode:driverNationality, raceId:racePoints, -positionText) %>% 
  arrange(driverId, raceId)

# Kubica stood in for Raikkonen in a couple of races
# Assign a constructor (Alfa Romeo) to these drivers for the races they didn't drive in
driver_standings_2021 <- driver_standings_2021 %>% 
  mutate(constructorId = ifelse(
    is.na(constructorId) & driverCode %in% c("KUB", "RAI"), 
    51, 
    constructorId)
    )

# Get constructor standing info for 2021
constructor_standings_2021 <- constructor_standings %>% 
  filter(raceId %in% races_2021$raceId) %>% 
  # Add constructor info
  left_join(
    select(constructors, c(-url)), 
    by = "constructorId"
    ) %>% 
  rename(
    "constructorPoints" = "points",
    "constructorPosition" = "position",
    "constructorWins" = "wins",
    "constructorName" = "name",
    "constructorNationality" = "nationality"
    ) %>% 
  select(constructorId, constructorName, constructorNationality, raceId:constructorWins, -positionText) %>% 
  arrange(constructorId, raceId)

# Join all the 2021 data together in one tibble
race_driver_constructor_2021 <- races_2021 %>% 
  right_join(driver_standings_2021, by = "raceId") %>% 
  left_join(constructor_standings_2021, by = c("constructorId", "raceId")) %>% 
  select(raceId:driverNationality, racePosition, racePoints, driverPoints:constructorWins) %>% 
  arrange(round, racePosition)


### Visualise Data ----------------------------------------------------------------------------------

# Load fonts
showtext_auto(enable = TRUE)
font_add_google("Titillium Web")

# Finalise data to be used in chart
data_for_chart <- race_driver_constructor_2021 %>% 
  # Select data for the final race of the season
  slice(which(round == max(round))) %>% 
  mutate(
    # Set a flag to identify reserve drivers (Kubica)
    reserveDriver = ifelse(driverSurname == "Kubica", TRUE, FALSE),
    # Change driver names to upper case, adding minor tweaks as required
    driverSurname = toupper(ifelse(driverSurname == "Kubica", "(reserve driver)\nKubica", driverSurname)),
    # Make minor tweaks to constructor names
    constructorName = case_when(
      constructorName == "Alpine F1 Team" ~ "Alpine",
      constructorName == "Haas F1 Team" ~ "Haas",
      TRUE ~ constructorName),
    # Add constructor name and position together to create a new variable
    constructorStanding = toupper(factor(str_c(as.character(constructorPosition), ":  ", constructorName))),
    # Reorder this new variable according to final constructor standings
    constructorStanding = fct_reorder(constructorStanding, -constructorPosition)
    )

# Build the chart
p_final <- data_for_chart %>% 
  ggplot(aes(x = driverPosition, y = constructorStanding)) +
  # Line to connect drivers in each team
  geom_line(aes(group = constructorId), colour = "grey80", size = 1.5) +
  # Dot for each driver (reserve drivers in grey)
  geom_point(aes(colour = reserveDriver), size = 4) +
  scale_colour_manual(values = c("#e10600", "grey80")) +
  # Driver name text labels
  geom_label_repel(
    aes(label = driverSurname),
    family = "Titillium Web",
    size = 3.2, 
    colour = "grey20",
    fill = "white",
    hjust = 0.5,
    vjust = 0.5,
    nudge_y = 0.25,
    label.padding = unit(0.2, "cm"),
    label.size = NA,
    lineheight = 1
    ) +
  # Scales
  scale_y_discrete(position = "right") +
  scale_x_reverse(limits = c(21, 1), breaks = seq(1, 21, 1)) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Titillium Web", colour = "grey20", size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.title = element_text(colour = "#e10600", size = 24, face = "bold.italic", 
                              margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    axis.title.y.right = element_text(vjust = 1.03, hjust = 0, angle = 0, margin = margin(l = -69, r = 0)),
    axis.title.x = element_text(hjust = 0.955, margin = margin(t = 10)),
    legend.position = "none"
    ) +
  # Titles
  labs(
    title = "FORMULA 1: 2021 STANDINGS",
    subtitle = "FINAL CONSTRUCTOR AND DRIVER STANDINGS FOR THE 2021 SEASON",
    x = "DRIVER POSITION",
    y = "CONSTRUCTOR\nPOSITION",
    caption = "VISUALISATION: FIONA LEES (@FI_LEES) | SOURCE: ERGAST API | TIDY TUESDAY: WEEK 1, 2022"
    )


# Tell showtext to use DPI of 320 so text size is correct when chart is saved as an image
showtext_opts(dpi = 320)

# Save plot as image
ggsave("F1Standings_2021.png", p_final, width = 11, height = 8, units = "in", dpi = 320)

# Turn off showtext
showtext_auto(FALSE)

