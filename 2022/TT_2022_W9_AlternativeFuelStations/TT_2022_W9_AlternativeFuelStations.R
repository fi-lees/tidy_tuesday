# Tidy Tuesday - 2022 W9 - Alternative Fuel Stations -------------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 9). 
#
# Date: 2022-03-01
#
# Author: Fiona Lees
#
# Data Source: U.S. Department of Transportation (US DOT) & Alternative Fuels Data Centre
#
# Data Description: Number and location of alternative fuel stations in the USA + 
#                   Vehicle registrations for all-electric vehicles (EVs) in the USA.

### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------

# Station data
stations <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv"
  ) %>% 
  janitor::clean_names()

# Supplementary data:
# Vehicle registration counts of all-electric vehicles (EVs) by state as at 31st December 2020
# (Only all-electric vehicles (EVs) are included; plug-in hybrid electric vehicles (PHEVs) are not included)
# Available at: https://afdc.energy.gov/data/10962
evs_reg_2020 <- readxl::read_xlsx(
  "data/10962-ev-registration-counts-by-state_6-11-21.xlsx",
  range = "EV Registration Counts!B3:C54",
  col_types = c("text", "numeric")
  ) %>% 
  janitor::clean_names() %>% 
  rename(n_ev_registrations = registration_count)


### Wrangle Data --------------------------------------------------------------------------------------

# Pull out data for electric stations. Only keep variables of interest (+ excluded data for Puerto Rico)
stations_elec <- stations %>% 
  filter(fuel_type_code == "ELEC" & state != "PR") %>% 
  select(objectid:station_name, city:zip, country, geocode_status, longitude, latitude, 
         status_code, expected_date, open_date, date_last_confirmed,
         access_code, access_detail_code, facility_type, owner_type_code,
         contains("ev_"))

# Exclude any stations that opened after the end of January 2021
stations_elec_Jan2021 <- stations_elec %>% 
  filter(!(status_code == "E" & open_date > lubridate::ymd("2021-01-31"))) %>% 
  filter(!(status_code == "P" & expected_date > lubridate::ymd("2021-01-31")))

# Calculate total number of electric charging ports in each station 
stations_elec_Jan2021 <- stations_elec_Jan2021 %>% 
  rowwise() %>% 
  mutate(n_ev_ports = sum(c(ev_level1_evse_num, ev_level2_evse_num, ev_dc_fast_count), na.rm = TRUE), 
         .after = ev_dc_fast_count)

# Get full state name / region of each US state
# (The base R {datasets} package has US State Facts and Figures)
us_states <- tibble(
  state_abb = state.abb,
  state_name = state.name,
  state_region = as.character(state.region)
)

# Add state name / region to dataset
stations_elec_Jan2021 <- stations_elec_Jan2021 %>% 
  rename(state_abb = state) %>% 
  left_join(us_states) %>% 
  mutate(
    state_name = ifelse(state_abb == "DC", "District of Columbia", state_name),
    state_region = ifelse(state_abb == "DC", "South", state_region)
    )

# Calculate number of electric stations and charging ports in each state (split by access type) 
stations_elec_Jan2021_by_state <- stations_elec_Jan2021 %>% 
  group_by(state_abb, state_name, state_region, access_code) %>% 
  summarise(n_stations = n(), n_ports = sum(n_ev_ports)) %>% 
  ungroup() %>% 
  mutate(state_abb = as_factor(state_abb))

# Add the EV registration data and calculate registrations per station and per charging port
stations_elec_Jan2021_by_state <- stations_elec_Jan2021_by_state %>% 
  left_join(evs_reg_2020, by = c("state_name" = "state")) %>% 
  mutate(
    ev_reg_per_station = round((n_ev_registrations / n_stations), 1),
    ev_reg_per_port = round((n_ev_registrations / n_ports), 1)
    ) %>% 
  select(state_abb, state_name, state_region, access_code, 
         n_ev_registrations, n_stations, ev_reg_per_station, n_ports, ev_reg_per_port)

# Pull out public charging station summary information
stations_elec_Jan2021_by_state_public <- stations_elec_Jan2021_by_state %>% 
  filter(access_code == "public") %>% 
  arrange(desc(ev_reg_per_port)) %>% 
  mutate(
    # Factor reorder states based on the number of EVs registered per port
    state_abb = fct_reorder(state_abb, desc(ev_reg_per_port)),
    # Calculate variables required to make bar widths in chart
    ymax = cumsum(n_ports),
    ymin = ymax - n_ports,
  ) %>% 
  ungroup()


### Visualise Data ------------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")


# Create initial Chart
p_initial <- stations_elec_Jan2021_by_state_public %>% 
  ggplot() +
  # Plot variable width bars
  geom_rect(
    aes(xmin = 0, xmax = ev_reg_per_port, ymin = ymin, ymax = ymax),
    colour = "white", 
    fill = "#2c6b67"
    ) +
  # Add state name to bars where the bar is deep enough to hold the text
  geom_text(
    data = filter(stations_elec_Jan2021_by_state_public, n_ports >= 1200), 
    aes(x = 0.2, y = ymin + ((ymax - ymin) / 2), label = state_name), 
    colour = "white",
    size = 3.5, 
    hjust = 0.0,
    vjust = 0.5
    ) +
  # Scales
  scale_x_continuous(
    limits = c(0, 25), 
    breaks = seq(0, 25, 5),
    expand = c(0, 0, 0.05, 0),
    ) +
  scale_y_continuous(
    limits = c(0, 90050), 
    breaks = seq(0, 90000, 10000),
    expand = c(0, 0, 0.01, 0),
    position = "right",
    label = scales::comma
    ) +
  # Styling
  theme_minimal() +
  theme(
    text = element_text(family = "Lato", colour = "grey40", size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.title = element_text(family = "Roboto Slab", colour = "black", size = 14, 
                              face = "bold", margin = margin(b = 5), hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(b = 30), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(t = 15), hjust = 0),
    plot.background = element_rect(colour = "white", fill = "white"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5)),
    axis.title.y.right = element_text(angle = 0, vjust = 1.030, hjust = 1, 
                                      margin = margin(l = -136), lineheight = 0.9),
    ) +
  # Titles
  labs(
    title = "How many all-electric vehicles are registered in each US state, per public charging port?",
    subtitle = "The height of each bar shows the number of electric vehicle charging ports available for public use in each\nUS state. The width shows the number of registered all-electric vehicles (EVs), per charging port.\n(January 2021)",
    x = "Number of registered all-electric vehicles (EVs), per charging port",
    y = "Number of electric vehicle\ncharging ports (public)",
    caption = "Visualisation: Fiona Lees (@Fi_Lees) | Source: US DOT + Alternative Fuels Data Centre | Tidy Tuesday: Week 9, 2022"
  ) 


# Create annotation text  
annotation <- stations_elec_Jan2021_by_state_public %>% 
  filter(state_abb %in% c("WY", "CA", "NJ")) %>% 
  # Create annotation text and variables for positioning
  mutate(
    text = str_c(state_name, 
                 " has ", 
                 format(n_ports, nsmall = 0, big.mark = ",", trim = TRUE), 
                 "\npublic charging ports,\nwith ",
                 format(ev_reg_per_port, nsmall = 1, big.mark = ",", trim = TRUE),
                 " EVs per port"),
    x = ifelse(state_abb %in% c("WY", "CA"), ev_reg_per_port + 2, ev_reg_per_port - 2.8),
    y = ifelse(state_abb %in% c("WY", "CA"), ymax, ymax + 6000)
    )

# Add annotation to final chart
p_final <- p_initial +
  # Annotation text
  geom_label(
    data = annotation,
    aes(x = x , y = y, label = text),
    family = "Lato",
    colour = "grey40", 
    fill = "white",
    size = 3.88, 
    hjust = 0,
    vjust = 0.8,
    label.padding = unit(0.2, "cm"),
    label.size = NA,
    lineheight = 1
  ) +
  # Annotation arrows
  annotate(
    geom = "segment", x = 4.1, xend = 2.2, y = 90040, yend = 90040, 
    arrow = arrow(length = unit(2, "mm"), type = "closed"), colour = "grey50"
    ) +
  annotate(
    geom = "segment", x = 16.9, xend = 15, y = 35000, yend = 35000, 
    arrow = arrow(length = unit(2, "mm"), type = "closed"), colour = "grey50"
    ) +
  annotate(
    geom = "segment", x = 23.4, xend = 23.4, y = 3300, yend = 1500, 
    arrow = arrow(length = unit(2, "mm"), type = "closed"), colour = "grey50"
    ) 


# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 320)

# Save plot as image
ggsave("EVsPerPort.png", p_final, width = 9, height = 13.5, units = "in", dpi = 320)

# Turn off showtext
showtext_auto(FALSE)
