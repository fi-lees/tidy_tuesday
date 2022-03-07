## Alternative Fuel Stations
The [data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-03-01/readme.md#alternative-fuel-stations) this weeks comes from the [U.S. Department of Transportation (US DOT)](https://data-usdot.opendata.arcgis.com/datasets/usdot::alternative-fueling-stations/about) and is about the number and location of alternative fuel stations in the USA. It was provided to Tidy Tuesday by [Data is Plural](https://www.data-is-plural.com/archive/2021-09-08-edition/). 

I've also used some supplementary data about vehicle registrations for all-electric vehicles (EVs) in the USA. I sourced this from the [Alternative Fuels Data Centre (AFDC)](https://afdc.energy.gov/data/?q=electric). It provides vehicle registration counts for all-electric vehicles (EVs), by state, as at **31st December 2020**. This was the most up-to-date data available (published in June 2021). Note that it does not include plug-in hybrid electric vehicles (PHEVs).  


### Objectives
I'm going to focus on **electric** charging stations. I'm going to look at the number of **publicly available electric charging ports** in each state, compared to the number of all-electric vehicles (EVs) registered in each state. My aim is to calculate and show the ratio of EVs to charging ports. Note that there is sometimes more than one port at a charging station.


### Learning Points
The main thing I learned about this week was:  

- **Variable width bar charts:** When I started this week's Tidy Tuesday I planned to create a map. As I was thinking about the best way to represent of the vastly different number of electric vehicles / charging ports in each US state, I got distracted by a [variable width bar chart I saw on Twitter](https://twitter.com/HarryKennard/status/1498732280298233861). Long-story-short, I decided to scrap my map idea and try creating a variable width bar chart instead. There isn't a specific geom for a variable width bar chart (as far as I know anyway!), but [this chapter of the R Gallery Book](https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/ranking.html#barplot-with-variable-width---ggplot2) shows how to make one using geom_rect().


### Load Packages


```r
library(showtext)
library(tidyverse)
```


### Import Data
 
First, I'll import the alternative fuel stations data provided by Tidy Tuesday.


```r
# Station data
stations <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv"
  ) %>% 
  janitor::clean_names()
```

Next, I'll pull in the supplementary data about EV registrations. I've saved the data file in my local directory, but you can find it on the [AFDC website](https://afdc.energy.gov/data/10962). 


```r
# Supplementary data:
# Vehicle registration counts of all-electric vehicles (EVs) by state as at 31st December 2020
# (Only all-electric vehicles (EVs) are included; plug-in hybrid electric vehicles (PHEVs) are not included)
# Available at: https://afdc.energy.gov/data/10962
evs_reg_2020 <- readxl::read_xlsx(
  "data/10962-ev-registration-counts-by-state_6-11-21.xlsx",
  range = "EV Registration Counts!B3:C54",
  # Explicitly define the data type in each column to ensure it gets pulled in correctly
  col_types = c("text", "numeric")
  ) %>% 
  janitor::clean_names() %>% 
  rename(n_ev_registrations = registration_count)
```


### Wrangle Data

Let's start with the alternative fuel stations data. I'm only interested in the electric data today, so I'll filter out the data / variables I don't need for this analysis.  


```r
# Pull out data for electric stations, only keep variables of interest  
stations_elec <- stations %>% 
  filter(fuel_type_code == "ELEC") %>% 
  select(objectid:station_name, city:zip, country, geocode_status, longitude, latitude, 
         status_code, expected_date, open_date, date_last_confirmed,
         access_code, access_detail_code, facility_type, owner_type_code,
         contains("ev_"))

# Exclude any stations in Puerto Rico from this analysis (no EV registration data available for PR)
stations_elec <- stations_elec %>% 
  filter(state != "PR")
```

I want the electric stations data to be broadly comparable to the EV registrations data, so my plan was to exclude stations that opened after December 2020. However, the [data dictionary for stations](https://afdc.energy.gov/data_download/alt_fuel_stations_format) notes that `open_date` may be approximate (or added through automated data feeds from charging networks). Looking at the data, there does seem to be some weird spikes in the number of stations that opened on certain days (see 27th Jan 2021 as an example).  


```r
# Q. Any Available stations that opened after the end of Dec 2020?
# A. Yes (n >20,000). The number that opened on 27th Jan 2021 looks suspiciously high though (>10,000).
stations_elec %>%
  filter(status_code == "E" & open_date > lubridate::ymd("2020-12-31")) %>% 
  count(open_date, sort = TRUE)
```

```
## # A tibble: 362 x 2
##    open_date                  n
##    <chr>                  <int>
##  1 2021/01/27 00:00:00+00 10444
##  2 2021/10/25 00:00:00+00   841
##  3 2021/01/15 00:00:00+00   654
##  4 2021/09/21 00:00:00+00   191
##  5 2021/01/05 00:00:00+00   165
##  6 2021/09/02 00:00:00+00   143
##  7 2021/09/05 00:00:00+00   132
##  8 2021/09/08 00:00:00+00   132
##  9 2021/09/07 00:00:00+00   122
## 10 2021/10/27 00:00:00+00   122
## # ... with 352 more rows
```

```r
# Q. Any Planned stations where the expected opening date is after end of Dec 2020?
# A. Yes (n = 69).
stations_elec %>% 
  filter(status_code == "P" & expected_date > lubridate::ymd("2020-12-01")) %>% 
  count(expected_date, sort = TRUE)
```

```
## # A tibble: 26 x 2
##    expected_date              n
##    <chr>                  <int>
##  1 2021/11/01 00:00:00+00    15
##  2 2022/11/01 00:00:00+00    11
##  3 2021/06/30 00:00:00+00     6
##  4 2021/07/31 00:00:00+00     6
##  5 2021/08/31 00:00:00+00     5
##  6 2021/06/01 00:00:00+00     3
##  7 2021/01/31 00:00:00+00     2
##  8 2021/04/15 00:00:00+00     2
##  9 2022/03/01 00:00:00+00     2
## 10 2021/03/01 00:00:00+00     1
## # ... with 16 more rows
```

I'm going to make a pragmatic decision and extend the cut-off date to electric stations that opened before the end of January 2021. I'll also keep any stations that were **planned** to open before this point.  


```r
# Extend cut-off date, by a month and exclude any stations that opened after the end of January 2021
stations_elec_Jan2021 <- stations_elec %>% 
  filter(!(status_code == "E" & open_date > lubridate::ymd("2021-01-31"))) %>% 
  filter(!(status_code == "P" & expected_date > lubridate::ymd("2021-01-31")))
```

Next, I want to calculate the total number of charging ports at each station (level 1 + level 2 + fast).  


```r
# Calculate total number of electric charging ports in each station 
stations_elec_Jan2021 <- stations_elec_Jan2021 %>% 
  rowwise() %>% 
  mutate(n_ev_ports = sum(c(ev_level1_evse_num, ev_level2_evse_num, ev_dc_fast_count), na.rm = TRUE), 
         .after = ev_dc_fast_count)
```

The next job is to add the full state name to the `stations_elec_Jan2021` dataset.  


```r
# Get the full state name / region of each US state
# (The base R {datasets} package has US State Facts and Figures)
us_states <- tibble(
  state_abb = state.abb,
  state_name = state.name,
  state_region = as.character(state.region)
)

# Add state name / region to `stations_elec_Jan2021` dataset
stations_elec_Jan2021 <- stations_elec_Jan2021 %>% 
  rename(state_abb = state) %>% 
  left_join(us_states) %>% 
  mutate(
    state_name = ifelse(state_abb == "DC", "District of Columbia", state_name),
    state_region = ifelse(state_abb == "DC", "South", state_region)
    )
```

Next, I'm going to summarise the data by state and access_code (public / private). 


```r
# Calculate number of electric stations and charging ports in each state (split by access type) 
stations_elec_Jan2021_by_state <- stations_elec_Jan2021 %>% 
  group_by(state_abb, state_name, state_region, access_code) %>% 
  summarise(n_stations = n(), n_ports = sum(n_ev_ports)) %>% 
  ungroup() %>% 
  mutate(state_abb = as_factor(state_abb))
```

Now I'll add the EV registration data to this summary.  


```r
# Add the EV registration data and calculate registrations per station and per charging port
stations_elec_Jan2021_by_state <- stations_elec_Jan2021_by_state %>% 
  left_join(evs_reg_2020, by = c("state_name" = "state")) %>% 
  mutate(
    ev_reg_per_station = round((n_ev_registrations / n_stations), 1),
    ev_reg_per_port = round((n_ev_registrations / n_ports), 1)
    ) %>% 
  select(state_abb, state_name, state_region, access_code, 
         n_ev_registrations, n_stations, ev_reg_per_station, n_ports, ev_reg_per_port)
```

Finally, I'm going to pull out just the public charging station summary information. I'll also add the additional variables I'll need to construct the variable width bar chart.  


```r
stations_elec_Jan2021_by_state_public <- stations_elec_Jan2021_by_state %>% 
  filter(access_code == "public") %>% 
  arrange(desc(ev_reg_per_port)) %>% 
  mutate(
    # Factor reorder states based on the number of EVs registered per port
    state_abb = fct_reorder(state_abb, desc(ev_reg_per_port)),
    # Calculate variables required to make bar widths
    ymax = cumsum(n_ports),
    ymin = ymax - n_ports,
    ) %>% 
  ungroup()
```


### Visualise Data
Right, I'm ready to start building my visualisation.  

First, I'll load the fonts I want to use.  


```r
# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Roboto Slab")
```

Next, I'll construct the chart.  


```r
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

#p_initial 
```

I had planned to colour the bars by US region, but I thought the chart looked a bit "busy" when I tried this. I'm going to add some annotation text instead. I'll pick out the states at either end of the chart (Wyoming and New Jersey), plus California (because it has so many ports / EVs).  

Create the annotation text:  


```r
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
```

Now add the annotation to the chart:


```r
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
  
p_final
```

<img src="README_files/figure-html/final_chart-1.png" title="Variable width bar chart. The height of each bar shows the number of electric vehicle charging ports available for public use in each US state. The width of each bar shows the number of registered all-electric vehicles, per charging port. Wyoming has 165 public charging ports, with 2 electric vehicles per port. At the other end of the scale, New Jersey has 1,295 public charging ports, with 23.5 electric vehicles per port. California has the most public charging ports (28,713), with 14.8 electric vehicles per port. All results based on data as at January 2021." alt="Variable width bar chart. The height of each bar shows the number of electric vehicle charging ports available for public use in each US state. The width of each bar shows the number of registered all-electric vehicles, per charging port. Wyoming has 165 public charging ports, with 2 electric vehicles per port. At the other end of the scale, New Jersey has 1,295 public charging ports, with 23.5 electric vehicles per port. California has the most public charging ports (28,713), with 14.8 electric vehicles per port. All results based on data as at January 2021." width="100%" style="display: block; margin: auto;" />

Save the plot as an image.  


```r
# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 320)

# Save plot as image
ggsave("EVsPerPort.png", p_final, width = 9, height = 13.5, units = "in", dpi = 320)

# Turn off showtext
showtext_auto(FALSE)
```


### Session Information

```r
sessionInfo()
```

```
## R version 4.1.2 (2021-11-01)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 22000)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4    
##  [5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
##  [9] tidyverse_1.3.1 showtext_0.9-5  showtextdb_3.0  sysfonts_0.8.5 
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.2        sass_0.4.0        bit64_4.0.5       vroom_1.5.7      
##  [5] jsonlite_1.8.0    modelr_0.1.8      bslib_0.3.1       assertthat_0.2.1 
##  [9] highr_0.9         cellranger_1.1.0  yaml_2.3.5        pillar_1.7.0     
## [13] backports_1.4.1   glue_1.6.2        digest_0.6.29     rvest_1.0.2      
## [17] snakecase_0.11.0  colorspace_2.0-3  htmltools_0.5.2   pkgconfig_2.0.3  
## [21] broom_0.7.12      haven_2.4.3       scales_1.1.1      tzdb_0.2.0       
## [25] generics_0.1.2    farver_2.1.0      ellipsis_0.3.2    withr_2.4.3      
## [29] janitor_2.1.0     cli_3.2.0         magrittr_2.0.2    crayon_1.5.0     
## [33] readxl_1.3.1      evaluate_0.15     fs_1.5.2          fansi_1.0.2      
## [37] xml2_1.3.3        textshaping_0.3.6 tools_4.1.2       hms_1.1.1        
## [41] lifecycle_1.0.1   munsell_0.5.0     reprex_2.0.1      compiler_4.1.2   
## [45] jquerylib_0.1.4   systemfonts_1.0.4 rlang_1.0.1       grid_4.1.2       
## [49] rstudioapi_0.13   rmarkdown_2.11    gtable_0.3.0      DBI_1.1.2        
## [53] curl_4.3.2        rematch_1.0.1     R6_2.5.1          lubridate_1.8.0  
## [57] knitr_1.37        fastmap_1.1.0     bit_4.0.4         utf8_1.2.2       
## [61] ragg_1.2.2        stringi_1.7.6     parallel_4.1.2    Rcpp_1.0.8       
## [65] vctrs_0.3.8       dbplyr_2.1.1      tidyselect_1.1.2  xfun_0.29
```

