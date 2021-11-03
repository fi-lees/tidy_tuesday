## Ultra Trail Running
The data this week was taken from the [International Trail Running Association (ITRA) website](https://itra.run/Races/FindRaceResults) and was made available to #TidyTuesday via [Benjamin Nowak](https://twitter.com/BjnNowak). The dataset provides a compilation of results from races that took place between 2012 - 2021 and focuses on 100 mile races (give or take a few miles). 


### Objectives
This week I'm going to focus on [The West Highland Way Race](https://westhighlandwayrace.org/); a 95 mile race from Milngavie (just north of Glasgow) to Fort William in the Scottish Highlands. It's one of the world’s longest established ultra-marathons and first took place in 1985. I chose this race because the start line is just a few miles from where I live and I'd like to know more about it.   

Due to the COVID-19 pandemic, the race was cancelled in 2020 and 2021, so I'm going to look at the most recent data available (2019). My objective is to visualise the distribution of finish times (in hours) by gender and age.


### Learning Points
This week I learned that a simple looking chart can take a long time to produce. I went down a number of rabbit holes looking at various chart types and at colours and fonts. My main learning points this week relate to:  

- **Dot Plots:** Not as simple as they seem. The y-axis in geom_dotplot() is not meaningful - it doesn't count the dots!.  

- **Colours:** I've done quite a bit of reading over the last few weeks about colour palettes, particularly in relation to [colour blindness and accessibility](https://blog.datawrapper.de/colorblindness-part1/). I used a great tool called [Viz Palette](https://projects.susielu.com/) that shows how various colour combinations look when viewed through the eyes of a person with a colour vision impairment. I also read an interesting article about [choosing colours when showing data by gender](https://blog.datawrapper.de/gendercolor/); it offers some ideas that don't involve the generic pink / blue combo.

- **Fonts:** I installed some [Google fonts](https://fonts.google.com/) on my laptop this week, so I wanted to try them out in my visualisations. I found a couple of great articles that helped me. The first one was about [choosing fonts for your data visualization](https://medium.com/nightingale/choosing-a-font-for-your-data-visualization-2ed37afea637). The second was about [understanding text size and resolution in ggplot2](https://www.christophenicault.com/post/understand_size_dimension_ggplot2/). This second one was really helpful in working out why charts within my R markdown file looked just fine, but looked awful when I saved them with ggsave().

- **Mapping Geographic Data:** Following on from #TidyTuesday [week 42](https://github.com/fi-lees/tidy_tuesday/tree/master/TT_2021_W42_Global_Seafood), I wanted some more practice mapping geographic data. [The West Highland Way Race website](https://westhighlandwayrace.org/map-and-gpx/) provides a gpx file with route data for the race. I learned how to read this type of data into R and plot it on a map.


### Load Packages
Load the required packages.  


```r
library(showtext)
library(sysfonts)
library(ggtext)
library(patchwork)
library(magick)
library(plotly)
library(sf)
library(rnaturalearth)
library(tidyverse)
```


### Import Data
Import this week's data.  


```r
# Read in race details from the Tidy Tuesday github page
race <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv"
  )

# For the ranking data, use Benjamin's link so that time values above 24hrs are not lost
# (reading time as an character string)
rankings <- readr::read_csv(
  "https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv",
  col_types = list(Time = "c")
  )
```

Have an initial look at the `race` data.  


```r
# Race data
glimpse(race)
```

```
## Rows: 1,207
## Columns: 13
## $ race_year_id   <dbl> 68140, 72496, 69855, 67856, 70469, 66887, 67851, 68241,~
## $ event          <chr> "Peak District Ultras", "UTMB®", "Grand Raid des Pyréné~
## $ race           <chr> "Millstone 100", "UTMB®", "Ultra Tour 160", "PERSENK UL~
## $ city           <chr> "Castleton", "Chamonix", "vielle-Aure", "Asenovgrad", "~
## $ country        <chr> "United Kingdom", "France", "France", "Bulgaria", "Turk~
## $ date           <date> 2021-09-03, 2021-08-27, 2021-08-20, 2021-08-20, 2021-0~
## $ start_time     <time> 19:00:00, 17:00:00, 05:00:00, 18:00:00, 18:00:00, 17:0~
## $ participation  <chr> "solo", "Solo", "solo", "solo", "solo", "solo", "solo",~
## $ distance       <dbl> 166.9, 170.7, 167.0, 164.0, 159.9, 159.9, 163.8, 163.9,~
## $ elevation_gain <dbl> 4520, 9930, 9980, 7490, 100, 9850, 5460, 4630, 6410, 31~
## $ elevation_loss <dbl> -4520, -9930, -9980, -7500, -100, -9850, -5460, -4660, ~
## $ aid_stations   <dbl> 10, 11, 13, 13, 12, 15, 5, 8, 13, 23, 13, 5, 12, 15, 0,~
## $ participants   <dbl> 150, 2300, 600, 150, 0, 300, 0, 200, 120, 100, 300, 50,~
```

Have an initial look at the `rankings` data.    


```r
# Ranking data
glimpse(rankings)
```

```
## Rows: 137,803
## Columns: 7
## $ RaceYearId  <dbl> 68140, 68140, 68140, 68140, 68140, 68140, 68140, 68140, 68~
## $ Rank        <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, NA, NA, NA, NA,~
## $ Runner      <chr> "VERHEUL Jasper", "MOULDING JON", "RICHARDSON Phill", "DYS~
## $ Time        <chr> "26:35:25", "27:00:29", "28:49:07", "30:53:37", "32:46:21"~
## $ Age         <dbl> 30, 43, 38, 55, 48, 31, 55, 40, 47, 29, 48, 47, 52, 49, 41~
## $ Gender      <chr> "M", "M", "M", "W", "W", "M", "W", "W", "M", "M", "M", "M"~
## $ Nationality <chr> "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "GBR", "G~
```


### Wrangle Data
Tidy-up the `rankings` data.   


```r
# Clean up ranking column names and use the Lubridate package to convert the time variable to 
# a period showing hours, minutes and seconds
rankings <- rankings %>% 
  janitor::clean_names() %>% 
  mutate(time = lubridate::hms(time)) %>% 
  mutate(time_in_seconds = lubridate::duration(as.numeric(time), unit = "seconds"), 
         finish_hour = lubridate::hour(time),
         finish_minute = lubridate::minute(time),
         .after = time
         )
```

Join the `race` data to the `ranking` data.  


```r
race_rankings <- rankings %>% 
  left_join(race, by = "race_year_id")
```

Create a filtered dataset that holds records for the West Highland Way Race in 2019.  


```r
whwr_race_rankings <- race_rankings %>% 
  filter(event == "West Highland Way Race" & date == lubridate::ymd(20190622))
```

Create some new variables that might be useful for exploration.  


```r
whwr_race_rankings <- whwr_race_rankings %>% 
  # Calculate the runner's approx age at the time of the race
  # (the age recorded for each runner is their age in 2021, not their age at the time of the race)
  mutate(race_age = age - (2021 - lubridate::year(date)), .after = age) %>%
  # If age is unavailable / invalid, set to NA 
  # (after reading info on the race website, I'm going assume values below 16 or over 100 are invalid)
  mutate(race_age = case_when(
    (!between(race_age, 16, 100)) ~ NA_real_, 
    TRUE ~ race_age)
    ) %>% 
  # Create an age group variable
  mutate(race_age_cut = cut(
    race_age,
    breaks = c(15, 19, 29, 39, 49, 59, 69, 100),
    labels = c("< 20", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", ">= 70")),
    .after = race_age) %>% 
  # Make gender and nationality factors
  mutate(
    gender = factor(gender, levels = c("M", "W"), labels = c("Man", "Woman")),
    nationality = factor(nationality)
    )
```


### Explore Data
Set-up a basic theme for charts before starting to explore the data.


```r
theme_set(theme_minimal())

theme_update(
  axis.title.x = element_text(margin = margin(t = 10, b = 10)),
  axis.title.y = element_text(margin = margin(r = 10, l = 10)),
  legend.position = "top",
  legend.justification = "left",
  panel.grid.minor = element_blank()
  )
```

Having a first look at the distributions of the variables I'm interested in:  

- More than 3/4 of the runners were men.  

- The most frequent age group was 40-49, but there's quite a nice distribution with a few runners in their 20s and a few who are 60 or older.  

- The vast majority of runners were from the UK (GBR).  

- The fastest runners finished in hour 15 of the race, the slowest in hour 34. There's a spike at hour 22.  



```r
p_gender <- whwr_race_rankings %>% 
  ggplot(aes(x = gender)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 200))

p_age_group <- whwr_race_rankings %>% 
  ggplot(aes(x = race_age_cut)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 200))

p_nationality <- whwr_race_rankings %>% 
  ggplot(aes(x = nationality)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 200))

p_finishing_hour <- whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 25))

# The patchwork package is loaded - use it to patch all the above charts together
(p_gender + p_age_group + p_nationality) /
  p_finishing_hour
```

<img src="README_files/figure-html/explore_distributions-1.png" title="Panel of bar charts showing the number of runners by gender, age group, nationality and finishing hour." alt="Panel of bar charts showing the number of runners by gender, age group, nationality and finishing hour." width="100%" style="display: block; margin: auto;" />

**Q.** When did men / women finish the race?    
**A.** The men ranged between hour 15 and 34 (median = hour 24). The women ranged between hour 17 and 34 (median = hour 27).  


```r
whwr_race_rankings %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    min_hour = min(finish_hour),
    Q1_hour = quantile(finish_hour, 0.25),
    median_hour = median(finish_hour),
    Q3_hour = quantile(finish_hour, 0.75),
    max_hour = max(finish_hour),
    mean_hour = mean(finish_hour),
    sd_hour = sd(finish_hour)
  )
```

```
## # A tibble: 2 x 9
##   gender     n min_hour Q1_hour median_hour Q3_hour max_hour mean_hour sd_hour
##   <fct>  <int>    <dbl>   <dbl>       <dbl>   <dbl>    <dbl>     <dbl>   <dbl>
## 1 Man      153       15      21          24    28         34      24.6    4.65
## 2 Woman     43       17      24          27    29.5       34      26.8    4.06
```


```r
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, fill = gender)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 25))
```

<img src="README_files/figure-html/gender_chart_1-1.png" title="Stacked bar chart showing the number of runners who finished in each hour, by gender." alt="Stacked bar chart showing the number of runners who finished in each hour, by gender." width="100%" style="display: block; margin: auto;" />

**Q.** When did the different age groups finish the race?    
**A.** The 30-39 age group had the lowest mean and median finish hour (median = hour 23), but the 20-29 and 40-49 groups were also fast (median = hour 24).  


```r
whwr_race_rankings %>% 
  group_by(race_age_cut) %>% 
  summarise(
    n = n(),
    min_hour = min(finish_hour),
    Q1_hour = quantile(finish_hour, 0.25),
    median_hour = median(finish_hour),
    Q3_hour = quantile(finish_hour, 0.75),
    max_hour = max(finish_hour),
    mean_hour = mean(finish_hour),
    sd_hour = sd(finish_hour)
  )
```

```
## # A tibble: 6 x 9
##   race_age_cut     n min_hour Q1_hour median_hour Q3_hour max_hour mean_hour
##   <fct>        <int>    <dbl>   <dbl>       <dbl>   <dbl>    <dbl>     <dbl>
## 1 20 - 29          7       18    23          24      24.5       29      23.7
## 2 30 - 39         51       15    19.5        23      28         34      23.6
## 3 40 - 49         82       16    21.2        24      28         34      24.8
## 4 50 - 59         49       18    23          27      29         34      26.7
## 5 60 - 69          6       25    27.2        28.5    30.5       33      28.8
## 6 >= 70            1       32    32          32      32         32      32  
## # ... with 1 more variable: sd_hour <dbl>
```


```r
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, fill = race_age_cut)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 25))
```

<img src="README_files/figure-html/age_chart_1-1.png" title="Stacked bar chart showing the number of runners who finished in each hour, by age_group." alt="Stacked bar chart showing the number of runners who finished in each hour, by age_group." width="100%" style="display: block; margin: auto;" />

Okay, so I now have a better idea of the distribution of finish hour and how this relates to gender and age group, but the stacked bar charts above are not very lovely and could be vastly improved. 

There are only 196 people who completed this race, so I'd like to create a chart that displays the runners as individuals (i.e. dots); I like the idea of a runner being able to pick themselves out in this chart and think "that's me! However, I still want to be able to see the overall distribution of finish hour.

My first thought was to create a dot plot using geom_dotplot. Using this approach it's possible to turn each individual into a dot and still see the finish hour distribution. However, there are a few drawbacks:  

- The y-scale isn't meaningful; geom_dotplot() doesn't count the dots and messing about with the y-axis scale didn't fully solve this issue. This geom is more complex than I first realised. I could hide this axis (and add count labels to each bar), but I'd prefer an axis showing the counts.  

- Now that I see the bars made of dots, I like the idea of ordering the dots so that it's possible to see the finishing position of the runners within each hour. The chart below just sticks all the women at the top of each bar.  



```r
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, fill = gender, colour = gender)) +
  geom_dotplot(dotsize = 0.2, stackratio = 1.3, method = "histodot", stackgroups = TRUE, binwidth = 1) + 
  #scale_y_continuous(limits = c(0, 25)) +
  scale_x_continuous(limits = c(15, 34), breaks = seq(15, 34, 1))
```

<img src="README_files/figure-html/gender_chart_2-1.png" title="Dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. Note that the y-axis is meaningless." alt="Dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. Note that the y-axis is meaningless." width="100%" style="display: block; margin: auto;" />

To show the count on the y-axis and order the runners by finishing time within each hour, I did two things:  

- I created a new variable  called `hour_rank`. It records the finishing position of each runner within each hour (it also acts as a count).   

- I switched to using geom_point() instead of geom_dotplot(). `hour_rank` is used to place each dot on the y-axis.  

The resulting chart is much closer to what I envisaged. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar.


```r
# Calculate the rank of each runner within each finishing hour
whwr_race_rankings <- whwr_race_rankings %>% 
  group_by(finish_hour) %>% 
  mutate(hour_rank = min_rank(time), .after = finish_minute) %>% 
  ungroup()
```


```r
# Draw the new chart
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = hour_rank, colour = gender)) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_x_continuous(limits = c(15, 34), breaks = seq(15, 34, 1)) +
  theme(panel.grid.major.x = element_blank())
```

<img src="README_files/figure-html/gender_chart_3-1.png" title="Ordered dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." alt="Ordered dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." width="100%" style="display: block; margin: auto;" />

Using the same principle, I created a similar chart for finish hour / age group. There are too many colours in this chart - it's a bit busy.  I tried using a single colour on a light-to-dark gradient, but I still found the chart tricky to process. I also tried binning age into fewer categories, but some of the detail I was interested in got lost.  


```r
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = hour_rank, colour = race_age_cut)) +
  # Try out square dots
  geom_point(size = 3.5, shape = 15) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_x_continuous(limits = c(15, 34), breaks = seq(15, 34, 1)) +
  theme(panel.grid.major.x = element_blank())
```

<img src="README_files/figure-html/age_chart_2-1.png" title="Ordered dot plot showing the number of runners who finished in each hour, by age group. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." alt="Ordered dot plot showing the number of runners who finished in each hour, by age group. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." width="100%" style="display: block; margin: auto;" />

In the end, I decided it would be more useful to retain the gender groupings and plot the finish hour data against the runner's age (a more conventional scatter plot). This makes it easier to see the age range of runners who finished in each hour. It has the added benefit of retaining the runner's gender information. Placed along side the gender chart I created above, I think this tells a more interesting story about the runners who finished in each hour. For example, you can see that everyone who finished by hour 24 was under 60 years of age.  


```r
whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = race_age, colour = gender)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_y_continuous(limits = c(20, 75), breaks = seq(20, 75, 5)) +
  scale_x_continuous(limits = c(15, 34), breaks = seq(15, 34, 1)) +
  theme(panel.grid.major.x = element_blank())
```

<img src="README_files/figure-html/age_chart_3-1.png" title="Scatter plot showing finishing hour versus runner's age. Each dot represents one runner, colour coded according to runner's gender." alt="Scatter plot showing finishing hour versus runner's age. Each dot represents one runner, colour coded according to runner's gender." width="100%" style="display: block; margin: auto;" />


### Visualise Data

Time to put it all together and build the final visualisation.  

First, set-up the fonts, colours and theme to be used in the final charts.  


```r
# Fonts
showtext_auto(enable = TRUE)
font_add_google("Lato")
font_add_google("Playfair Display")

# Colours
runner_colours = c("Man" = "#32616a", "Woman" = "#e06508")

# Theme
theme_set(theme_minimal())

theme_update(
  # Update fonts to be used
  text = element_text(colour = "gray30", family = "Lato"),
  plot.title = element_text(colour = "grey20", family = "Playfair Display", face = "bold", 
                            size = 18, margin = margin(t = 5, b = 5)),
  # Setting subtitle to ggtext:element_markdown() allows the colour / style of subtitle words to be changed
  plot.subtitle = element_markdown(size = 12.5, margin = margin(t = 10, b = 10), lineheight = 1.2),
  plot.caption = element_text(size = 10, margin = margin(t = 10), hjust = 0),
  plot.margin = margin(rep(4, 8)),
  plot.background = element_rect(color = "#ffffff", size = 0.5),
  axis.text = element_text(size = 12),
  axis.title.x = element_text(size = 12, margin = margin(t = 10, b = 10)),
  axis.title.y = element_text(size = 12, angle = 0, vjust = 1.01),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  legend.position = "none"
  )
```

Get some information about the fastest / youngest / oldest runners for chart labelling.  
  

```r
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
  select(runner, rank, time, finish_hour, finish_minute, hour_rank, 
         race_age, gender, nationality, max_hour_rank, runner_label) %>% 
  mutate(runner_label = str_c("Fastest ", runner_label)) %>% 
  ungroup()

# Youngest
whwr_youngest <- whwr_race_rankings %>% 
  group_by(gender) %>% 
  slice(which.min(race_age)) %>% 
  select(runner, rank, time, finish_hour, finish_minute, hour_rank, 
         race_age, gender, nationality, min_hour_age, runner_label) %>% 
  mutate(runner_label = str_c("Youngest ", runner_label)) %>% 
  ungroup()

# Oldest
whwr_oldest <- whwr_race_rankings %>% 
  group_by(gender) %>% 
  slice(which.max(race_age)) %>% 
  select(runner, rank, time, finish_hour, finish_minute, hour_rank, 
         race_age, gender, nationality, max_hour_age, runner_label) %>% 
  mutate(runner_label = str_c("Oldest ", runner_label)) %>% 
  ungroup()
```

Create the final chart showing the number of runners who finished in each hour, by gender.  


```r
p_gender_final <- whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = hour_rank, colour = gender, text = runner_label)) +
  geom_point(size = 3.5, alpha = 0.8) +
  # Use pre-defined colour palette
  scale_colour_manual(values = runner_colours) +
  # Make a bit of extra space along the x + y-axis for labels
  scale_y_continuous(limits = c(1, 25.7), breaks = seq(5, 25, 5)) +
  scale_x_continuous(limits = c(13.5, 34), breaks = seq(15, 34, 1)) +
  # Add labels for fastest man and woman 
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
  labs(subtitle = "",
       x = " ",
       y = "Number of runners")

p_gender_final
```

<img src="README_files/figure-html/gender_final-1.png" title="Ordered dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." alt="Ordered dot plot showing the number of runners who finished in each hour, by gender. Each dot represents one runner. The dots are ordered according to the runner's finishing position within each hour, with those who finished first shown at the bottom of each dotted bar." width="100%" style="display: block; margin: auto;" />

As a very quick aside, I've never tried out `plotly` before, but I saw a tweet by Yan Holtz about it this week. There was a link to a short [demo](https://holtzy.github.io/data_analysis_website/) . I'm keen to know how easy it is to make an interactive version of the above chart with tool-tips (that's why I added the `text = runner_label` bit in the above chart).   

I'll definitely play around a bit more with `plotly` in future weeks!  


```r
ggplotly(p_gender_final, tooltip="text")
```

```{=html}
<div id="htmlwidget-55215ee3760fa64c81fb" style="width:100%;height:576px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-55215ee3760fa64c81fb">{"x":{"data":[{"x":[15,16,16,17,17,17,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27,27,27,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,30,30,30,30,30,30,31,31,31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,33,33,33,34,34,34],"y":[1,1,2,1,2,4,1,2,3,4,5,6,8,9,1,2,3,4,5,6,7,8,9,10,1,2,3,4,6,7,1,2,3,4,5,6,7,8,9,10,11,12,13,1,2,3,4,5,6,7,9,10,11,12,13,15,16,17,18,19,20,21,22,1,3,4,6,7,8,9,10,11,1,2,3,4,5,7,8,1,2,3,4,5,6,7,8,9,10,12,1,2,3,4,5,7,8,9,11,1,2,3,4,5,6,7,8,10,14,1,4,8,9,10,11,2,3,5,6,7,10,11,12,13,1,2,4,5,7,9,2,3,4,5,6,7,8,9,11,12,14,1,2,3,6,7,8,1,3,4,2,4,5],"text":["man<br />15h 14m 42s<br />Overall position: 1","man<br />16h 9m 4s<br />Overall position: 2","man<br />16h 42m 18s<br />Overall position: 3","man<br />17h 33m 35s<br />Overall position: 4","man<br />17h 36m 14s<br />Overall position: 5","man<br />17h 53m 50s<br />Overall position: 7","man<br />18h 4m 11s<br />Overall position: 8","man<br />18h 12m 17s<br />Overall position: 9","man<br />18h 28m 43s<br />Overall position: 10","man<br />18h 29m 16s<br />Overall position: 11","man<br />18h 44m 40s<br />Overall position: 12","man<br />18h 44m 44s<br />Overall position: 13","man<br />18h 53m 41s<br />Overall position: 15","man<br />18h 56m 28s<br />Overall position: 16","man<br />19h 7m 59s<br />Overall position: 17","man<br />19h 9m 43s<br />Overall position: 18","man<br />19h 15m 50s<br />Overall position: 19","man<br />19h 30m 21s<br />Overall position: 20","man<br />19h 37m 24s<br />Overall position: 21","man<br />19h 48m 51s<br />Overall position: 22","man<br />19h 48m 56s<br />Overall position: 23","man<br />19h 52m 52s<br />Overall position: 24","man<br />19h 52m 58s<br />Overall position: 25","man<br />19h 55m 57s<br />Overall position: 26","man<br />20h 17m 31s<br />Overall position: 27","man<br />20h 32m 9s<br />Overall position: 28","man<br />20h 36m 30s<br />Overall position: 29","man<br />20h 37m 54s<br />Overall position: 30","man<br />20h 54m 46s<br />Overall position: 32","man<br />20h 59m 27s<br />Overall position: 33","man<br />21h 2m 38s<br />Overall position: 34","man<br />21h 9m 41s<br />Overall position: 35","man<br />21h 12m 11s<br />Overall position: 36","man<br />21h 13m 17s<br />Overall position: 37","man<br />21h 16m 50s<br />Overall position: 38","man<br />21h 20m 21s<br />Overall position: 39","man<br />21h 28m 43s<br />Overall position: 40","man<br />21h 39m 13s<br />Overall position: 41","man<br />21h 43m 31s<br />Overall position: 42","man<br />21h 48m 55s<br />Overall position: 43","man<br />21h 52m 16s<br />Overall position: 44","man<br />21h 54m 52s<br />Overall position: 45","man<br />21h 57m 19s<br />Overall position: 46","man<br />22h 1m 45s<br />Overall position: 48","man<br />22h 1m 47s<br />Overall position: 49","man<br />22h 2m 27s<br />Overall position: 50","man<br />22h 7m 33s<br />Overall position: 51","man<br />22h 13m 5s<br />Overall position: 52","man<br />22h 13m 43s<br />Overall position: 53","man<br />22h 15m 11s<br />Overall position: 54","man<br />22h 17m 30s<br />Overall position: 56","man<br />22h 18m 30s<br />Overall position: 57","man<br />22h 24m 35s<br />Overall position: 58","man<br />22h 32m 57s<br />Overall position: 59","man<br />22h 33m 50s<br />Overall position: 60","man<br />22h 42m 35s<br />Overall position: 62","man<br />22h 42m 43s<br />Overall position: 63","man<br />22h 44m 48s<br />Overall position: 64","man<br />22h 45m 18s<br />Overall position: 65","man<br />22h 45m 46s<br />Overall position: 66","man<br />22h 46m 23s<br />Overall position: 67","man<br />22h 46m 33s<br />Overall position: 68","man<br />22h 47m 37s<br />Overall position: 69","man<br />23h 0m 25s<br />Overall position: 71","man<br />23h 3m 20s<br />Overall position: 73","man<br />23h 5m 17s<br />Overall position: 74","man<br />23h 16m 29s<br />Overall position: 76","man<br />23h 19m 35s<br />Overall position: 77","man<br />23h 22m 39s<br />Overall position: 78","man<br />23h 48m 4s<br />Overall position: 79","man<br />23h 50m 10s<br />Overall position: 80","man<br />23h 50m 55s<br />Overall position: 81","man<br />24h 16m 0s<br />Overall position: 82","man<br />24h 16m 35s<br />Overall position: 83","man<br />24h 21m 43s<br />Overall position: 84","man<br />24h 24m 10s<br />Overall position: 85","man<br />24h 32m 39s<br />Overall position: 86","man<br />24h 52m 8s<br />Overall position: 88","man<br />24h 55m 7s<br />Overall position: 89","man<br />25h 0m 3s<br />Overall position: 92","man<br />25h 11m 25s<br />Overall position: 93","man<br />25h 11m 39s<br />Overall position: 94","man<br />25h 17m 17s<br />Overall position: 95","man<br />25h 24m 56s<br />Overall position: 96","man<br />25h 38m 32s<br />Overall position: 97","man<br />25h 38m 55s<br />Overall position: 98","man<br />25h 42m 20s<br />Overall position: 99","man<br />25h 50m 0s<br />Overall position: 100","man<br />25h 51m 40s<br />Overall position: 101","man<br />25h 56m 54s<br />Overall position: 103","man<br />26h 3m 59s<br />Overall position: 105","man<br />26h 7m 17s<br />Overall position: 106","man<br />26h 8m 50s<br />Overall position: 107","man<br />26h 11m 58s<br />Overall position: 108","man<br />26h 12m 13s<br />Overall position: 109","man<br />26h 31m 1s<br />Overall position: 111","man<br />26h 35m 38s<br />Overall position: 112","man<br />26h 36m 27s<br />Overall position: 113","man<br />26h 44m 47s<br />Overall position: 115","man<br />27h 10m 20s<br />Overall position: 118","man<br />27h 18m 6s<br />Overall position: 119","man<br />27h 28m 54s<br />Overall position: 120","man<br />27h 30m 46s<br />Overall position: 121","man<br />27h 31m 37s<br />Overall position: 122","man<br />27h 35m 17s<br />Overall position: 123","man<br />27h 38m 43s<br />Overall position: 124","man<br />27h 41m 15s<br />Overall position: 125","man<br />27h 41m 29s<br />Overall position: 127","man<br />27h 49m 43s<br />Overall position: 131","man<br />28h 1m 46s<br />Overall position: 132","man<br />28h 9m 54s<br />Overall position: 135","man<br />28h 38m 35s<br />Overall position: 139","man<br />28h 48m 38s<br />Overall position: 140","man<br />28h 50m 57s<br />Overall position: 141","man<br />28h 55m 11s<br />Overall position: 142","man<br />29h 13m 41s<br />Overall position: 145","man<br />29h 18m 0s<br />Overall position: 146","man<br />29h 19m 28s<br />Overall position: 148","man<br />29h 20m 35s<br />Overall position: 149","man<br />29h 26m 49s<br />Overall position: 150","man<br />29h 42m 48s<br />Overall position: 153","man<br />29h 45m 36s<br />Overall position: 154","man<br />29h 48m 44s<br />Overall position: 155","man<br />29h 52m 18s<br />Overall position: 156","man<br />30h 7m 35s<br />Overall position: 157","man<br />30h 12m 49s<br />Overall position: 158","man<br />30h 26m 21s<br />Overall position: 160","man<br />30h 32m 56s<br />Overall position: 161","man<br />30h 36m 44s<br />Overall position: 163","man<br />30h 48m 21s<br />Overall position: 165","man<br />31h 12m 56s<br />Overall position: 167","man<br />31h 15m 49s<br />Overall position: 168","man<br />31h 23m 0s<br />Overall position: 169","man<br />31h 26m 1s<br />Overall position: 170","man<br />31h 27m 41s<br />Overall position: 171","man<br />31h 40m 52s<br />Overall position: 172","man<br />31h 41m 17s<br />Overall position: 173","man<br />31h 44m 7s<br />Overall position: 174","man<br />31h 49m 12s<br />Overall position: 176","man<br />31h 51m 43s<br />Overall position: 177","man<br />31h 58m 16s<br />Overall position: 179","man<br />32h 16m 47s<br />Overall position: 180","man<br />32h 24m 17s<br />Overall position: 181","man<br />32h 28m 31s<br />Overall position: 182","man<br />32h 39m 52s<br />Overall position: 185","man<br />32h 40m 20s<br />Overall position: 186","man<br />32h 42m 13s<br />Overall position: 187","man<br />33h 27m 56s<br />Overall position: 188","man<br />33h 32m 6s<br />Overall position: 190","man<br />33h 46m 43s<br />Overall position: 191","man<br />34h 7m 42s<br />Overall position: 193","man<br />34h 21m 3s<br />Overall position: 195","man<br />34h 27m 51s<br />Overall position: 196"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(50,97,106,1)","opacity":0.8,"size":13.2283464566929,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(50,97,106,1)"}},"hoveron":"points","name":"Man","legendgroup":"Man","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[17,18,20,21,22,22,22,23,23,24,24,24,25,25,26,26,26,26,27,27,27,27,28,28,28,28,28,28,29,29,29,29,30,30,30,31,31,31,32,32,33,34,34],"y":[3,7,5,14,8,14,23,2,5,6,9,10,11,13,6,10,12,13,9,11,12,13,2,3,5,6,7,12,1,4,8,9,3,6,8,1,10,13,4,5,2,1,3],"text":["woman<br />17h 41m 9s<br />Overall position: 6","woman<br />18h 48m 35s<br />Overall position: 14","woman<br />20h 54m 43s<br />Overall position: 31","woman<br />21h 59m 51s<br />Overall position: 47","woman<br />22h 15m 58s<br />Overall position: 55","woman<br />22h 35m 6s<br />Overall position: 61","woman<br />22h 57m 2s<br />Overall position: 70","woman<br />23h 2m 27s<br />Overall position: 72","woman<br />23h 13m 27s<br />Overall position: 75","woman<br />24h 48m 25s<br />Overall position: 87","woman<br />24h 55m 27s<br />Overall position: 90","woman<br />24h 58m 46s<br />Overall position: 91","woman<br />25h 55m 19s<br />Overall position: 102","woman<br />25h 58m 20s<br />Overall position: 104","woman<br />26h 19m 25s<br />Overall position: 110","woman<br />26h 44m 43s<br />Overall position: 114","woman<br />26h 44m 57s<br />Overall position: 116","woman<br />26h 47m 38s<br />Overall position: 117","woman<br />27h 41m 19s<br />Overall position: 126","woman<br />27h 43m 42s<br />Overall position: 128","woman<br />27h 44m 50s<br />Overall position: 129","woman<br />27h 46m 59s<br />Overall position: 130","woman<br />28h 6m 33s<br />Overall position: 133","woman<br />28h 9m 12s<br />Overall position: 134","woman<br />28h 22m 7s<br />Overall position: 136","woman<br />28h 24m 43s<br />Overall position: 137","woman<br />28h 26m 9s<br />Overall position: 138","woman<br />28h 59m 31s<br />Overall position: 143","woman<br />29h 10m 15s<br />Overall position: 144","woman<br />29h 19m 0s<br />Overall position: 147","woman<br />29h 29m 17s<br />Overall position: 151","woman<br />29h 34m 7s<br />Overall position: 152","woman<br />30h 20m 57s<br />Overall position: 159","woman<br />30h 35m 12s<br />Overall position: 162","woman<br />30h 47m 19s<br />Overall position: 164","woman<br />31h 10m 44s<br />Overall position: 166","woman<br />31h 45m 3s<br />Overall position: 175","woman<br />31h 58m 2s<br />Overall position: 178","woman<br />32h 35m 8s<br />Overall position: 183","woman<br />32h 38m 43s<br />Overall position: 184","woman<br />33h 29m 20s<br />Overall position: 189","woman<br />34h 0m 17s<br />Overall position: 192","woman<br />34h 15m 42s<br />Overall position: 194"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(224,101,8,1)","opacity":0.8,"size":13.2283464566929,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(224,101,8,1)"}},"hoveron":"points","name":"Woman","legendgroup":"Woman","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"name":"Man","legendgroup":"Man","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"name":"Woman","legendgroup":"Woman","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.6268161062682,"r":5.31340805313408,"b":46.1602324616023,"l":160.398505603985},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(77,77,77,1)","family":"Lato","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[12.475,35.025],"tickmode":"array","ticktext":["15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34"],"tickvals":[15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34],"categoryorder":"array","categoryarray":["15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Lato","size":15.9402241594022},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":" ","font":{"color":"rgba(77,77,77,1)","family":"Lato","size":15.9402241594022}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.235,26.935],"tickmode":"array","ticktext":["5","10","15","20","25"],"tickvals":[5,10,15,20,25],"categoryorder":"array","categoryarray":["5","10","15","20","25"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"Lato","size":15.9402241594022},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Number of runners","font":{"color":"rgba(77,77,77,1)","family":"Lato","size":15.9402241594022}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(77,77,77,1)","family":"Lato","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"10c433024710":{"x":{},"y":{},"colour":{},"text":{},"type":"scatter"},"10c4248e6e6e":{"x":{},"y":{},"colour":{},"text":{},"label":{}}},"cur_data":"10c433024710","visdat":{"10c433024710":["function (y) ","x"],"10c4248e6e6e":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

Create the final chart showing finishing hour versus the runner's age, by gender.  


```r
p_age_final <- whwr_race_rankings %>% 
  ggplot(aes(x = finish_hour, y = race_age, colour = gender)) +
  geom_point(size = 3.5, alpha = 0.6) +
  # Use pre-defined colour palette
  scale_colour_manual(values = runner_colours) +
  # Make a bit of extra space along the x + y-axis for labels
  scale_y_continuous(limits = c(15, 82), breaks = seq(20, 80, 10)) +
  scale_x_continuous(limits = c(13.5, 34), breaks = seq(15, 34, 1)) +
  # Add label for oldest man
  geom_label(
    data = filter(whwr_oldest, gender == "Man"),
    aes(x = finish_hour, y = max_hour_age + 5.5, label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 0,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  # Add label for oldest woman
  geom_label(
    data = filter(whwr_oldest, gender == "Woman"),
    aes(x = finish_hour, y = max_hour_age + 5.5, label = runner_label),
    label.size = NA,
    size = 3.5,
    hjust = 1,
    vjust = 0.5,
    lineheight = 0.95
  ) +
  # Add labels for youngest man + woman
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
  labs(subtitle = "",
       x = "Finish time (hours)",
       y = "Age of runner")

p_age_final
```

<img src="README_files/figure-html/age_final-1.png" title="Scatter plot showing finishing hour versus runner's age. Each dot represents one runner, colour coded according to runner's gender." alt="Scatter plot showing finishing hour versus runner's age. Each dot represents one runner, colour coded according to runner's gender." width="100%" style="display: block; margin: auto;" />

Put the charts together.  


```r
# Use Patchwork package to patch gender and age charts together into one
p_final <- p_gender_final /
  p_age_final

# Add plot titles
p_final <- p_final + 
  plot_annotation(
    title = "The West Highland Way Race: Finishing Times (hours), 2019",
    # Colour code the words man / woman to match charts
    subtitle = "In 2019, 196 runners completed the 95 mile race from Milngavie (just north of Glasgow) to
  Fort William in the Scottish Highlands.<br>Each dot represents a runner who completed the race (
  <b style='color: #32616a;'>153 men</b>
  /
  <b style='color: #e06508;'>43 women</b>
  ).",
  caption = "Tidy Tuesday: Week 42, 2021 | Data source: International Trail Running Association  &  westhighlandwayrace.org | Visualisation: @Fi_Lees"
  )

p_final
```

<img src="README_files/figure-html/gender_age_final-1.png" title="Panel chart with (1) a dot plot showing the number of runners who finished the 2019 West Highland Way Race in each hour; (2) a scatter plot showing each runner's age versus the hour they finished. Each dot represents one runner (N = 196) and is colour coded by gender. The fastest man finished in 15 hours, 14 minutes; the fastest woman in 17 hours, 41 minutes. The youngest runner (man in 20s) finished in 18 hours, 28 minutes; the oldest (man in 70s) in 32 hours, 16 minutes." alt="Panel chart with (1) a dot plot showing the number of runners who finished the 2019 West Highland Way Race in each hour; (2) a scatter plot showing each runner's age versus the hour they finished. Each dot represents one runner (N = 196) and is colour coded by gender. The fastest man finished in 15 hours, 14 minutes; the fastest woman in 17 hours, 41 minutes. The youngest runner (man in 20s) finished in 18 hours, 28 minutes; the oldest (man in 70s) in 32 hours, 16 minutes." width="100%" style="display: block; margin: auto;" />
  
Save this visualisation as an image.


```r
# Tell showtext to use a DPI of 300 so that the text size is correct in the final saved chart.
showtext_opts(dpi = 300)

ggsave("whwr_2019.png", p_final, width = 12, height = 12, units = "in", dpi = 300)
```

One last thing. I'd like to add a small map showing the West Highland Way Race route. I have a gpx file with the route data; I want to import the gpx data and get it into a simple features (sf) format so that I can plot it on top of a map of Scotland. I can use the `sf` package to do this.


```r
# What layers are available in the West Highland Way Race gpx file?
st_layers("West_Highland_Way_Race_2019.gpx")
```

```
## Driver: GPX 
## Available layers:
##     layer_name     geometry_type features fields
## 1    waypoints             Point        0     23
## 2       routes       Line String        0     12
## 3       tracks Multi Line String        1     12
## 4 route_points             Point        0     25
## 5 track_points             Point   100746     27
```

```r
# Get the West Highland Way Race track_points data
whwr_route <- st_read("West_Highland_Way_Race_2019.gpx", layer = "track_points")
```

```
## Reading layer `track_points' from data source 
## Simple feature collection with 100746 features and 27 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -5.110399 ymin: 55.94148 xmax: -4.314881 ymax: 56.82212
## Geodetic CRS:  WGS 84
```

```r
# Keep columns with useful data - drop the rest
whwr_route <- whwr_route %>% 
  select(track_fid:time, gpxtpx_TrackPointExtension, geometry)

# Get the start and end points of race
whwr_route_start_finish <- whwr_route %>% 
  mutate(
    start = min(track_seg_point_id),
    end = max(track_seg_point_id)
    ) %>% 
  filter(track_seg_point_id == start | track_seg_point_id == end) 

# Use the ne_states() function from the rnaturalearth package to load boundary data for the UK
uk_sf <- ne_states(country = "united kingdom", returnclass = "sf")

# Get data for Scotland
scot_sf <- filter(uk_sf, geonunit == "Scotland")
```

Draw the map and route.   


```r
# Plot the route of the West Highland Way Race on top of the map of Scotland
p_whwr_map <- ggplot() +
  # Scotland boundary
  geom_sf(data = scot_sf, fill = "#cbe2e7",  colour = "#cbe2e7") +
  # Race route
  geom_sf(data = whwr_route, size = 0.05, colour = "#32616a") +
  # Start and end points
  geom_sf(data = whwr_route_start_finish, size = 2.5, colour = "#e06508") +
  # Trim longitude / latitude coordinates to fit mainland Scotland  
  coord_sf(xlim = c(-8, -1.5), ylim = c(54.8, 58.55)) +
  # Remove all standard lines / labels from chart
  theme_void()

p_whwr_map
```

<img src="README_files/figure-html/whwr_map-1.png" title="Map of Scotland showing the route of the West Highland Way Race." alt="Map of Scotland showing the route of the West Highland Way Race." width="100%" style="display: block; margin: auto;" />

Save the map as an image.  


```r
ggsave("whwr_map.png", p_whwr_map, width = 2, height = 2.5, units = "in", dpi = 300)
```

Using the `magick` package, add the map image to the top right-hand corner of the final chart image and save.  


```r
# Get the final chart
chart <- image_read("whwr_2019.png")

# Get the map and then rescale it
map <- image_read("whwr_map.png")
map_scaled <- image_scale(map, "x500")

# Add the map to the chart and then save
whwr_2019_with_map <- image_composite(chart, map_scaled, offset = "+3100")
whwr_2019_with_map <- image_border(whwr_2019_with_map, "#ffffff", "30x20")
image_write(whwr_2019_with_map, path = "whwr_2019_with_map.png", format = "png")
```


### Session Information

```r
sessionInfo()
```

```
## R version 4.1.1 (2021-08-10)
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
##  [1] forcats_0.5.1       stringr_1.4.0       dplyr_1.0.7        
##  [4] purrr_0.3.4         readr_2.0.2         tidyr_1.1.4        
##  [7] tibble_3.1.5        tidyverse_1.3.1     rnaturalearth_0.1.0
## [10] sf_1.0-3            plotly_4.10.0       ggplot2_3.3.5      
## [13] magick_2.7.3        patchwork_1.1.1     ggtext_0.1.1       
## [16] showtext_0.9-4      showtextdb_3.0      sysfonts_0.8.5     
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.0                 lubridate_1.8.0          bit64_4.0.5             
##  [4] httr_1.4.2               tools_4.1.1              backports_1.2.1         
##  [7] bslib_0.3.1              utf8_1.2.2               R6_2.5.1                
## [10] KernSmooth_2.23-20       rgeos_0.5-8              DBI_1.1.1               
## [13] lazyeval_0.2.2           colorspace_2.0-2         withr_2.4.2             
## [16] sp_1.4-5                 tidyselect_1.1.1         bit_4.0.4               
## [19] curl_4.3.2               compiler_4.1.1           textshaping_0.3.5       
## [22] cli_3.0.1                rvest_1.0.1              xml2_1.3.2              
## [25] labeling_0.4.2           sass_0.4.0               scales_1.1.1            
## [28] classInt_0.4-3           proxy_0.4-26             systemfonts_1.0.2       
## [31] digest_0.6.28            rmarkdown_2.11           pkgconfig_2.0.3         
## [34] htmltools_0.5.2          highr_0.9                dbplyr_2.1.1            
## [37] fastmap_1.1.0            htmlwidgets_1.5.4        rlang_0.4.11            
## [40] readxl_1.3.1             rstudioapi_0.13          farver_2.1.0            
## [43] jquerylib_0.1.4          generics_0.1.0           jsonlite_1.7.2          
## [46] crosstalk_1.1.1          vroom_1.5.5              magrittr_2.0.1          
## [49] s2_1.0.7                 Rcpp_1.0.7               munsell_0.5.0           
## [52] fansi_0.5.0              lifecycle_1.0.1          stringi_1.7.5           
## [55] yaml_2.2.1               snakecase_0.11.0         grid_4.1.1              
## [58] parallel_4.1.1           crayon_1.4.1             lattice_0.20-45         
## [61] haven_2.4.3              gridtext_0.1.4           hms_1.1.1               
## [64] knitr_1.36               pillar_1.6.3             markdown_1.1            
## [67] wk_0.5.0                 reprex_2.0.1             glue_1.4.2              
## [70] evaluate_0.14            data.table_1.14.2        modelr_0.1.8            
## [73] vctrs_0.3.8              tzdb_0.1.2               cellranger_1.1.0        
## [76] gtable_0.3.0             assertthat_0.2.1         xfun_0.26               
## [79] janitor_2.1.0            broom_0.7.9              e1071_1.7-9             
## [82] rnaturalearthhires_0.2.0 ragg_1.1.3               class_7.3-19            
## [85] viridisLite_0.4.0        units_0.7-2              ellipsis_0.3.2
```


