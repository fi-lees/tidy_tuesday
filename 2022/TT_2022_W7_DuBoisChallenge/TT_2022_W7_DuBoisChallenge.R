# Tidy Tuesday - 2022 W7 - W.E.B Du Bois Challenge No. 2 --------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisation (2022, Week 7). 
#
# Date: 2022-02-15
#
# Author: Fiona Lees
#
# Data Source: Anthony Starks 
#              https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022
#
# Data Description: Dataset for #DuBoisChallenge2022, Challenge No. 2 (Plate 22).

### Load Packages ------------------------------------------------------------------------------------
library(showtext)
library(ggforce)
library(tidyverse)


### Import Data --------------------------------------------------------------------------------------
df_raw <- readr::read_csv(
  "https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge02/data.csv"
) %>% 
  # Clean names
  janitor::clean_names() %>% 
  # Remove duplicate data for year 1880
  distinct()


### Wrangle Data -------------------------------------------------------------------------------------
df_plate22 = df_raw %>% 
  mutate(year = factor(year, levels = c("1899", "1895", "1890", "1885", "1880", "1875")),
         # Use valuation_dollars (desired area of each circle) to calculate radius
         radius = sqrt(valuation_dollars / pi) / 100,
         # Manually input alternative radius values (sized closer to the Do Bois original)
         radius_adjusted = c(9, 9.75, 14, 20.50, 21.25, 22)
         )


### Visualise Data -----------------------------------------------------------------------------------

# Fonts
showtext_auto(enable = TRUE)
font_add_google("Public Sans")

# Colours
col_red <- "#c31f35"
col_light_grey <- "#d9c7af"
col_gold <- "#edaf07"
col_blue <- "#2b4788"
col_tan <- "#af8f7e"
col_off_black <- "#141514"
col_off_white <- "#eae0d9"
col_parchment <- "#e3d2bd"
year_palette <- c(col_red, col_light_grey, col_gold, col_blue, col_tan, col_off_black)

# Create chart
p_plate22 <- ggplot(df_plate22) +
  # Circles
  geom_circle(aes(x0 = 0, y0 = 0, r = radius_adjusted, fill = year), 
              colour = col_tan
              ) +
  scale_fill_manual(values = year_palette) +
  # Year labels - size, colour and nudge label for each year separately  
  geom_text(aes(x = 0, y = 0 - radius_adjusted, label = year), 
            size = c(2.9, 2.4, 2.9, 2.9, 2.4, 2.4), 
            colour = c(col_off_white, col_off_black, col_off_white, col_off_black, col_off_black, col_off_black),
            nudge_y = c(1, 0.35, 0.9, 0.8, 0.4, 0.4)
            ) +
  # Label for centre of 1875 circle
  geom_text(data = df_plate22 %>% filter(year == 1875), 
            aes(x = 0, y = 0, label = scales::dollar(valuation_dollars)), 
            colour = col_off_white, size = 3
            ) +
  # Wedges + wedge labels
  # 1880
  geom_arc_bar(aes(x0 = -3, y0 = -3, r0 = 5.5, r = 0.3, start = 3.8, end = 4.4), 
               fill = col_tan, colour = col_tan
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1880), 
            aes(x = -5.8, y = -5, label = scales::dollar(valuation_dollars), angle = 35), 
            colour = col_off_black, size = 2.4
            ) +
  # 1885
  geom_arc_bar(aes(x0 = 3, y0 = -3, r0 = 8, r = 0.3, start = 2.1, end = 2.7), 
               fill = col_blue, colour = col_blue
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1885), 
            aes(x = 6, y = -6.2, label = scales::dollar(valuation_dollars), angle = -45), 
            colour = col_off_white, size = 2.6
            ) +
  # 1890
  geom_arc_bar(aes(x0 = 5, y0 = 0, r0 = 14, r = 0.7, start = 1.4, end = 1.7), 
               fill = col_gold, colour = col_gold
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1890), 
            aes(x = 14.5, y = 0.1, label = scales::dollar(valuation_dollars), angle = 0), 
            colour = col_off_black, size = 3
            ) +
  # 1895
  geom_arc_bar(aes(x0 = 3, y0 = 3, r0 = 16.75, r = 1, start = 0.55, end = 0.8), 
               fill = col_light_grey, colour = col_light_grey
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1895), 
            aes(x = 11.2, y = 13.5, label = scales::dollar(valuation_dollars), angle = 51), 
            colour = col_off_black, size = 3
            ) +
  # 1899
  geom_arc_bar(aes(x0 = -3, y0 = 3, r0 = 17.5, r = 1, start = 5.5, end = 5.75), 
               fill = col_red, colour = col_red
               ) +
  geom_text(data = df_plate22 %>% filter(year == 1899), 
            aes(x = -11.5, y = 14.1, label = scales::dollar(valuation_dollars), angle = -51), 
            colour = col_off_black, size = 3
            ) +
  # Co-ordinates
  coord_fixed() +
  # Theme
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "Public Sans", colour = col_off_black),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 40), 
                              hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(size = 8, margin = margin(t = 40), 
                                hjust = 0.5, lineheight = 1.1),
    plot.background = element_rect(fill = col_parchment, colour = NA),
    plot.margin = margin(t = 20, r = 20, b = 0, l = 20)
    ) +
  # Titles
  labs(
    title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY AFRICAN AMERICANS IN GEORGIA .",
    caption = "VISUALISATION: FIONA LEES | @Fi_Lees\nSOURCE: ANTHONY STARKS\nTIDY TUESDAY: WEEK 7, 2022 | DU BOIS CHALLENGE: NO.2, 2022\n"
    )

# Tell showtext to use DPI of 300 so text size is correct when chart is saved as an image
showtext_opts(dpi = 300)

# Save plot as image
ggsave("DuBois_Plate22.png", p_plate22, height = 11.5, width = 10, units = "in", dpi = 300)

# Turn off showtext
showtext_auto(FALSE)



