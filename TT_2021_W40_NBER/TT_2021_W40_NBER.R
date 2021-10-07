# Tidy Tuesday - W40 - National Bureau of Economic Research -------------------------------------------
#
# R-script for producing Tidy Tuesday data visualisations (2021, Week 40). 
#
# Date: 2021-10-01
#
# Author: Fiona Lees
#
# Data Source: nberwp R package (https://github.com/bldavies/nberwp)
#
# Data Description: The National Bureau of Economic Research (NBER) distributes more than 1,200 working 
# papers each year. The nberwp R package contains information about papers published between June 1973 
# and July 2021.
# -----------------------------------------------------------------------------------------------------

### Load Packages -------------------------------------------------------------------------------------
library(ggwordcloud)
library(tidytext)
library(tidyverse)


### Import Data ---------------------------------------------------------------------------------------
# Read in the papers data.
papers <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv"
)


### Wrangle Data --------------------------------------------------------------------------------------

# Add a variable showing the decade of publication
papers_new <- papers %>% 
  mutate(decade = factor(str_c(as.character(floor(year / 10) * 10),"s")), .before = year)

# Split each paper title into individual words 
title_words <- papers_new %>%
  # Split title and format so that each word is on a separate row
  unnest_tokens(word, title, drop = FALSE) %>% 
  # Remove standard stop words (e.g. and, the, to, of)
  anti_join(stop_words) %>% 
  # Remove numbers (e.g. 1982, 19)
  filter(nchar(gsub('[a-z.]', '', word)) == 0) %>%
  # Remove duplicate words (those that appear in a title more than once)
  group_by(paper, word) %>% 
  mutate(word_instance = row_number()) %>% 
  filter(word_instance == 1) %>% 
  ungroup() %>%
  select(-month, -word_instance)

# How may words?
length(title_words$word)

# How many unique words?
n_distinct(title_words$word)

# Most common words?
title_words %>% 
  count(word, sort = TRUE) %>%
  rename(word_freq = n) %>% 
  print(n = 20)

# Define additional stop words specific to this analysis
other_stopwords <- c(
  "analysis",
  "effect",
  "effects",
  "estimation",
  "evidence",
  "model",
  "models",
  "policy",
  "rate",
  "rates",
  "theory",
  "u.s"
)

# Remove other stop words specific to this analysis
title_words_reduced <- title_words %>% 
  filter(!word %in% other_stopwords) 

# How may words now?
length(title_words_reduced$word)

# How many unique words now?
n_distinct(title_words_reduced$word)

# Create summary table showing frequency of word use in each decade.
summary_by_decade <- title_words_reduced %>% 
  group_by(decade) %>% 
  mutate(n_papers = n_distinct(paper)) %>% 
  group_by(decade, word) %>% 
  summarise(
    n_papers = min(n_papers), 
    word_freq_n = n(),
    word_freq_pct = (word_freq_n / n_papers) 
    ) %>% 
  mutate(decade_rank = min_rank(desc(word_freq_n))) %>% 
  arrange(decade, decade_rank) %>% 
  ungroup()

# Show the top three words for each decade.
# Note that some words were used the same number of times and rank jointly with other words.
summary_by_decade %>% 
  filter(decade_rank <= 3) %>% 
  print(n = 20)

 
### Visualise Data ------------------------------------------------------------------------------------

# Set theme
theme_set(theme_minimal())

theme_update(text = element_text(colour = "grey40"),
             plot.title = element_text(colour = "black", size = 16, face = "bold", margin = margin(t = 5, b = 10)),
             strip.text.x = element_text(size = 12, vjust = 1),
             axis.text = element_text(size = 11),
             axis.title.x = element_text(margin = margin(t = 10, b = 10), hjust = 0.0),
             panel.grid.major.y = element_blank(),
             panel.grid.minor = element_blank(),
             plot.margin = margin(rep(8, 4)),
             legend.position = "none"
)

# Create new labels for facet strips
decade_labels <- c("1970s" = "1970s\n(June 1973 +)", 
                   "1980s" = "1980s", 
                   "1990s" = "1990s", 
                   "2000s" = "2000s", 
                   "2010s" = "2010s", 
                   "2020s" =  "2020s\n(until July 2021)")

# Create different labels for facet strips (no line breaks this time)
decade_labels_2 <- c("1970s" = "1970s (June 1973 +)", 
                     "1980s" = "1980s", 
                     "1990s" = "1990s", 
                     "2000s" = "2000s", 
                     "2010s" = "2010s", 
                     "2020s" =  "2020s (until July 2021)")

# Chart 1: Ten most common title words, by decade (number of times used)
chart_1 <- summary_by_decade %>% 
  filter(decade_rank <= 10) %>% 
  # Reorder the words by frequency within each decade so that they are shown in order
  mutate(word = reorder_within(word, word_freq_n, decade)) %>% 
  ggplot(aes(x = word_freq_n, y = word)) +
  geom_col(fill = "#482878FF", width = 0.5) +
  scale_y_reordered() +
  facet_wrap(~ decade, scales = "free_y", ncol = 3, labeller = labeller(decade = decade_labels)) +
  labs(title = "Ten most common words used in the titles of NBER papers, by decade",
       x = "Number of times word was used in a title",
       y = "",
       caption = "Tidy Tuesday: Week 40, 2021 | Data source: NBER (via the nberwp package) | Visualisation: @Fi_Lees")

chart_1

# Chart 2: Ten most common title words, by decade (percentage of times used)
chart_2 <- summary_by_decade %>% 
  filter(decade_rank <= 10) %>% 
  mutate(word = reorder_within(word, word_freq_pct, decade)) %>% 
  ggplot() +
  # Lollipop stick
  geom_segment(aes(x = 0, xend = word_freq_pct, y = word, yend = word), colour = "grey") +
  # Lollipop head
  geom_point(aes(x = word_freq_pct, y = word, colour = decade_rank), size = 3) +
  scale_colour_viridis_b(direction = 1, option = "D", end = 0.8) +
  scale_y_reordered() +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.12), 
    breaks = seq(0, 0.12, 0.02)
    ) +
  facet_wrap(~ decade, scales = "free_y", ncol = 3, labeller = labeller(decade = decade_labels)) +
  labs(title = "Ten most common words used in the titles of NBER papers, by decade",
       x = "Percentage of titles in which word was used",
       y = "",
       caption = "Tidy Tuesday: Week 40, 2021 | Data source: NBER (via the nberwp package) | Visualisation: @Fi_Lees")

chart_2
   
# Chart 3: Ten most common title words, by decade (word cloud)
set.seed(42)
chart_3 <- summary_by_decade %>% 
  filter(decade_rank <= 10) %>%
  ggplot(aes(label = word, size = word_freq_pct, colour = decade_rank)) +
  # Draw word cloud and set eccentricity of the spiral
  geom_text_wordcloud_area(eccentricity = 1) +
  # Set size of word cloud text area
  scale_size_area(max_size = 15) +
  scale_colour_viridis_b(direction = 1, option = "D", end = 0.8) +
  facet_wrap(~ decade, ncol = 3, labeller = labeller(decade = decade_labels_2)) +
  theme(plot.title = element_text(margin = margin(b = 20)),
        strip.text.x = element_text(size = 14, face = "bold", vjust = 1)) +
  labs(title = "Ten most common words used in the titles of NBER papers, by decade",
       caption = "Tidy Tuesday: Week 40, 2021 | Data source: NBER (via the nberwp package) | Visualisation: @Fi_Lees")

chart_3
# Save this visualisation:
ggsave("wordcloud.png", chart_3 , width = 12, height = 8, units = "in", dpi = 300)

 
# Chart 4: Ten most common title words, by decade (first time in top ten)

# Flag words that are a new entry to the top ten (never appeared in any previous decade's top ten)
top_ten_decade <- summary_by_decade %>% 
  filter(decade_rank <= 10) %>% 
  group_by(word) %>% 
  mutate(word_instance = row_number(),
         new_entry = factor(ifelse(word_instance == 1, "Yes", "No")),
         new_entry = factor(new_entry, levels = c("Yes", "No"))
  ) %>% 
  ungroup()

# Create chart
chart_4 <- top_ten_decade %>% 
  mutate(word = reorder_within(word, word_freq_pct, decade)) %>% 
  ggplot() +
  geom_segment(aes(x = 0, xend = word_freq_pct, y = word, yend = word), colour = "grey") +
  geom_point(aes(x = word_freq_pct, y = word, colour = new_entry), size = 3) +
  scale_colour_manual(values = c("#482878FF", "grey")) +
  scale_y_reordered() +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.12), 
    breaks = seq(0, 0.12, 0.02)
  ) +
  facet_wrap(~ decade, scales = "free_y", ncol = 3, labeller = labeller(decade = decade_labels)) +
  theme(
    legend.position = "top", 
    legend.justification = "left", 
    legend.title=element_text(size = 12),
    legend.text=element_text(size = 12)) +
  labs(title = "Ten most common words used in the titles of NBER papers, by decade",
       x = "Percentage of titles in which word was used",
       y = "",
       colour = "First time in top ten words list? ",
       caption = "Tidy Tuesday: Week 40, 2021 | Data source: NBER (via the nberwp package) | Visualisation: @Fi_Lees")

chart_4
