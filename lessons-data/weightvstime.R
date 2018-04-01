# Filename: weightvstime.R
# Author:   Elliot Vosburgh
# Date:     1 April 2018
#
# Purpose:  Render graphs comparing weight 
#           for each species_id over time in years.

# Load necessary libraries
library(tidyverse)
library(directlabels)

# Import data from CSV file
surveys_complete <- read_csv("data_output/surveys_complete.csv")

# Store average weights of species_id by year

yearly_weight <- surveys_complete %>%
  group_by(year, species_id) %>%
  summarize(avg_weight = mean(weight))

# Store average weights of sexes of species_id by year

yearly_sex_weight <- surveys_complete %>%
  group_by(year, sex, species_id) %>%
  summarize(avg_weight = mean(weight))

# Render a weight-comparison plot with sexes split

ggplot(data = yearly_sex_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_grid(sex ~ .) +
  geom_dl(aes(label = species_id), method = list(dl.trans(x = x + 0.1), "last.points"), cex = 0.1) +
  theme_grey(base_family = "") +
  theme(axis.text.x = element_text(size = 11, angle = 0),
        axis.text.y = element_text(size = 11, angle = 0),
        text = element_text(size = 11),
        legend.position = "none") +
  labs(x = "Year",
       y = "Weight")

# Render a weight-comparison plot with sexes combined

ggplot(data = yearly_weight, aes(x = year, y = avg_weight, color = species_id)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_dl(aes(label = species_id), method = list(dl.trans(x = x + 0.2), "last.points"), cex = 1.0) +
  theme_grey(base_family = "") +
  theme(axis.text.x = element_text(size = 11, angle = 0),
        axis.text.y = element_text(size = 11, angle = 0),
        text = element_text(size = 11),
        legend.position = "none") +
  labs(x = "Year",
       y = "Weight")
