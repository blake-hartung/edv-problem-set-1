library(tidyverse)

# Load fastfood dataset from openintro
library(openintro)
data(fastfood)

# Horizontal boxplots categorized by restaurant
boxplot_by_rest <- ggplot(fastfood, aes(x=calories, y=restaurant)) +
  geom_boxplot()

# cretae hist
histograms <- ggplot(fastfood, aes(x=calories)) +
  geom_histogram(fill="gray", color="black") +
  facet_grid(restaurant ~ .) +
  ylab("") +
  theme_minimal()

# filter the fastfood df to only contain crispy or grilled items
crisp <- fastfood %>%
  filter((str_detect(item, "Crispy")) == T) %>%
  mutate(cooktype = "Crispy")
grilled <- fastfood %>%
  filter((str_detect(item, "Grilled")) == T) %>%
  mutate(cooktype = "Grilled")

# append the two dataframes together
crisp_and_grilled <- rbind(crisp, grilled)

# create density plot
dens_comparison <- ggplot(crisp_and_grilled, aes(x=calories, color=cooktype)) +
  geom_density()
