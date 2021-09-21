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

# this funtion will filter the strings entered in the item column
crispy_or_grilled <- function(string) {
  string <- tolower(string)
  if (grepl("crispy", string) == T) {
    return("Crispy")
  }
  else if (grepl("grilled", string) == T) {
    return("Grilled")
  }
  else
    return(NaN)
}

# create a new column with the function applied to each row
fastfood$cooktype <- sapply(fastfood$item, crispy_or_grilled)

# remove all rows whose items do not contain crispy or grilled
grilled_df <- subset(fastfood, cooktype=="Grilled")
crispy_df <- subset(fastfood, cooktype=="Crispy")

grilled_dens <- ggplot(grilled_df, aes(x=calories)) +
  geom_density()

crispy_dens <- ggplot(crispy_df, aes(x=calories)) +
  geom_density()
