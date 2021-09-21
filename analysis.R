library(tidyverse)

### problem 1
# Load fastfood dataset from openintro
library(openintro)
data(fastfood)

# Horizontal boxplots categorized by restaurant
boxplot_by_rest <- ggplot(fastfood, aes(x=calories, y=restaurant)) +
  geom_boxplot()

# create hist
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

### problem 4
# import breslow from boot
library(boot)
data(breslow)

# dataframe filtered for non smokers and plotted
non_smokers <- ggplot(breslow %>% filter(smoke == 0), aes(x=age, y=y)) +
  geom_histogram(stat='identity') +
  ylim(0,225) +
  xlab("Age (midpoint of 10 yr bins)") +
  ylab("") +
  ggtitle("Deaths due to coronary artery diease in non-smokers")

# and filtered for smokers
smokers <- ggplot(breslow %>% filter(smoke == 1), aes(x=age, y=y)) +
  geom_col() +
  ylim(0, 225) +
  xlab("Age (midpoint of 10 yr bins") +
  ylab("") +
  ggtitle("Deaths due to coronary artery diease in smokers")

# arrange the histograms onto the same page
arranged_plots <- grid.arrange(non_smokers, smokers, nrow=1)

### problem 5
library(ggridges)
data(loans_full_schema)

quick_histogram <- ggplot(loans_full_schema, aes(x=loan_amount)) +
  geom_histogram(binwidth=2500, fill='gray', color='black') +
  geom_density(aes(y=2500 * ..count..), color='red')

boxplot_data <- ggplot(loans_full_schema, aes(x=loan_amount, y=loan_purpose)) +
  geom_boxplot()

ridge_data <- ggplot(loans_full_schema, aes(x=loan_amount, y=loan_purpose)) +
  geom_density_ridges(bandwidth=2500)



