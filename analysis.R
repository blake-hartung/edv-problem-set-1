library(tidyverse)

### problem 1
# Load fastfood dataset from openintro
library(openintro)
data(fastfood)

# Horizontal boxplots categorized by restaurant
boxplot_by_rest <- ggplot(fastfood, aes(x=calories,
                                        y=reorder(restaurant, calories, median))) +
  geom_boxplot() +
  ylabb("restaurant")

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
dens_comparison <- ggplot(crisp_and_grilled, aes(x=calories,
                                                 color=cooktype,
                                                 fill=cooktype)) +
  geom_density(alpha=0.2)

### problem 2
data(mtl)

gg_hist <- ggplot(mtl) +
  geom_histogram(aes(x=asubic),
                 fill="gray",
                 color="black")

default_hist <- hist(mtl$asubic)

gg_left <- ggplot(mtl) +
  geom_histogram(aes(x = age),
                 fill = "gray",
                 color = "black",
                 binwidth = 5,
                 boundary = 40,
                 closed = "left") +
  scale_x_continuous(breaks = seq(40, 80, by = 5))

gg_right <- ggplot(mtl) +
  geom_histogram(aes(x = age),
                 fill = "gray",
                 color = "black",
                 binwidth = 5,
                 boundary = 40,
                 closed = "right") +
  scale_x_continuous(breaks = seq(40, 80, by = 5))

# change boundary to just above the discrete value so now boundary value is included
gg_left_changed <- ggplot(mtl) +
  geom_histogram(aes(x = age),
                 fill = "gray",
                 color = "black",
                 binwidth = 5,
                 boundary = 40.01,
                 closed = "left") +
  scale_x_continuous(breaks = seq(40, 80, by = 5))


### problem 4
# import breslow from boot
library(boot)
data(breslow)

# Mutate dataset for labelling purposes
breslow_mutate <- breslow %>%
  mutate(labeled = ifelse(smoke == 1, "Smoker", "Non-Smoker"))

# plot the comparison
death_comparison <- ggplot(breslow_mutate, aes(x=age, y=y)) +
  geom_col() +
  ylim(0, 215) +
  xlab("Age (midpoint of 10 yr bins)") +
  ylab("Deaths due to Coronary Artery Disease") +
  facet_wrap(labeled ~ .)

### problem 5
library(ggridges)
data(loans_full_schema)

quick_histogram <- ggplot(loans_full_schema, aes(x=loan_amount)) +
  geom_histogram(binwidth=2500, fill='gray', color='black') +
  geom_density(aes(y=2500 * ..count..), color='red')

boxplot_data <- ggplot(loans_full_schema,
                       aes(x=loan_amount,
                           y=reorder(loan_purpose, loan_amount, median))) +
  geom_boxplot() +
  ylab("Loan Purpose") +
  xlab("Loan Amount")

ridge_data <- ggplot(loans_full_schema,
                     aes(x=loan_amount,
                         y=reorder(loan_purpose, loan_amount, median))) +
  geom_density_ridges(bandwidth=2500) +
  ylab("Loan Purpose") +
  xlab("Loan Amount")


