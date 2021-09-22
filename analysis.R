library(tidyverse)

### problem 1
# Load fastfood dataset from openintro
library(openintro)
data(fastfood)

# Horizontal boxplots categorized by restaurant
boxplot_by_rest <- ggplot(fastfood, aes(x=calories,
                                        y=reorder(restaurant, calories, median))) +
  geom_boxplot(fill="gray") +
  ylab("restaurant") +
  theme_minimal()

most_caloric <-  fastfood %>%
  filter(calories == max(calories)) %>%
  select(item)

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


### Problem 3
library(agridat)
data(australia.soybean)

# filter data by location
lawes_data <- australia.soybean %>%
  filter(loc == "Lawes")
brookstead_data <- australia.soybean %>%
  filter(loc == "Brookstead")
nambour_data <- australia.soybean %>%
  filter(loc == "Nambour")
redlandbay_data <- australia.soybean %>%
  filter(loc == "RedlandBay")

# create qq plots using normal data on the x-axis
lawes_qq <- qqplot(rnorm(length(lawes_data$yield)),
                   lawes_data$yield,
                   xlab = "Normal Distribution",
                   ylab = "Lawes Yield")
brookstead_qq <- qqplot(rnorm(length(brookstead_data$yield)),
                        brookstead_data$yield,
                        xlab = "Normal Distribution",
                        ylab = "Brookstead Yield")
nambour_qq <- qqplot(rnorm(length(nambour_data$yield)),
                     nambour_data$yield,
                     xlab = "Normal Distribution",
                     ylab = "Nambour Yield")
redlandbay_qq <- qqplot(rnorm(length(redlandbay_data$yield)),
                        redlandbay_data$yield,
                        xlab = "Normal Distribution",
                        ylab = "Redland Bay Yield")

# create the histograms with density plots

yield_histograms <- ggplot(australia.soybean, aes(x = yield)) +
  geom_histogram(binwidth = 0.5, fill='gray', color='black') +
  geom_density(aes(y=0.5 * ..count..), color='red') +
  facet_wrap(loc ~ .)

# shapiro-wilk tests (This function produces a test statistic W along with
# a corresponding p-value. If the p-value is less than a =.05, there is
# sufficient evidence to say that the sample does not come from a population
# that is normally distributed.)

lawes_shap <- round(shapiro.test(lawes_data$yield)$p.value, 4)
brookstead_shap <- round(shapiro.test(brookstead_data$yield)$p.value, 4)
nambour_shap <- round(shapiro.test(nambour_data$yield)$p.value, 4)
redlandbay_shap <- round(shapiro.test(redlandbay_data$yield)$p.value, 4)

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

# create a histogram with a density plot in order to describe the distribution
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


