library(tidyverse)
library(vroom)

# download data file from https://crimeindex.org/current?topic=crime&stat=violent&geo=us&size=all
nyccrimes <- vroom("nyc_crimedata.csv")
glimpse(nyccrimes)

# Location measures
median(nyccrimes$violent_count)
mean(nyccrimes$violent_count)

# Dispersion measures
min(nyccrimes$violent_count)
max(nyccrimes$violent_count)
range(nyccrimes$violent_count)
max(nyccrimes$violent_count) - min(nyccrimes$violent_count)

quantile(nyccrimes$violent_count, 0.25)
quantile(nyccrimes$violent_count, 0.75)
quantile(nyccrimes$violent_count, 0.92)
IQR(nyccrimes$violent_count)

sd(nyccrimes$violent_count)
var(nyccrimes$violent_count)

# Let's look at that distribution
hist(nyccrimes$violent_count)

nyccrimes %>%
  ggplot(aes(violent_count)) +
  geom_histogram()

nyccrimes %>%
  ggplot(aes(violent_count)) +
  geom_histogram(bins = 15)

boxplot(nyccrimes$violent_count)

# get all of these summary stats at once!
summary(nyccrimes)

library(descr)
descr(nyccrimes)

# or you can group by another variable to get summary stats by that variable
by(nyccrimes, nyccrimes$month, summary)

# correlation (two variables)
cor(nyccrimes$violent_count, nyccrimes$property_count) #default is pearson
cor(nyccrimes$violent_count, nyccrimes$property_count,
    method = "spearman")

# let's look at all possible correlations!
nyccrime_quant <- nyccrimes %>%
  select(-contains("_change"), -year)

round(cor(nyccrime_quant),
      digits = 2 # rounded to 2 decimals
      )

library(corrplot)
corrplot(cor(nyccrime_quant),
         method = "number",
         type = "upper" # only show upper side of plot
         )

# contingency table (two qualitative variables)
propertypatterns <- c("property","burglary","theft","motor")
crime_bymonthtype <- nyccrimes %>% 
  select(-contains("_change"), -year) %>% 
  pivot_longer(cols=2:10, names_to = "name", values_to = "count") %>% 
  mutate(type = ifelse(str_detect(name,paste(propertypatterns,collapse = "|")), "property", "violent"),
         month = as.character(month))
table(crime_bymonthtype$month, crime_bymonthtype$type)

# better example of two qualitative variables using diff, broader dataset
allcrimes <- vroom("final_sample.csv")
glimpse(allcrimes)
nationwidecrime <- allcrimes %>%
  filter(State == "Nationwide" & Agency == "Full Sample")
crime_bystate <- allcrimes %>%
  filter(State != "Nationwide" & Agency != "Full Sample")
table(crime_bystate$State, crime_bystate$Region)
table(crime_bystate$Region, crime_bystate$Source.Type)

# visualize distributions for one quant/one qual variable
boxplot(nyccrimes$violent_count ~ nyccrimes$month)

nyccrimes %>%
  ggplot(aes(x = month, y = violent_count, group = year)) +
  geom_boxplot() +
  theme_minimal()

nyccrimes %>%
  mutate(month = fct_inseq(as.character(month))) %>% 
  ggplot(aes(x = month, y = violent_count)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 100) +
  theme_minimal()

# visualize the two-quantitative variables you correlated as a scatterplot
nyccrimes %>%
  ggplot(aes(x=violent_count, y=property_count)) +
  geom_point() +
  theme_minimal()

nyccrimes %>%
  ggplot(aes(x=violent_count, y=property_count)) +
  geom_point(size = 1) +
  facet_wrap(~year) +
  theme_minimal()

nyccrimes %>%
  #mutate(month = fct_inseq(as.character(month))) %>% 
  ggplot(aes(x=violent_count, y=property_count, color=year)) +
  geom_point(size = 1) +
  facet_wrap(~month) +
  theme_minimal()

# check for normality + compare distributions
library(car)
car::qqPlot(nyccrimes$violent_count)
car::qqPlot(nyccrimes$property_count)
qqplot(nyccrimes$violent_count, nyccrimes$property_count)

### HYPOTHESIS TESTING

# test the correlation between two quantitative variables (assuming independent variables, normal distribution)
cor.test(nyccrimes$violent_count, nyccrimes$property_count,
         alternative = "less")

# test if there is a relationship between two qualitative variables
# also known as the Chi-Squared Test of Independence, requires independent variables
chisq.test(table(crime_bymonthtype$type, crime_bymonthtype$month))
chisq.test(table(crime_bystate$Region, crime_bystate$Source.Type))

# proportion testing (for large samples, n > 30; otherwise use binom.test)
# alt-hypothesis: proportion of crime data from Midwest versus South is different
table(crime_bystate$Region)
prop.test(
  x = 14391, # number of "successes" (Midwest)
  n = 49061, # total number of trials (total number)
  p = 0.25 # we test for equal proportion so prob = 0.5 in each group
)

# One mean: monthly murder count in any place in the South
t.test(x = crime_bystate$Murder[crime_bystate$Region=="South"],
       mu = 5)

## Compare two (or more) means

# Are the means of murder counts in the Northeast and South different?
crime_bystate_agg <- crime_bystate %>%
  group_by(Year, State, Region) %>%
  summarise(murder_total = sum(Murder)) %>%
  filter(Region %in% c("Northeast","South"))

crime_bystate_wide <- crime_bystate_agg %>%
  pivot_wider(names_from = Region, values_from = murder_total)

t.test(crime_bystate_wide$Northeast, crime_bystate_wide$South, # variance unknown/equal
       var.equal = TRUE, alternative = "greater"
       )

t.test(murder_total ~ Region, # variance unknown/unequal
       data = crime_bystate_agg,
       var.equal = FALSE,
       alternative = "less"
       )

wilcox.test(murder_total ~ Region,
            data = crime_bystate_agg) # non-parametric/non-normal

crime_bystate_agg2 <- crime_bystate %>%
  group_by(Year, State, Region) %>%
  summarise(murder_total = sum(Murder))

# more than two groups...ANOVA
oneway.test(murder_total ~ Region,
            data = crime_bystate_agg2,
            var.equal = TRUE # assuming equal variances
            )

oneway.test(murder_total ~ Region,
            data = crime_bystate_agg2,
            var.equal = FALSE # assuming unequal variances
            )

library(ggstatsplot)

nationwidecrime_byregion <- allcrimes %>%
  filter(State == "Nationwide" & Region != "Other")

ggbetweenstats(
  data = nationwidecrime_byregion,
  x = Region,
  y = `Violent Crime`,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  var.equal = FALSE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

# Did violent crime numbers in NYC change from 2017 to 2025?
nyccrimes_violent_wide <- nyccrimes %>% 
  select(1:3) %>% 
  pivot_wider(names_from = "year", values_from = "violent_count")
t.test(nyccrimes_violent_wide$`2017`, nyccrimes_violent_wide$`2025`,
       alternative = "less",
       paired = TRUE
       )

### LINEAR REGRESSION

model <- lm(violent_count ~ property_count, data = nyccrimes)
summary(model)

#Call: information about the formula and dataset used to fit the model.
#Residuals: information about the model residuals
#Coefficients: a table showing the fitted model coefficients their standard errors, t-statistics (used for computing p-values), and p-values
#Information about the model fit, including the the Multiple R-squared, or coefficient of determination, which describes how much of the variance of y is described by x

library(ggpubr)

nyccrimes %>%
  ggplot(aes(x = violent_count, y = property_count)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.x = 3700, label.y = 9000) + # for regression equation
  stat_cor(aes(label = after_stat(rr.label)), label.x = 2400, label.y = 9000) + # for R^2
  theme_minimal()

# see how the slope changes when we add a "confounding variable"! is it better or worse?
model2 <- lm(violent_count ~ property_count + theft_count, data = nyccrimes)
summary(model2)

coef(model)
coef(model2)

model3 <- lm(violent_count ~ ., data = nyccrimes)
summary(model3)
coef(model3)
