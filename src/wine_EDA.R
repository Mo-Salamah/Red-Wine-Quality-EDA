library(tidyverse)
library(tidyr)
library(readr)
library(skimr)
library(corrplot)
library(janitor)
library(reactable)




wine <- read_csv("./data/winequality-red-clean.csv")
head(wine)

# Categorical Variables: 
# - quality (has range 3 - 8)
# fixed_acidity
# volatile_acidity
# citric_acid
# residual_sugar
# chlorides
# free_sulfur_dioxide
# total_sulfur_dioxide
# density
# pH
# sulphates
# alcohol
# quality

# it seems like low avg_volatile_acidity is correlated with higher quality
# ... high avg_citric_acid is ... higher quality
# avg_fixed_acidity: undetermined 

# Creating dataframses -----------

wine %>% 
  group_by(quality) %>% 
  summarise(avg_fixed_acidity = mean(fixed_acidity, na.rm = TRUE),
            avg_volatile_acidity = mean(volatile_acidity, na.rm = TRUE),
            avg_citric_acid = mean(citric_acid, na.rm = TRUE)) %>% 
  arrange(quality) %>%
  print()
            
wine %>% 
  group_by(quality) %>% 
  summarise(avg_residual_sugar = mean(residual_sugar, na.rm = TRUE),
            avg_chlorides = mean(chlorides, na.rm = TRUE),
            avg_free_sulfur_dioxide = mean(free_sulfur_dioxide, na.rm = TRUE)) %>% 
  arrange(quality) %>%
  print()
            
wine %>% 
  group_by(quality) %>% 
  summarise(avg_total_sulfur_dioxide = mean(total_sulfur_dioxide, na.rm = TRUE),
            avg_density = mean(density, na.rm = TRUE),
            avg_pH = mean(pH, na.rm = TRUE),
            avg_sulphates = mean(sulphates, na.rm = TRUE),
            avg_quality = mean(quality, na.rm = TRUE)) %>% 
  arrange(quality) %>%
  print()
           
# Simple individual plots -----------------

# 
# wine %>% 
#   select(fixed_acidity, quality) %>% 
#   group_by(quality) %>% 
#   summarise(avg = mean(fixed_acidity)) -> wine_f_acidity
# 
# wine %>% 
#   select(volatile_acidity, quality) %>% 
#   group_by(quality) %>% 
#   summarise(avg = mean(volatile_acidity)) -> wine_v_acidity
# 
# wine %>% 
#   select(citric_acid, quality) %>% 
#   group_by(quality) %>% 
#   summarise(avg = mean(citric_acid)) -> wine_c_acid


# Plot idea: combine these three plots into one 
# x axis: quality
# y axis: avg value for each group 
#  - Need to scale the avg values appropriately
# color: group
#  - group is an additional column that denotes to which of (fixed_acidity, 
#    volatile_acidity, citric_acid) the avg value belongs

p_f_acidity <- ggplot(wine, aes(y = fixed_acidity, x = quality, group = quality)) 
p_v_acidity <- ggplot(wine, aes(y = volatile_acidity, x = quality, group = quality)) 
p_citric_acid <- ggplot(wine, aes(y = citric_acid, x = quality, group = quality)) 
p_alcohol <- ggplot(wine, aes(y = alcohol, x = quality, group = quality)) 

# What we learn from the below graph is that fixed_acidity is 
# Not informative
p_f_acidity +     
  geom_boxplot()

# What we learn from the below plot is that volatile_acidity
# is negatively correlated with quality and that the relationship
# is well described by a linear function 
p_v_acidity +
  geom_boxplot()

p_v_acidity +
  geom_point(alpha = 0.1)

p_citric_acid +
  geom_boxplot()

p_alcohol +
  geom_boxplot()


skim(wine)

# Finding strongest correlations amongst attributes ----------
wine %>% 
  cor() %>% 
   corrplot()
  # sort() -> corr_array
  

wine %>% 
  cor() -> corr_matrix
  

# find the strongest positive correlations between attributes
corr_positive <- corr_array[corr_array > 0.5 & corr_array < 1]

# Find the strongest negative correlations between attributes
corr_negative <- corr_array[corr_array < -0.5]



as.data.frame(scale(wine))

wine %>% 
  select(residual_sugar, free_sulfur_dioxide, pH) %>% 
  scale() %>% 
  as.data.frame() %>% 
  print()


# Identifying the strongest positive correlations :
# - fixed_acidity x citric_acid
# - fixed_acidity x density
# - free_sulfur_dioxide x total_sulfur_dioxide

# Curious observation
# - fixed_acidity is strongly correlated with both 
# citric_acid and density. However, they aren't strongly
# correlated with each other


# Identifying the strongest negative correlations :
# - fixed_acidity x pH
# - citric_acid x volatile_acidity
# - citric_acid x pH

# Identifying strongest correlations with quality (neg or pos):
# - alcohol
# - volatile_acidity
# - sulphates


corr_matrix[12, 1:ncol(corr_matrix)] %>% 
  abs() %>% 
  sort()


# Predictions:
# - fixed_acidity isn't informative in predicting quality because the information
# it encodes is contained within citric_acid and density
# - pH 


# Visualizing correlations


p_fixed_acidity_citric_acid <- ggplot(wine, aes(fixed_acidity, citric_acid, color = quality))

p_fixed_acidity_citric_acid +
  geom_point()

# acidity and ph
p_volatile_acidity_pH <- ggplot(wine, aes(volatile_acidity, pH, color = quality))

p_volatile_acidity_pH +
  geom_density_2d_filled()

p_fixed_acidity_pH <- ggplot(wine, aes(fixed_acidity, pH, color = quality))

p_fixed_acidity_pH +
  geom_density_2d_filled()



p_fixed_acidity_density <- ggplot(wine, aes(fixed_acidity, density, color = quality))

p_fixed_acidity_density +
  geom_point()

p_free_sulphur_total_sulphur <- ggplot(wine, aes(free_sulfur_dioxide, total_sulfur_dioxide, color = quality))

p_free_sulphur_total_sulphur +
  geom_point()

# negative


# Identifying the strongest negative correlations :
# - fixed_acidity x pH
# - citric_acid x volatile_acidity
# - citric_acid x pH

p_fixed_acidity_pH <- ggplot(wine, aes(fixed_acidity, pH, color = quality))

p_fixed_acidity_pH +
  geom_point()

p_citric_acid_volatile_acidity <- ggplot(wine, aes(citric_acid, volatile_acidity, color = quality))

p_citric_acid_volatile_acidity +
  geom_point()

p_citric_acid_pH <- ggplot(wine, aes(citric_acid, pH, color = quality))

p_citric_acid_pH +
  geom_point()

p_citric_acid_pH +
  geom_smooth()

p_citric_acid_pH +
  geom_quantile()

p_citric_acid_pH +
  geom_density_2d()

p_citric_acid_pH +
  geom_hex()


wine %>% 
  filter(citric_acid == 0) %>%
  summarise(avg_quality = mean(quality))


# The above analysis should allow us to make predictions on the most important 
# Features, and make guesses on the probable existance of confounding variables

# The below analysis should allow us to asses how much predictive information 
# We lose when we eliminate features with confounding variables

# Linear Regression Coeefecient Feature Importance Assessment
























