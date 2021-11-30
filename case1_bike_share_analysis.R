# ANALYSIS ---------------------------------------------------------------------

## Environment ----
# Import packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('skimr')
install.packages('ggplot2')

# Load packages
library(tidyverse)
library(lubridate)
library(skimr)
library(ggplot2)

# Initialize functions and options
'%!in%' <- Negate('%in%')
options(scipen = 999)

# Import data and wrange for visualizations ----
df <- read_csv('Divvy_Trips_2019Q2_to_2020Q1.csv')

# Add columns for the year, month, day, day of the week, and hour of each ride
df <- df %>% mutate(
  duration_asdate = difftime(end_time, start_time, units = 'secs') %>% 
    as.numeric() %>% as_datetime(), #For graphics
  start_hour = df$start_time %>% format('%H:%M:%S'),
  start_year = df$start_time %>% year() %>% as_factor(),
  start_month = df$start_time %>% month(label = TRUE),
  start_day = df$start_time %>% day(),
  start_wday = df$start_time %>% wday(label = TRUE),
  end_hour = df$end_time %>% format('%H:%M:%S'),
  end_year = df$end_time %>% year() %>% as_factor(),
  end_month = df$end_time %>% month(label = TRUE),
  end_day = df$end_time %>% day(),
  end_wday = df$end_time %>% wday(label = TRUE)
)

# Add columns with fraction of day for start and return
df <- df %>% mutate(
  start_pday = case_when(
    start_hour >= '05:00:00' & start_hour < '12:00:00' ~ 'Morning',
    start_hour >= '12:00:00' & start_hour < '18:00:00' ~ 'Afternoon',
    start_hour >= '18:00:00' & start_hour < '22:00:00' ~ 'Evening',
    start_hour >= '22:00:00' | start_hour < '05:00:00' ~ 'Night') %>% 
    factor(levels = c('Morning', 'Afternoon', 'Evening', 'Night')),
  end_pday = case_when(
    end_hour >= '05:00:00' & end_hour < '12:00:00' ~ 'Morning',
    end_hour >= '12:00:00' & end_hour < '18:00:00' ~ 'Afternoon',
    end_hour >= '18:00:00' & end_hour < '22:00:00' ~ 'Evening',
    end_hour >= '22:00:00' | end_hour < '05:00:00' ~ 'Night') %>% 
    factor(levels = c('Morning', 'Afternoon', 'Evening', 'Night'))
)

# ANALYSIS ----
# Statistical discovery
summary(df)
skim(df)

## Number of rides----
table(df$user_type)

# Per month
df %>% group_by(
  start_month, user_type) %>% count() %>% 
  pivot_wider(names_from = user_type, values_from = n) %>% 
  mutate(ratio = Subscriber / Customer)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_month, fill = user_type),
    position = 'dodge') +
  ggtitle('Rides per month') +
  labs(x = 'Months', y = 'Number of rides') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'))
  
# Per day of the week
df %>% group_by(start_wday, user_type = user_type) %>% count() %>% 
  pivot_wider(names_from = user_type, values_from = n) %>%
  mutate(ratio = Subscriber / Customer)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_wday, fill = user_type),
    position = 'dodge') +
  ggtitle('Rides per day of the week') +
  labs(x = 'Days', y = 'Number of rides') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold')) 

# Per day of the week per month
df %>% group_by(start_month, start_wday, user_type) %>% count() %>% 
  pivot_wider(names_from = c(start_wday, user_type), values_from = n)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_wday, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ start_month) +
  ggtitle('Rides per month per day') +
  labs(y = 'Number of rides') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = c(0.77,1.107))

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_month, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ start_wday) +
  ggtitle('Rides per day per month') +
  labs(y = 'Number of rides') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = c(0.9,0.07))

## Duration----
df %>% group_by(user_type) %>% summarise(
  mean = duration_asdate %>% mean() %>% format('%m-%d %H:%M:%S'),
  median = duration_asdate %>% median() %>% format('%m-%d %H:%M:%S'),
  min = duration_asdate %>% min() %>% format('%m-%d %H:%M:%S'),
  max = duration_asdate %>% max() %>% format('%m-%d %H:%M:%S'),
  sd = duration_asdate %>% sd()
)

# Per month
aggregate(
  df$duration_asdate ~ df$user_type + df$start_month, 
  FUN = mean) %>% 
  setNames(c('user_type', 'month', 'duration_asdate')) %>% 
  mutate(duration_asdate = duration_asdate %>% format('%m-%d %H:%M:%S')) %>% 
  pivot_wider(names_from = user_type, values_from = duration_asdate)

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = start_month,
                             y = duration_asdate,
                             fill = user_type),
               outlier.shape = NA) +
  ggtitle('Ride duration per month') +
  labs(x = 'Month', y = 'duration in hours') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'))
  
# Per day
aggregate(
  df$duration_asdate ~ df$user_type + df$start_wday, 
  FUN = mean) %>% 
  setNames(c('user_type', 'day', 'duration_asdate')) %>% 
  mutate(duration_asdate = duration_asdate %>% format('%m-%d %H:%M:%S')) %>% 
  pivot_wider(names_from = user_type, values_from = duration_asdate)

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = start_wday,
                             y = duration_asdate,
                             fill = user_type),
               outlier.shape = NA) +
  ggtitle('Ride duration per day') +
  labs(x = 'Month', y = 'Duration in hours') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'))

# Per day per month
aggregate(
  df$duration_asdate ~ df$user_type + 
                     df$start_month +
                     df$start_wday,
  FUN = mean) %>% 
  setNames(c('user_type', 'month', 'day', 'duration_asdate')) %>% 
  mutate(duration_asdate = duration_asdate %>% format('%m-%d %H:%M:%S')) %>%
  pivot_wider(names_from = c(day, user_type), values_from = duration_asdate)

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = start_wday,
                             y = duration_asdate,
                             fill = user_type),
               outlier.shape = NA) +
  facet_wrap(~ start_month) +
  ggtitle('Ride duration per month per day') +
  labs(y = 'Duration in hours') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = c(0.8,1.107))

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = start_month,
                             y = duration_asdate,
                             fill = user_type),
               outlier.shape = NA) +
  facet_wrap(~ start_wday) +
  ggtitle('Ride duration per day per month') +
  labs(y = 'Duration in hours') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = c(0.9,0.07))

#Looking into an anomaly
ggplot(data = df %>% filter(start_wday == 'Wed' & start_month == 'Mar')) +
  geom_boxplot(mapping = aes(x = start_day %>% as.factor(),
                             y = duration_asdate,
                             fill = user_type),
               outlier.shape = NA) +
  ggtitle('Ride duration per day, in Wednesday on March') +
  labs(x = 'date in September', y = 'Duration in hours') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'))
## Pick up and drop off times----

# Starts
# Per day of the week:
df %>% group_by(start_pday, start_wday, user_type) %>% count() %>% 
  pivot_wider(names_from = c(start_pday, user_type), values_from = n)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_pday, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ start_wday) +
  ggtitle('Pick up times per day') +
  labs(y = 'Count') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank()) 

# Per month:
df %>% group_by(start_month, start_pday, user_type) %>% count() %>% 
  pivot_wider(names_from = c(start_month, user_type), values_from = n)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = start_pday, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ start_month) +
  ggtitle('Pick up times per month') +
  labs(y = 'Count') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank()) 

# Ends
# Per day of the week:
df %>% group_by(end_pday, end_wday, user_type) %>% count() %>% 
  pivot_wider(names_from = c(end_pday, user_type), values_from = n)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = end_pday, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ end_wday) +
  ggtitle('Drop off times per day') +
  labs(y = 'Count') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank()) 

# Per month:
df %>% group_by(end_month, end_pday, user_type) %>% count() %>% 
  pivot_wider(names_from = c(end_month, user_type), values_from = n)

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = end_pday, fill = user_type),
    position = 'dodge') +
  facet_wrap(~ end_month) +
  ggtitle('Drop off times per month') +
  labs(y = 'Count') +
  scale_fill_discrete(name = 'User type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'bold'),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank())
