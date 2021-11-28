# DATA CLEANING ----------------------------------------------------------------

# Environment ----
# Import packages
install.packages('tidyverse')
install.packages('lubridate')

# Load packages
library(tidyverse)
library(lubridate)

# Initialize functions and options
'%!in%' <- Negate('%in%')

## Data ----
# Import data
files <- list.files(pattern = 'Divvy_Trips_20.._Q[1-4]')
alldata <- lapply(files, read_csv)

# Wrangle to merge
# Column names
lapply(alldata, colnames)

alldata[[1]] <- alldata[[1]] %>% rename(c(
  ride_id = "01 - Rental Details Rental ID",
  start_time = "01 - Rental Details Local Start Time",
  end_time = "01 - Rental Details Local End Time",
  start_station_name = "03 - Rental Start Station Name",
  start_station_id = "03 - Rental Start Station ID",
  end_station_name = "02 - Rental End Station Name",
  end_station_id = "02 - Rental End Station ID",
  user_type="User Type"))

alldata[[2]] <- alldata[[2]] %>% rename(c(
  ride_id = trip_id,
  start_time = start_time,
  end_time = end_time,
  start_station_name = from_station_name,
  start_station_id = from_station_id,
  end_station_name = to_station_name,
  end_station_id = to_station_id,
  user_type= usertype))

alldata[[3]] <- alldata[[3]] %>% rename(c(
  ride_id = trip_id,
  start_time = start_time,
  end_time = end_time,
  start_station_name = from_station_name,
  start_station_id = from_station_id,
  end_station_name = to_station_name,
  end_station_id = to_station_id,
  user_type= usertype))

alldata[[4]] <- alldata[[4]] %>% rename(c(
  ride_id = ride_id,
  start_time = started_at,
  end_time = ended_at,
  start_station_name = start_station_name,
  start_station_id = start_station_id,
  end_station_name = end_station_name,
  end_station_id = end_station_id,
  user_type= member_casual))

# Column types
alldata %>% lapply(str)
for(i in 1:length(alldata)){
  alldata[[i]] <- alldata[[i]] %>% mutate(
    ride_id = as.character(format(ride_id, scientific = FALSE)),
    start_station_id = as.character(start_station_id),
    end_station_id = as.character(end_station_id)
  )
}

# Select only columns to merge
for(i in 1:length(alldata)){
  alldata[[i]] <- alldata[[i]] %>% select(
    ride_id,
    start_time,
    end_time,
    start_station_name,
    start_station_id,
    end_station_name,
    end_station_id,
    user_type
  )
}

# Merge all data
df <- bind_rows(alldata)
rm(alldata, files, i)


# CLEAN ----
df0 <- df

## Ride id----
# If TRUE, all ids are unique:
df %>% nrow() == df$ride_id %>% unique() %>% length()

# If TRUE, all ids have only alpha numeric characters only
df$ride_id[df$ride_id %>% str_detect('[^[:alnum:]]')] %>% length() == 0

## Dates----

# Earliest trip: If TRUE, earliest ride is 2019 / Q2:
df$start_time %>% min() >= ymd_hms('2019-04-01 00:00:00')

# Latest trip, if TRUE, latest ride is 2020 / Q1
df$start_time %>% max() <= ymd_hms('2020-03-31 23:59:59')

# make sure all end_times are greater and than start_times
df <- df %>% filter(start_time < end_time)

# Add a column for length of ride (in hours)
df <- df %>% mutate(
  duration = difftime(end_time, start_time, units = 'secs') %>% 
    as.numeric() %>% '/' (3600),
  .after = end_time)

# Add columns for start hour
df <- df %>% mutate(start_hour = df$start_time %>% format('%H:%M:%S'))

## Stations----
# If True, all names reference only one station id:
df %>% group_by(start_station_name, start_station_id) %>% count() %>% 
  filter(start_station_name %>% duplicated()) %>% nrow() == 0
df %>% group_by(end_station_name, end_station_id) %>% count() %>% 
  filter(end_station_name %>% duplicated()) %>% nrow() == 0

# If True, all ids reference only one station name:
df %>% group_by(start_station_name, start_station_id) %>% count() %>% 
  filter(start_station_id %>% duplicated()) %>% nrow() == 0
df %>% group_by(end_station_name, end_station_id) %>% count() %>% 
  filter(end_station_id %>% duplicated()) %>% nrow() == 0

# Delete data point with missing station name and id
df <- df %>% filter(
  !(start_station_name %>% is.na() & start_station_id %>% is.na()) &
    !(end_station_name %>% is.na() & end_station_id %>% is.na())
)
# Delete start and end stations test (HQ related)
df <- df %>% filter(!(start_station_name %>% str_detect('HQ') | 
                        end_station_name %>% str_detect('HQ')))

## User type----
diffs <- 
  df$user_type[df$user_type %!in% c('Subscriber', 'Customer')] %>% unique()
df$user_type <- df$user_type %>% replace(df$user_type == diffs[1], 'Subscriber')
df$user_type <- df$user_type %>% replace(df$user_type == diffs[2], 'Customer')
rm(diffs)
df <- df %>% mutate(user_type = as_factor(user_type))

# EXPORT

# Export .csv file for use in Tableau
write_csv(df[sample(nrow(df), nrow(df)/20), ] %>% select(
  start_time, end_time, start_hour, duration, start_station_name, 
  end_station_name, user_type),
  file = 'Divvy_Trips_2019Q2_to_2020Q1_vizplan.csv')

write_csv(df %>% select(start_time, end_time, start_hour,
                        duration, start_station_name, end_station_name, 
                        user_type),
          file = 'Divvy_Trips_2019Q2_to_2020Q1.csv')