#2.6
library(tidyverse)

library(palmerpenguins)

# Create a long version of the penguins dataset

penguins_long <- penguins |>
  pivot_longer(
    cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
    names_to = "measurement_type",
    values_to = "value"
  )
# View the result
head(penguins_long)

penguins_long |>
  drop_na(value) |>
  ggplot(aes(x = value, fill = species)) +
  geom_histogram(bins = 30, alpha = 0.7, colour = "black") +
  facet_wrap(~ measurement_type, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Morphometric distributions across penguin species",
    x = "Measurement value",
    y = "Frequency"
  )


mass_summary <- penguins |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(mean_mass = mean(body_mass_g))

head(mass_summary)

mass_matrix <- mass_summary |>
  pivot_wider(
    names_from = island,
    values_from = mean_mass
  )

head(mass_matrix)


#2.8
library(tidyverse)

# A remarkably messy data frame of field sites
messy_sites <- tibble(
  site_id = c("Nelly Bay", "nelly_bay", "NELLY BAY", " Geoffrey_Bay ", "geoffrey bay")
)

# Using stringr within mutate to standardize the text
clean_sites <- messy_sites |>
  mutate(
    # 1. Convert everything to lowercase
    site_clean = str_to_lower(site_id),
    # 2. Replace any spaces with underscores
    site_clean = str_replace_all(site_clean, pattern = " ", replacement = "_"),
    # 3. Trim any leading or trailing whitespace (invisible spaces at the ends)
    site_clean = str_trim(site_clean)
  )

print(clean_sites)


library(lubridate)

# Parsing different date formats
date_1 <- dmy("25/12/2026")
date_2 <- ymd("2026-12-25")

# R now recognizes these as identical Date objects
date_1 == date_2

# A tibble of raw sensor data with a messy character timestamp
sensor_data <- tibble(
  raw_time = c("14-05-2026 08:30:00", "14-05-2026 08:45:00", "14-05-2026 09:00:00"),
  temperature = c(24.5, 24.6, 24.4)
)

# Converting character strings to true POSIXct datetime objects
sensor_clean <- sensor_data |>
  mutate(
    true_time = dmy_hms(raw_time)
  )

print(sensor_clean)


#2.9
# Table 1: Biological observation data
observations <- tibble(
  site_code = c("NB", "GB", "MI", "NB", "HB"),
  species = c("Trout", "Snapper", "Trout", "Cod", "Trout"),
  count = c(5, 2, 1, 3, 8)
)

# Table 2: Spatial metadata
site_metadata <- tibble(
  site_code = c("NB", "GB", "MI", "RP", "WP"),
  zone = c("Marine National Park", "Conservation Park", "Habitat Protection", "General Use", "Other Use"),
  lat = c(-19.16, -19.15, -19.14, -19.12, -19.11)
)

# Joining metadata to our observations
joined_data <- observations |>
  left_join(site_metadata, by = join_by(site_code))

print(joined_data)

matched_data <- observations |>
  inner_join(site_metadata, by = join_by(site_code))
glimpse(matched_data)

# Which observations are missing from our metadata dictionary?
missing_context <- observations |>
  anti_join(site_metadata, by = join_by(site_code))
glimpse(missing_context)


#2.10
# Raw data from an old temperature logger
logger_data <- tibble(
  depth_m = c(10, 20, 30, 40),
  temp_c = c(24.5, 24.1, -999, 23.5)) # -999 is a known sensor error code
print(logger_data)

# Convert the -999 error codes to true NA values
fixed_logger <- logger_data |>
  mutate(temp_c = na_if(temp_c, -999))

print(fixed_logger)

# Simulate some count data
shark_counts <- tibble(
  site = c("Reef_A", "Reef_B", "Reef_C"),
  shark_count = c(3, NA, 5)) # The NA here actually means 0 sharks were seen

# Replace NA with 0 
shark_fixed <- shark_counts |>
  mutate(shark_count = coalesce(shark_count, 0))

print(shark_fixed)

cpue_data <- tibble(
  site = c("Bay_1", "Bay_2"),
  catch = c(10, 0),
  effort_hours = c(2, 0))

# Calculating CPUE (catch / effort)
cpue_calc <- cpue_data |>
  mutate(
    cpue = catch / effort_hours)

print(cpue_calc)


raw_catch <- tibble(
  site = c("Reef_1", "Reef_1", "Reef_2"),
  species = c("Pmaculatus", "Pleopardus", "Pmaculatus"),
  count = c(5, 2, 8))

full_catch_matrix <- raw_catch |>
  complete(site, species, fill = list(count = 0))

print(full_catch_matrix)

sensor_log <- tibble(
  day = 1:4,
  salinity = c(35.2, 35.1, NA, 35.3)
)

# Remove any row where salinity is missing
clean_log <- sensor_log |>
  drop_na(salinity)


#2.11
#part 1
# Make sure packages are loaded
library(tidyverse)
library(palmerpenguins)

# The messy metadata provided by your colleague
island_metadata <- tibble(
  island_name = c(" biscoe", "Dream ", "Torgersen"),
  station_install = c("15/01/2003", "22-03-2004", "05/11/2001"),
  latitude = c(-64.81, -64.73, -64.76)
)

print(island_metadata)

clean_metadata <- island_metadata |> 
  mutate(
    
    island_name = str_trim(island_name),
    island_name = str_to_title(island_name),
    
    station_install = standardized <- parse_date_time(station_install, orders = c("dmy", "dmy", "dmy"))
  )

print(clean_metadata)

#part 2
penguins_spatial <- penguins |>
  left_join(clean_metadata, by = join_by(island==island_name))

print(penguins_spatial)
# Scroll to the right to see the new columns
head(penguins_spatial)



#2.12
#Phase 1
#import data sets via right clicking them in the files window
