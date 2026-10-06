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
