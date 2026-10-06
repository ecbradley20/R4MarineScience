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


