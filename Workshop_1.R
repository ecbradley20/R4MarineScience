# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv("data/workshop1/reef_cover_log.csv")

# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/workshop1/acoustic_telemetry_stream.txt"))

# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/workshop1/fish_catch_data.xlsx"), sheet = "Commercial_2026")


# Read in mangrove_data
mangrove_data <- read_csv(file = here::here("data/workshop1/mangrove_survey_raw.csv"))


# Force a modern tibble to degrade into a legacy base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)

# Print the old-style dataframe structure to view
print(benthic_cover_df)
# And compare with tibble alternative
print(benthic_cover)


# Load the package data into active memory
library(palmerpenguins)
data("penguins")

# Examine the structure of the dataset - always do this when loading a new dataset!
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version

# Generate an exploratory summary matrix
summary(penguins)

# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)
# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island)
# Discard logistics tracking attributes while preserving everything else using the minus sign
clean_scientific_fields <- select(penguins, -year)

# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")
# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)
# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")
# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))

# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")
# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)
# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")
# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter

# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, body_mass_g)
# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))
# Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))


# Tidyverse Pipe syntax
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")


# Calculate a new morphological ratio in our environment
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  )
# View your newly engineered variables appended to the far-right columns
glimpse(penguin_ratios)


# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)

# Notice that the table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing the buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)

print(species_mass_summary)


# Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(biological_signal)


mean_bodymass_by_species_and_island <- penguins %>%
  group_by(species, island) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(mean_bodymass_by_species_and_island)


ggplot(data = penguins) +
  geom_boxplot(mapping = aes(x = species,y = body_mass_g,fill = island))
