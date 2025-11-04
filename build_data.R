library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

# Get geographic data from the Census ----
regions_info <- regions() |> 
  sf::st_drop_geometry() |> 
  select(RGN_ID = GEOID, REGION = NAME) |> 
  mutate(RGN_ID = as.numeric(RGN_ID))

divisions_info <- divisions() |> 
  sf::st_drop_geometry() |> 
  select(DIV_ID = GEOID, DIVISION = NAME) |> 
  mutate(DIV_ID = as.numeric(DIV_ID))

# This is a little annoying. Getting states with cb = FALSE returns data with
# region and division IDs (yay) but returns geographic blobs for states like
# Michigan (boo). Getting states with cb = TRUE returns data with correct
# borders for states like Michigan (yay), but no columns for region and division
# IDs (boo)
states_info <- states(cb = FALSE) |> 
  sf::st_drop_geometry() |> 
  select(RGN_ID = REGION, DIV_ID = DIVISION, STATEFP) |> 
  mutate(across(c("RGN_ID", "DIV_ID"), as.numeric))

states <- states(cb = TRUE, resolution = "5m") |>
  filter(STATEFP < 60) |>
  left_join(states_info, by = join_by(STATEFP)) |>
  select(RGN_ID, DIV_ID, STATEFP, STUSPS, STNAME = NAME, geometry) |> 
  mutate(across(c("RGN_ID", "DIV_ID"), as.numeric))

counties <- counties(cb = TRUE, resolution = "5m") |> 
  left_join(states_info, by = join_by(STATEFP)) |> 
  left_join(regions_info, by = join_by(RGN_ID)) |> 
  left_join(divisions_info, by = join_by(DIV_ID)) |> 
  filter(STATEFP < 60) |> 
  select(
    GEOID, NAME, NAMELSAD, REGION, DIVISION, 
    STUSPS, STNAME = STATE_NAME, geometry
  )

states_with_regions <- states |> 
  left_join(regions_info, by = join_by(RGN_ID)) |> 
  left_join(divisions_info, by = join_by(DIV_ID))


# Get data from the ACS ----
vars <- c(
  "total_pop" = "B01003_001",
  "total_households" = "B22003_001", 
  "snap_households" = "B22003_002"
)

snap_2023 <- get_acs(
  geography = "county",
  variable = vars,
  year = 2023,
  geometry = FALSE,
  survey = "acs5"
) |>
  mutate(STATEFP = str_sub(GEOID, 1, 2)) |>
  select(-moe) |>  # Get rid of margin of error for pivoting
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |>
  left_join(st_drop_geometry(states_with_regions), by = join_by(STATEFP)) |>
  # Bring in state, region, and division details
  select(
    GEOID, NAME_FULL = NAME, STATEFP, STUSPS, STNAME, REGION, DIVISION,
    total_pop, total_households, snap_households
  )


# Save all the data ----
if (!dir.exists("data/states")) {
  dir.create("data/states", recursive = TRUE)
}
write_sf(states, "data/states/states.shp")

if (!dir.exists("data/counties")) {
  dir.create("data/counties", recursive = TRUE)
}
write_sf(counties, "data/counties/counties.shp")

write_csv(regions_info, "data/regions.csv")
write_csv(divisions_info, "data/divisions.csv")
write_csv(snap_2023, "data/snap_2023.csv")


# Build answer key so that the plots to recreate exist in images/ ----
quarto::quarto_render(
  "answers.qmd",
  output_format = c("html", "typst")
)
