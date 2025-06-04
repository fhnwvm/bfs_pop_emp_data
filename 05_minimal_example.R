library(data.table)
library(dplyr)
rm(list = ls())


# Data Harmonization ------------------------------------------------------


# Load and convert to data.table
statpop <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023.csv", sep = ";")
statpop_noloc <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023_NOLOC.csv", sep = ";")

#Merge the 2 tables by "RELI" make sure the NOLOC tables gets an _n suffix added to all it's variables
corrected_statpop <- merge(statpop, statpop_noloc, by = "RELI", all.x = TRUE, suffixes = c("", "_n"))

#Define which columns should be corrected
cols_to_correct <- c("BBTOT")

#Execute subtraction
for (col in cols_to_correct) {
  n_col <- paste0(col, "_n")
  corrected_statpop[, (col) := get(col) - fifelse(is.na(get(n_col)), 0, get(n_col))]
}

#Define which columns should be kept. This must inclue "E_KOORD" and "N_KOORD"
cols_to_keep <- c("E_KOORD", "N_KOORD", "BBTOT")

#Restrict dataset to desired data
corrected_statpop <- corrected_statpop %>%
  select(all_of(cols_to_keep)) %>%
  rename(TOT = BBTOT) 

#Save corrected Dataset in _data/input
fwrite(corrected_statpop, "_data/input/totpop2023.csv")

#For a more thorough correction and harmonazation of a dataset please check "01_harmonization_correction.R"

# Aggregation -------------------------------------------------------------

## Initialization --------------------------------------------------

rm(list = ls())
source("00_functions.R")
library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)

## Create reference grid --------------------------------------------------

# Load data grid
coordinates <- fread("_data/input/totpop2023.csv", sep = ",") %>%
  select(E_KOORD, N_KOORD)

#Define radius
R <- 400

#Create empty cells 
coordinates <- create_empties(coordinates, R)

#Create base grid with all cells now present in "coordinates"
output <- create_gpkg(coordinates)


## Create neighborhood reference table -------------------------------------

#Given Radius and coordinates defined above calculate the reference table
neighborhood <- find_neighbors(coordinates, R)


## Density Calculation -------------------------------------------------------------

#Load the actual data we want to aggregate this may only contain a subset of the grid defined above
input <- fread("_data/input/totpop2023.csv", sep = ",")

#Make sure we are dealing with data.tables for efficiency
setDT(input)
setDT(neighborhood)

#Set keys for efficiency
setkey(input, E_KOORD, N_KOORD)
setkey(neighborhood, E_neighbor, N_neighbor)

#Aggregate the population into average densites per cell
aggregated <- aggregate_population(input, c("TOT"), "Diamond", 1, neighborhood)

#Join the densities to the outputfile
output <- output %>%
  left_join(aggregated, by = c("E_center", "N_center")) %>%
  rename_with(~ sub("_sum_norm$", "", .x), ends_with("_sum_norm"))

#save the outputfile
st_write(output, "_output/totpop_density_400m_manhatten_2023.gpkg", layer = "squares", delete_layer = TRUE)

#For quicker display in ggplot restrict to a predefined bounding-box
bounding_box <- c(xmin = 2608500, xmax = 2617800, 
                  ymin = 1265200, ymax = 1271200 )
output_clipped <- clip_to_bbox(output, bounding_box)

output_clipped %>%
  ggplot() +                
  geom_sf(                
    aes(                  
      geometry = geometry,
      fill = TOT)    
  ) +
  coord_sf(
    xlim = c(bounding_box["xmin"], bounding_box["xmax"]),
    ylim = c(bounding_box["ymin"], bounding_box["ymax"]),
    expand = FALSE
  ) +
  scale_fill_viridis_c()

ggsave("_output/totpop_density_400m_manhatten_2023_basel.jpg")
