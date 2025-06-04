# Density aggregation of Swiss Statpop and Statent data

This project is meant to aggregate population or employment data by the Swiss Federal Statistics Office. It creates smoother density gradients based on the Swiss hectare grid.

## Project Structure

The project consists of several small scripts performing various normalizations and aggregation steps that enable the aggregation of data into a standardized grid-based density. These scripts and their basic functions are as follows:

* **00\_functions**: Holds common functions from scripts 02–04 for easier use in other projects.
* **01\_harmonization\_correction**: Creates a uniform input file for the later scripts. As data structure, variable names, and delimiters can vary from year to year, this script requires the most customization to achieve the desired results.
* **02\_grid**: Creates a `.gpkg` hectare grid onto which the aggregated data will be written.
* **03\_reference\_neighborhood**: Generates a reference table listing neighboring cells that should be considered for aggregation.
* **04\_aggregation**: Uses the reference table to group cells, sum population, and divide the sum across relevant cells.

**Data**

* **\_data**: Holds input data for the different scripts.
* **\_output**: Stores `.gpkg` files as well as the neighborhood reference table.

## Prerequisites

This project requires the following R packages. Install them before execution:

```R
install.packages(c("data.table", "dplyr", "sf", "tidyr", "ggplot2"))
```

## Usage

### 1. Harmonization and Correction

Depending on the year and export settings, some attributes of StatPop and StatEnt may change. This script harmonizes the data for consistent processing. Notable changes for StatPop include renaming variables to use the format `BXX` (e.g., `B18`), and removing populations without registered addresses (typically placed at town halls) to avoid artificially high densities.

The final section merges StatPop and StatEnt datasets to build a grid that includes as many cells in the built-up environment as possible.

**Script output requirement**: For compatibility with later scripts, the dataset must include two columns: `E_KOORD` and `N_KOORD`, representing the base point of each hectare cell (LV95 CRS). Data to be aggregated should be in additional columns with freely chosen names.

### 2. Grid

This script creates a `.gpkg` file reflecting the `E_KOORD` and `N_KOORD` grid of the input data. It includes two functions:

* `create_empties(grid, radius)`

  * Generates empty cells within a given radius of existing ones. This is useful for including parks or open spaces but may also create cells outside Switzerland or in lakes. Radius is measured in terms of maximum axis distance: `radius = max(abs(E_existing - E_new), abs(N_existing - N_new))`.
* `create_gpkg(grid)`

  * Creates a `.gpkg` file with a hectare raster based on the base points, including any empty cells if `create_empties()` was used.

### 3. Reference Table – Neighborhoods

This script defines neighboring relationships with the function:

* `find_neighbors(grid, radius)`

  * Produces a reference table listing, for each cell, all neighbors within the specified radius. Also includes indicators for whether a neighbor is within the Euclidean distance (`Circle`) or Manhattan norm (`Diamond`).

### 4. Aggregation

The main function for aggregation is:

* `aggregate_population(data, rel_vars, shape_filter, neighbors)`

  * Computes densities for all variables specified in the vector `rel_vars = c("Variable A", "Variable B", ...)`.
  * `data` must comply with section 1 (harmonized input).
  * `shape_filter` defines the distance norm: `Circle` (Euclidean), `Diamond` (Manhattan), or `Rectangle` (Maximum norm).
  * `neighbors` refers to the reference table from section 3.

The resulting data should be joined with the `.gpkg` file, making sure the join is based on cell location.

## Minimal Working Example using 00\_functions

### Data Harmonization

Download Statpop or Statent Data from [Statpop](https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/erhebungen/statpop.html) or [Statent](https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/arbeitsstaetten-beschaeftigung/statistik-unternehmensstruktur-statent-ab-2011.html) and store the downloaded folder in: `_data`

Start **01\_harmonization\_correction**:

#### Load Libraries and Clear Workspace

```R
library(data.table)
library(dplyr)
rm(list = ls())
```

#### Load Data

```R
statpop <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023.csv", sep = ";")
statpop_noloc <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023_NOLOC.csv", sep = ";")
```

#### Adjust Data as Needed

```R
# Merge and correct data
total_pop <- merge(statpop, statpop_noloc, by = "RELI", all.x = TRUE, suffixes = c("", "_n"))
cols_to_correct <- c("BBTOT")

for (col in cols_to_correct) {
  n_col <- paste0(col, "_n")
  total_pop[, (col) := get(col) - fifelse(is.na(get(n_col)), 0, get(n_col))]
}

# Final columns
cols_to_keep <- c("E_KOORD", "N_KOORD", "BBTOT")
total_pop <- total_pop %>% select(all_of(cols_to_keep)) %>% rename(TOT = BBTOT)

# Save
fwrite(total_pop, "_data/input/totpop2023.csv")
```

### Aggregation

#### Initialization

```R
rm(list = ls())
source("00_functions.R")
library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
```

#### Create Reference Grid

```R
coordinates <- fread("_data/input/totpop2023.csv") %>% select(E_KOORD, N_KOORD)
R <- 400
coordinates <- create_empties(coordinates, R)
output <- create_gpkg(coordinates)
```

#### Create Neighborhood Reference Table

```R
neighborhood <- find_neighbors(coordinates, R)
```

#### Density Calculation

```R
input <- fread("_data/input/totpop2023.csv")
setDT(input); setDT(neighborhood)
setkey(input, E_KOORD, N_KOORD)
setkey(neighborhood, E_neighbor, N_neighbor)

aggregated <- aggregate_population(input, c("TOT"), "Diamond", neighborhood)
output <- output %>% 
  left_join(aggregated, by = c("E_center", "N_center")) %>% 
  rename_with(~ sub("_sum_norm$", "", .x), ends_with("_sum_norm"))
```

#### Saving and Display

```R
st_write(output, "_output/totpop_density_400m_manhatten_2023.gpkg", layer = "squares", delete_layer = TRUE)

bounding_box <- c(xmin = 2608500, xmax = 2617800, ymin = 1265200, ymax = 1271200)
output_clipped <- clip_to_bbox(output, bounding_box)

output_clipped %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = TOT)) + 
  coord_sf(xlim = c(bounding_box["xmin"], bounding_box["xmax"]),
           ylim = c(bounding_box["ymin"], bounding_box["ymax"]),
           expand = FALSE) +
  scale_fill_viridis_c()

ggsave("_output/totpop_density_400m_manhatten_2023_basel.jpg")
```
