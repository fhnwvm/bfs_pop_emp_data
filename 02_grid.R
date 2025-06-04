library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
rm(list = ls())


# Load Basic Data --------------------------------------------------

# Load data grid
coordinates <- fread("_data/input/grid.csv", sep = ",") %>%
  select(E_KOORD, N_KOORD)

#Define Radius
N <- 400


# Reference Table Functions -----------------------------------------------

#Creates Empty cells around cells where none exist. Creates a smoother density gradient
create_empties <- function(grid, radius){
  #Step 1: Create the offset dataset
  offsets <- CJ(
    dx = seq(-radius, radius, by = 100),
    dy = seq(-radius, radius, by = 100)
  )
  
  # Step 2: Expand grid with all offsets
  expanded <- grid[
    , .(E_KOORD = E_KOORD + offsets$dx, N_KOORD = N_KOORD + offsets$dy),
    by = .I  # keep track of original row (optional)
  ]
  
  # Step 3: Deduplicate — only keep unique grid points
  expanded <- unique(expanded[, .(E_KOORD, N_KOORD)])
  
  return(expanded)
}

create_gpkg <- function(data){
  geometries <- Map(function(e, n) {
    st_polygon(list(rbind(
      c(e - 0, n - 0),
      c(e + 100, n - 0),
      c(e + 100, n + 100),
      c(e - 0, n + 100),
      c(e - 0, n - 0)
    )))
  }, data$E_KOORD, data$N_KOORD)
  
  # Step 2: Add geometry to a new data.table
  squares <- data.table(
    E_center = data$E_KOORD,
    N_center = data$N_KOORD,
    geometry = st_sfc(geometries, crs = 2056)  
  )
  
  # Step 3 (optional): Convert to sf
  squares_sf <- st_as_sf(squares)
  return(squares_sf)
}

# Create Empties ----------------------------------------------------------

#Create the empty cells if required, leave #-out if not

#coordinates <- create_empties(coordinates, N)

# Execution ----------------------------------------------------------

output <- create_gpkg(coordinates)
st_write(output, "_output/grid.gpkg", layer = "squares", delete_layer = TRUE)
fwrite(coordinates, "_data/input/grid.csv")