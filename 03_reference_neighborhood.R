library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
rm(list = ls())

# Load Data Grid ----------------------------------------------------------

# Load data grid
coordinates <- fread("_data/input/grid+empties.csv", sep = ",") %>%
  select(E_KOORD, N_KOORD)


# Function ----------------------------------------------------------------

# Creates Reference Table which contains which cells are neighbors to which other cells given varying distance norms
find_neighbors <- function(grid, radius){
  # Create a copy to represent the 'center' points
  center_points <- copy(grid)
  # Add bounding box columns to 'center_points' only to reduce calculation intensity
  center_points[, `:=`(
    Xmin = E_KOORD - radius,
    Xmax = E_KOORD + radius,
    Ymin = N_KOORD - radius,
    Ymax = N_KOORD + radius
  )]
  # Perform non-equi join with all cells inside defined bounding box
  joined <- center_points[grid,
                          on = .(Xmin <= E_KOORD, Xmax >= E_KOORD,
                                 Ymin <= N_KOORD, Ymax >= N_KOORD),
                          allow.cartesian = TRUE,
                          nomatch = 0L,
                          .(E_center = E_KOORD, N_center = N_KOORD,
                            E_neighbor = i.E_KOORD, N_neighbor = i.N_KOORD)
  ]
  
  joined[, Diamond := as.integer(abs(E_center - E_neighbor) + abs(N_center - N_neighbor) <= radius)]
  joined[, Circle := as.integer(sqrt(abs(E_center - E_neighbor)) + sqrt(abs(N_center - N_neighbor)) <= radius)] 
  joined$Rectangle <- 1
  return(joined)
}


# Execution ---------------------------------------------------------------


#Define Smooting Radius
R <- 400
neighborhood <- find_neighbors(coordinates, R)
fwrite(neighborhood, "_output/neighborhoods+empties.csv")