library(data.table)
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)
rm(list = ls())



aggregate_population <- function(data, rel_vars, shape_filter, neighbors) {
  # Defensive copies
  neighbors_dt <- as.data.table(copy(neighbors))
  data_dt <- as.data.table(copy(data))
  
  # Join all rel_vars at once
  for (var in rel_vars) {
    neighbors_dt[
      data_dt,
      (var) := get(var),
      on = .(E_neighbor = E_KOORD, N_neighbor = N_KOORD)
    ]
  }
  
  # Filter and group in data.table
  result <- neighbors_dt[get(shape_filter) != 0, 
                         c(lapply(.SD, function(x) sum(replace_na(x, 0), na.rm = TRUE)), .(n_cells = .N)),
                         by = .(E_center, N_center),
                         .SDcols = rel_vars
  ]
  
  setnames(result, old = rel_vars, new = paste0(rel_vars, "_sum"))
  
  result %>%
    mutate(across(ends_with("_sum"), ~ .x / n_cells, .names = "{.col}_norm")) %>%
    select(E_center, N_center, ends_with("_norm"))
  
  result <- result %>%
    mutate(across(ends_with("_sum"), ~ .x / n_cells, .names = "{.col}_norm")) %>%
    select(E_center, N_center, ends_with("_norm"))
  
  return(result)
}

# Initialization ---------------------------------------------------------

neighborhood <- fread("_output/neighborhoods+empties.csv")
output <- st_read("_output/grid.gpkg")
input <- fread("_data/input/2020.csv", sep = ",")


setDT(input)
setDT(neighborhood)

# Set keys
setkey(input, E_KOORD, N_KOORD)
setkey(neighborhood, E_neighbor, N_neighbor)


# Execution ---------------------------------------------------------------

aggregated <- aggregate_population(input, c("TOT", "B20BWTOT", "B20BMTOT"), "Diamond", neighborhood)

output <- output %>%
  left_join(aggregated, by = c("E_center", "N_center")) %>%
  rename_with(~ sub("_sum_norm$", "", .x), ends_with("_sum_norm"))

st_write(output, "_output/2020.gpkg", layer = "squares", delete_layer = TRUE)
