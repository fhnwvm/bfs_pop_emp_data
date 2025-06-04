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
  joined[, Circle := as.integer((E_center - E_neighbor)^2 + (N_center - N_neighbor)^2 <= radius^2)] 
  joined$Rectangle <- 1
  return(joined)
}


# Aggregation Functions ---------------------------------------------------


aggregate_population <- function(data, rel_vars, shape_filter, neighbors, fill) {
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
  
  if (fill == 1) {
    fill_value <- switch(shape_filter,
                         "Diamond" = 41,
                         "Circle" = 49,
                         "Rectangle" = 81,
                         1)
    result[, n_cells := fill_value]
  }

result <- result %>%
    mutate(across(ends_with("_sum"), ~ .x / n_cells, .names = "{.col}_norm")) %>%
    select(E_center, N_center, ends_with("_norm"))

  return(result)
}


# Create .gpkg using sf ---------------------------------------------------

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


# Restrict to Bounding Box ------------------------------------------------

clip_to_bbox <- function(sf_data, bbox) {
  # Define Basel bounding box in EPSG:2056
  basel_bbox <- st_as_sfc(st_bbox(bbox, crs = 2056))
  
  # Ensure input is in the same CRS
  sf_data <- st_transform(sf_data, crs = 2056)
  
  # Spatial intersection: keep only geometries that intersect with the bounding box
  sf_data_clipped <- st_intersection(sf_data, basel_bbox)
  
  return(sf_data_clipped)
}
