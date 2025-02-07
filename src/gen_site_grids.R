###### Started February 6, 2025


# Load necessary libraries
library(sf)
library(terra)


### read sites

lancaster <- st_read(here("data","sites","lancaster_sites.kml"))
weber <- st_read(here("data","sites","weber_sites.kml"))
weber_rep <- weber[1:3,] ## Weber sites I visited and surveyed (drop one outside of CoV borders)
plot(weber_rep) # check 
weber_lancaster <- do.call(rbind, list(lancaster, weber_rep))
#weber_lancaster_no43churchill <- do.call(rbind, list(lancaster, weber_rep2))

plot(weber_lancaster) # check

strathcona <- st_read(here("data","sites","strathcona_final.kml"))
plot(strathcona)
second_beach <- st_read(here("data","sites", "2nd_beach.kml"))
plot(second_beach)


sites <- do.call(rbind, list(lancaster, weber_rep, strathcona, second_beach))
sites_3857 <- st_transform(sites, 3857)



# Define grid cell size (e.g., 2x2)
cell_size <- 5 ### size of a single side in meters

# Create a grid covering the polygon's bounding box
bbox <- st_bbox(sites_3857)  # Get bounding box
grid <- expand.grid(
  x = seq(bbox["xmin"], bbox["xmax"], by = cell_size),
  y = seq(bbox["ymin"], bbox["ymax"], by = cell_size)
)

# Convert grid points to polygons (grid cells)
grid_sites <- st_make_grid(sites_3857, cellsize = cell_size, square = TRUE)
grid_sites_sf <- st_sf(geometry = grid_sites)  # Convert geometry to sf if needed
# Clip grid to the polygon (optional)
grid_clipped <- st_filter(grid_sites_sf, sites_3857)


# Plot the results
plot(st_geometry(sites_3857), col = "lightblue", border = "black")
plot(st_geometry(grid_clipped), add = TRUE, border = "red")


# Define output file name and resolution
pdf(here("figs","grid_plot.pdf"), width = 80, height = 64)  # Adjust size as needed

# Plot the grid inside polygons
plot(st_geometry(sites_3857), col = "lightblue", border = "black")
plot(st_geometry(grid_clipped), add = TRUE, border = "red")

# Close the PDF device to save the file
dev.off()


save(final_grid, file = here("data","grid_clipped.Rdata"))
#######




library(sf)

# Check the CRS of 'sites'
if (is.na(st_crs(sites))) {
  stop("CRS of 'sites' is missing! Assign a CRS using st_crs().")
}

# Transform to a metric CRS if needed (e.g., EPSG:3857 or UTM)
metric_crs <- 3857  # Web Mercator, or use your region's UTM
sites_metric <- st_transform(sites, crs = metric_crs)

# Define grid cell size in degrees (original)
cell_size <- 0.0002  # Adjust as needed

# Convert 'cell_size' from degrees to meters using an approximate conversion (if necessary)
# Only needed if starting in lat/lon (EPSG:4326)
if (st_crs(sites)$epsg == 4326) {
  # Approximate conversion for small-scale areas (~111,320m per degree at equator)
  cell_size_meters <- cell_size * 111320  
} else {
  cell_size_meters <- cell_size  # Already in meters
}

# Function to create an internally fitting grid for each polygon
create_internal_grid <- function(polygon, cell_size) {
  polygon_metric <- st_transform(polygon, crs = metric_crs)  # Convert to metric system
  bbox <- st_bbox(polygon_metric)
  
  # Generate grid in metric units
  grid <- st_make_grid(polygon_metric, cellsize = cell_size_meters, square = TRUE, 
                       offset = c(bbox["xmin"], bbox["ymin"]))  
  
  # Convert to sf object
  grid_sf <- st_sf(geometry = grid)
  
  # Keep only full grid cells within the polygon
  grid_inside <- grid_sf[st_within(grid_sf, polygon_metric, sparse = FALSE), ]
  
  return(grid_inside)
}

# Apply function to each polygon separately
grid_list <- lapply(st_geometry(sites_metric), create_internal_grid, cell_size = cell_size_meters)

# Combine grids
final_grid <- do.call(rbind, grid_list)

# ✅ Calculate cell area in square meters
cell_areas <- st_area(grid_clipped)

# Print mean cell area (if needed)
mean_cell_area <- mean(cell_areas)
print(paste("Mean cell size:", mean_cell_area, "square meters"))

# Plot results
plot(st_geometry(sites_metric), col = "lightblue", border = "black")
plot(st_geometry(final_grid), add = TRUE, border = "red")
