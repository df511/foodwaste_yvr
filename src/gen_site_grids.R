###### Started February 6, 2025


# Load necessary libraries
library(sf)
library(terra)

#### build functions

# Function to create an internally fitting grid for each polygon
create_internal_grid <- function(polygon, cell_size) {
  bbox <- st_bbox(polygon)
  
  # Generate grid in metric units
  grid <- st_make_grid(polygon, cellsize = cell_size, square = TRUE, 
                       offset = c(bbox["xmin"], bbox["ymin"]))  
  
  # Convert to sf object
  grid_sf <- st_sf(geometry = grid)
  
  # Keep only full grid cells within the polygon
  grid_inside <- grid_sf[st_within(grid_sf, polygon, sparse = FALSE), ]
  
  return(grid_inside)
}



###### read files

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
### size of a single side in meters
#cell_size <- 2.2360679774998 ### for 5 sq. meters.
cell_size <- 5 ## for 25 sq. meters.



# Apply function to each polygon separately
grid_list <- lapply(st_geometry(sites_3857), create_internal_grid, cell_size = cell_size)

# Combine grids
final_grid <- do.call(rbind, grid_list)

# ✅ Calculate cell area in square meters
cell_areas <- st_area(final_grid)

# Print mean cell area (if needed)
mean_cell_area <- mean(cell_areas)
print(paste("Mean cell size:", mean_cell_area, "square meters"))

# # Plot results
# plot(st_geometry(sites_3857), col = "lightblue", border = "black")
# plot(st_geometry(final_grid), add = TRUE, border = "red")
# 

# Define output file name and resolution
pdf(here("figs","grid_plot_25m.pdf"), width = 80, height = 64)  # Adjust size as needed

# Plot the grid inside polygons
plot(st_geometry(sites_3857), col = "lightblue", border = "black")
plot(st_geometry(final_grid), add = TRUE, border = "red")

# Close the PDF device to save the file
dev.off()


save(final_grid, file = here("data","grid_clipped_25m.Rdata"))
#######
