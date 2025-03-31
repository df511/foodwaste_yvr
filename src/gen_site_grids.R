###### Started February 6, 2025
### by Daniel Forrest

# Load necessary libraries
library(sf)
library(terra)





if (gen_grids) {
  ### Set parameters
  
  # Define grid cell size (e.g., 2x2)
  ### size of a single side in meters
  #cell_size <- 2.2360679774998 ### for 5 sq. meters.
  #cell_size <- 5 ## for 25 sq. meters.
  cell_size <- 27.3861278 ### for ~750 sq. meters (I measured several parcels across the region and the range was ~600 to 900 sq. m)
  
  #### build functions
  
  # Function to create an internally fitting grid for each polygon with its site name
  create_internal_grid <- function(polygon, cell_size, site_name) {
    bbox <- st_bbox(polygon)
    
    # Generate grid in metric units
    grid <- st_make_grid(polygon, cellsize = cell_size, square = TRUE, 
                         offset = c(bbox["xmin"], bbox["ymin"]))  
    
    # Convert to sf object
    grid_sf <- st_sf(geometry = grid)
    
    # Keep only full grid cells within the polygon
    grid_inside <- grid_sf[st_within(grid_sf, polygon, sparse = FALSE), ]
    
    # Add site name as an attribute
    grid_inside$name <- site_name
    
    return(grid_inside)
  } 
  
  
  ###### read files
  
  lancaster <- st_read(here("data","sites","lancaster_sites.kml"))
  weber <- st_read(here("data","sites","weber_site_edits.kml"))
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
  sites_3857 <- st_transform(sites, crs = 3857)
  sites_3857$Name <- c("lancaster_cw","lancaster_sh", "lancaster_mk" ,
                       "lancaster_qe", "lancaster_fc",  "lancaster_dt",
                       "lancaster_we", "lancaster_ub",
                       "43_churchill","14_spruce", "19_yukon",  "strathcona", "2nd_beach")
  
  # ### plot to check correct naming
  # # Ensure 'name' column exists
  # if (!"Name" %in% colnames(sites_3857)) {
  #   stop("Column 'name' not found in sites_3857!")
  # }
  # 
  # # Create the plot
  # ggplot() +
  #   geom_sf(data = sites_3857, fill = "lightblue", color = "black") +  # Plot polygons
  #   geom_sf_text(data = sites_3857, aes(label = Name), size = 3, color = "red") +  # Add names as text
  #   theme_minimal() +
  #   labs(title = "Sites with Names", x = "Longitude", y = "Latitude")
  # 
  # 
  save(sites_3857, file = here("data","sites_3857.Rdata"))
  
  
  # Apply function to each polygon, retaining "name" column
  grid_list <- mapply(create_internal_grid, 
                      st_geometry(sites_3857), 
                      cell_size, 
                      sites_3857$Name, 
                      SIMPLIFY = FALSE)
  
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
  pdf(here("figs","grid_plot_750m.pdf"), width = 80, height = 64)  # Adjust size as needed
  
  # Plot the grid inside polygons
  plot(st_geometry(sites_3857), col = "lightblue", border = "black")
  plot(st_geometry(final_grid), add = TRUE, border = "red")
  
  # Close the PDF device to save the file
  dev.off()
  
  save(final_grid, file = here("data","grid_clipped_750m.Rdata"))
  #######
} else {
  ## file called "final_grid"
  load(file = here("data", "grid_clipped_750m.Rdata"))
}


