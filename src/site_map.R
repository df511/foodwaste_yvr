

#### explore site locations

### load libraries
library(cancensus)
library(ggplot2)
#library(tidyverse)
library(sf)
library(geojsonsf)
library(viridis)
library(matrixStats)
library(ggmap)
library(terra)
library(tidyterra)
library(here)


### insert your own Google Maps key here
options(cancensus.api_key = 'CensusMapper_e5809a95267db864f07fd91906dfdc24')


## define functions 


##### allows for geom_sf layering with ggmap; see glitch fix: https://github.com/dkahle/ggmap/issues/160

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}



##### read in transects data

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

# french_path <- './initial_setup/99_French.kml'
# ninetynine_french <- st_read(french_path)
# plot(ninetynine_french)

# strathcona_3_path <- './initial_setup/strathcona3.kml'
# strathcona_3 <- st_read(strathcona_3_path)
# plot(strathcona_3)

# Riley_Park_path <- './initial_setup/Riley_Park.kml'
# Riley_Park <- st_read(Riley_Park_path)
# plot(Riley_Park)
# 
# fortythird_joyce_path <- here('initial_setup',"43_joyce.kml")
# fortythird_joyce <- st_read(fortythird_joyce_path)
# 
# seventysecond_cartier_path <- here('initial_setup',"72_cartier.kml")
# seventysecond_cartier <- st_read(seventysecond_cartier_path)
# 
# transfer_stn_lot_spot_path <- here('initial_setup', "transfer_stn_lot_spot.kml")
# transfer_stn_lot_spot <- st_read(transfer_stn_lot_spot_path)
# 
# baseball_lot_spot_path <- here('initial_setup', "baseball_lot_spot.kml")
# baseball_lot_spot <- st_read(baseball_lot_spot_path)
# 
# granville_island_spot_path <- here('initial_setup', "granville_island_spot.kml")
# granville_island_spot <- st_read(granville_island_spot_path)
# 
# oppenheimer_park_spot_path <- here('initial_setup', "oppenheimer_park_spot.kml")
# oppenheimer_park_spot <- st_read(oppenheimer_park_spot_path)
# 


sites <- do.call(rbind, list(lancaster, weber_rep, strathcona, second_beach))
#allsites_new_spot <- do.call(rbind, list(lancaster, weber_rep, strathcona, second_beach, fortythird_joyce, seventysecond_cartier, transfer_stn_lot_spot, baseball_lot_spot, granville_island_spot, oppenheimer_park_spot)) # add spot surveys here
#allsites_new <- do.call(rbind, list(strathcona, second_beach, fortythird_joyce, seventysecond_cartier, transfer_stn_lot_spot, baseball_lot_spot, granville_island_spot, oppenheimer_park_spot)) # add spot surveys here
new_sites<- do.call(rbind, list(strathcona, second_beach))

#weber_lancaster_potential <- do.call(rbind, list(lancaster, weber_rep, strathcona_3, Riley_Park, French))
#sites_plus_french <- do.call(rbind, list(lancaster, weber_rep, strathcona_3,  French))

#plot(weber_lancaster_new)
#plot(allsites_new_spot)

#map <- get_map("Vancouver", maptype = "satellite", zoom = 12, source = "google")

#map <- get_map(c(left = -123.22, bottom = 49.21, right = -123.08, top = 49.32), maptype = "satellite", zoom = 12, source = "google")
map <- get_map(c(-123.14, 49.26), maptype = "satellite", zoom = 12, source = "google")



# convert crs to 3857 for plotting with google maps
weber_3857 <- st_transform(weber_rep, 3857)
lanc_3857 <- st_transform(lancaster, 3857)
# allsites_new_3857 <- st_transform(allsites_new, 3857)
new_sites_3857 <- st_transform(new_sites, 3857)
sites_3857 <- st_transform(sites, 3857)



map <- ggmap_bbox(map) # Use the function

ggmap(map) + geom_sf(data = sites_3857, inherit.aes = F)


sites_map_annotated <- ggmap(map) + 
  geom_sf(data = weber_3857, inherit.aes = F, color = "white", fill = "#000000", lwd = 0.5) +
  geom_sf(data = lanc_3857, inherit.aes = F, color = "white", fill = "#0072B2", lwd = 0.5) + 
  geom_sf(data = new_sites_3857, inherit.aes = F, color = "white", fill = "#D55E00", lwd = 0.5) + 
  annotate(
    "label", label = "Weber (1973); Eyster (2020); Forrest (2023)",
    x = -13713000, y = 6313000, size = 5, color = "#000000"
  ) + 
  annotate(
    "label", label = "Lancaster (1979); Eyster (2020); Forrest (2023)",
    x = -13713000, y = 6322000, size = 5, color = "#0072B2"
  )+ 
  annotate(
    "label", label = "Forrest (2023)",
    x = -13703000, y = 6325600, size = 5, color = "#D55E00"
  )

sites_map_annotated

ggsave(here("figs","sites_map.pdf"), dpi = 600, scale = 2)


# sites_map <- ggmap(map) + 
#   geom_sf(data = sites_3857, inherit.aes = F, color = 'yellow', fill = NA, lwd = 1)
#   
# sites_map
# 
# ggsave("figures/sites_map.pdf")

#read in census data via query of census mapper
census_data <- get_census(dataset = 'CA21', regions = list(CSD = "5915022"), 
                          
                          vectors = c("v_CA21_1","v_CA21_906","v_CA21_6",
                                      "v_CA21_5808","v_CA21_5865", "v_CA21_4204", 
                                      "v_CA21_4875","v_CA21_4878", "v_CA21_4881",
                                      "v_CA21_4884", "v_CA21_4887", "v_CA21_4890",
                                      "v_CA21_4893", "v_CA21_4896", "v_CA21_4899",
                                      "v_CA21_4902", "v_CA21_4905", "v_CA21_4908",
                                      "v_CA21_4911","v_CA21_4914"),
                          labels = "detailed", geo_format = "sf", level = "DA")

#rename columns
census_data <- census_data %>% 
  rename(household_income = `v_CA21_906: Median total income of household in 2020 ($)`,
         population = `v_CA21_1: Population, 2021`,
         minority_pop = `v_CA21_4875: Total visible minority population`,
         black_pop = `v_CA21_4884: Black`, indig_pop = 'v_CA21_4204: Indigenous identity (39)',
         pop_km = `v_CA21_6: Population density per square kilometre`
  )

#calculate percemtages of ethnicities per population and create new cols in df
census_data$black_percent <- census_data$black_pop / census_data$population
census_data$indig_percent <- census_data$indig_pop / census_data$population
census_data$min_percent <- census_data$minority_pop / census_data$population

#ggplot(census_data) + geom_sf(aes(fill = household_income))


income_map  <- ggplot() + geom_sf(data = census_data, aes(fill = household_income)) + 
  scale_fill_viridis(direction = -1)  + 
  geom_sf(data = sites, color = 'black', fill = NA, lwd = 1) +
  scale_y_continuous(breaks = seq(49.2, 49.34, by = 0.025)) + 
  scale_x_continuous(breaks = seq(-123.25, -123, by = 0.05))

# income_map_spot  <- ggplot() + geom_sf(data = census_data, aes(fill = household_income)) + 
#   scale_fill_viridis(direction = -1)  + 
#   geom_sf(data = allsites_new_spot, color = 'red', fill = NA, lwd = 1) +
#   scale_y_continuous(breaks = seq(49.2, 49.34, by = 0.025)) + 
#   scale_x_continuous(breaks = seq(-123.25, -123, by = 0.05))

income_map
# income_map_spot


ggsave(here("figs","income_map.pdf"), dpi = 600, scale = 2)

hist(census_data$household_income, breaks = 30)





lowrise_map  <- ggplot() + geom_sf(data = census_data, aes(fill = low_rise)) + 
  scale_fill_viridis(direction = -1)  + 
  geom_sf(data = sites, color = 'black', fill = NA, lwd = 1) +
  scale_y_continuous(breaks = seq(49.2, 49.34, by = 0.025)) + 
  scale_x_continuous(breaks = seq(-123.25, -123, by = 0.05))

lowrise_map

highrise_map  <- ggplot() + geom_sf(data = census_data, aes(fill = high_rise)) + 
  scale_fill_viridis(direction = -1)  + 
  geom_sf(data = sites, color = 'black', fill = NA, lwd = 1) +
  scale_y_continuous(breaks = seq(49.2, 49.34, by = 0.025)) + 
  scale_x_continuous(breaks = seq(-123.25, -123, by = 0.05))

highrise_map


highrise_map  <- ggplot() + geom_sf(data = census_data, aes(fill = high_rise)) + 
  scale_fill_viridis(direction = -1)  + 
  geom_sf(data = sites, color = 'black', fill = NA, lwd = 1) +
  scale_y_continuous(breaks = seq(49.2, 49.34, by = 0.025)) + 
  scale_x_continuous(breaks = seq(-123.25, -123, by = 0.05))

highrise_map
