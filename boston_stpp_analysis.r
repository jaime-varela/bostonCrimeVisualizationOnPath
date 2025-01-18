#install.packages("sf")
library("sf")

# load the data and convert to dates
boston_data <- read.csv("data/crimesBoston.csv", stringsAsFactors = FALSE)
# only violent crimes
boston_data <- read.csv("data/violentCrimes.csv", stringsAsFactors = FALSE)
boston_data <- read.csv("data/homicides.csv")

boston_data$date_time = as.POSIXct(boston_data$OCCURRED_ON_DATE, format = "%Y-%m-%d %H:%M:%S")

# convert time to be hours since the start time
# Find the minimum date-time
min_datetime <- min(boston_data$date_time)

# Calculate hours since the minimum date-time
boston_data$hours_since_start <- as.numeric(difftime(boston_data$date_time, min_datetime, units = "hours"))


crimes_with_locations <- boston_data[!is.na(boston_data$Lat) & !is.na(boston_data$Long), ]

# Assuming 'Lat' and 'Long' columns in your data
coords <- data.frame(Long = crimes_with_locations$Long, Lat = crimes_with_locations$Lat)

# Convert to sf object
sf_points <- st_as_sf(coords, coords = c("Long", "Lat"), crs = 4326)  # WGS84

# Transform to UTM (example: Zone 33N, adjust for your region)
sf_points_utm <- st_transform(sf_points, crs = 32619)

# Extract Northing and Easting
utm_coords <- st_coordinates(sf_points_utm)
crimes_with_locations$Easting <- utm_coords[, "X"]
crimes_with_locations$Northing <- utm_coords[, "Y"]

#center_easting = mean(crimes_with_locations$Easting)
#center_northing = mean(crimes_with_locations$Northing)
# using city center easting northing center
center_easting = 330600.55
center_northing = 4691924.48


spatio_temporal_data_all = data.frame(x=(crimes_with_locations$Easting - center_easting), y=(crimes_with_locations$Northing - center_northing),t=crimes_with_locations$hours_since_start)

min(spatio_temporal_data_all$x)
max(spatio_temporal_data_all$x)

# remove large outlier space time point
spatio_temporal_far <- spatio_temporal_data_all[spatio_temporal_data_all$x > 1e6 , ]
crime_at_large_distance <- crimes_with_locations[spatio_temporal_data_all$x > 1e6 ,] # only one data point

spatio_temporal_data <- spatio_temporal_data_all[spatio_temporal_data_all$x < 1e6, ]
spatio_temporal_data <- spatio_temporal_data[order(spatio_temporal_data$t), ]

# add some noise in time to remove redundancy ( assuming crime data is not accurate at the five minute level)
noise_in_hours <- runif(length(spatio_temporal_data$t), min = 0, max = 5) / 60

spatio_temporal_data$t = spatio_temporal_data$t + noise_in_hours

# Plot the spatio-temporal points for all crimes
#install.packages("stopp")
library("stopp")


all_crimes_stpp = stp(spatio_temporal_data)

plot(all_crimes_stpp, tcum = TRUE)


#min(spatio_temporal_data$y)
#max(spatio_temporal_data$y)
#min(spatio_temporal_data$x)
#max(spatio_temporal_data$x)

#all_crimes_stpp
#plot(all_crimes_stpp, tcum = FALSE)

# estimate the spate-time densities
#install.packages("sf")
#install.packages("tigris")
#install.packages("ggplot2") # Optional for visualization
library(sf)
library(tigris)
library(ggplot2)
library("stpp")


# Set options to use sf objects
options(tigris_use_sf = TRUE)
# Download polygon data for Massachusetts
ma_places <- places(state = "MA", year = 2022)
# Filter for Boston
boston_boundary <- ma_places[ma_places$NAME == "Boston", ]
# View the data structure
print(boston_boundary)
# Plot the boundary
ggplot(data = boston_boundary) +
  geom_sf() +
  ggtitle("Boston City Boundary")

# Transform the boundary to UTM Zone 19N
boston_boundary_utm <- st_transform(boston_boundary, crs = 32619)

# Extract coordinates
coords <- st_coordinates(boston_boundary_utm)

# Define the values to subtract (e.g., x and y UTM shifts)
x_shift <- center_easting  # Example UTM easting value
y_shift <- center_northing # Example UTM northing value

# Subtract the UTM values
coords[, "X"] <- coords[, "X"] - x_shift
coords[, "Y"] <- coords[, "Y"] - y_shift

# Rebuild the geometry with modified coordinates
boston_boundary_shifted <- st_sf(
  st_sfc(st_polygon(list(coords[, 1:2]))),
  crs = 32619
)
# Extract coordinates
shifted_coords <- st_coordinates(boston_boundary_shifted)
# Keep only the x and y columns
polygon_matrix <- shifted_coords[, c("X", "Y")]

# FIXME polygon is not working
plot(all_crimes_stpp, s.region = polygon_matrix,tcum = TRUE)

