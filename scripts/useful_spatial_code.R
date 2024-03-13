require(tidyverse)
require(sf)
require(stars)


# Single point to sf geometry:
pt <- st_sfc(st_point(c(0.5, 0.5))) # create points as SF objects (no CRS)

# Points to sf geometry:
pts <- data.frame(x = runif(100), y = runif(100)) %>% # simulate random points
	st_as_sf(coords = c("x", "y"), crs=NA) %>% # create simple feature (sf) collection of points
	st_geometry() # create 'geometry set', allowing merging with other types
is(pts)

# Link together points:
st_union(pts)

# Get bounding box: 
(bb <- st_bbox(pts))

# Get centroid of points (link together first):
(ce <- st_centroid(st_union(pts)))

# Add polygon buffer around an object:
(bu <- st_buffer(ce, 1.2)) # if used on points, converts to polygons

# Tests of uniform intensity (2st order CRS property) and clustering (2nd order CRS property)
envelope(ppattern, Linhom) %>% plot() # tests the 1st order property of CSR process: spatially constant intensity
envelope(ppattern, Lest) %>% plot() # tests the 2nd order properties of ‘completely independent locations’



#### Point process modelling ####

# Create a planar point pattern:
(ppattern <- as.ppp(pts, W = as.owin(bu)))

# Count numbers in 3x3 quadrats
(qcount <- quadratcount(ppattern, nx=3, ny=3))
# plot(qcount, main = ""); plot(ppattern, col = "red", add = TRUE)

# Test if data follow a completely spatially-random pattern using Chisq or MonteCarlo (for smaller sample sizes)
quadrat.test(ppattern, nx=3, ny=3, method = "MonteCarlo") # a completely spatially-random pattern

# Calculate kernel density raster:
kdensity <- density(ppattern, sigma = bw.diggle) # access function help using: ?spatstat.explore::density.ppp
plot(kdensity); plot(ppattern, col = "green", add = TRUE, pch=16, cex=0.5)

# Convert to stars raster object:
kdensity_stars <- st_as_stars(kdensity)

# Convert back to points:
kdensity_pts <- st_as_sf(kdensity_stars, as_points = TRUE, na.rm = FALSE)

## Modelling point processes

# Create a model of patterning based on distance from point [0.5, 0.5]
pt <- st_sfc(st_point(c(0.5, 0.5)))
kdensity_stars$dist <- kdensity_stars %>%
	st_as_sf(as_points = TRUE, na.rm = FALSE) %>%
	st_distance(pt) # calculate distance to the centre (0.5,0.5)


pt <- st_sfc(st_point(c(0.5, 0.5)))
kdensity_stars$dist <- st_distance(kdensity_pts, pt)

kdensity_dist_im <- as.im(kdensity_stars["dist"]) # save distances as image file
plot(kdensity_dist_im)

kdensity_stars$a <- st_area(kdensity_stars) |> suppressMessages()

# Model point pattern (ppp object) based on distance (x var, as image file)
(mod <- ppm(ppattern ~ dist, 
		  data = list(dist = kdensity_dist_im)))
plot(mod, se = FALSE)
