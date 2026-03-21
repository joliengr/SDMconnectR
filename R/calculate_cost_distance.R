#' Calculate least-cost paths between points
#'
#' This function computes least-cost paths between all points based on a
#' resistance raster using the leastcostpath package.
#'
#' @param raster A SpatRaster representing resistance values
#' @param points A data frame containing point coordinates
#' @param x_col Name of the longitude column in points
#' @param y_col Name of the latitude column in points
#' @return An sf object with least-cost paths and cost distances
#' @examples
#' library(terra)
#' library(sf)
#' library(leastcostpath)
#' r <- rast(nrows = 20, ncols = 20)
#' values(r) <- runif(ncell(r)) + 0.1
#' points <- data.frame(
#'   x = runif(4, xmin(r), xmax(r)),
#'   y = runif(4, ymin(r), ymax(r))
#' )
#' lcps <- calculate_cost_distance(r, points, "x", "y")
#' plot(r)
#' plot(sf::st_geometry(lcps), add = TRUE, col = "red", lwd = 2)
#' @export


calculate_cost_distance <- function(raster, points, x_col, y_col) {
  
  # Checks
  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a SpatRaster object (terra package)")
  }
  
  if (!is.data.frame(points)) {
    stop("points must be a data frame")
  }
  
  if (!(x_col %in% names(points)) || !(y_col %in% names(points))) {
    stop("x_col and y_col must exist in points")
  }
  
  # Convert to sf
  pts_sf <- sf::st_as_sf(points, coords = c(x_col, y_col), crs = terra::crs(raster))
  
  # Convert resistance -> conductance
  conductance <- 1 / raster
  
  # Create conductance surface
  cs <- leastcostpath::create_cs(conductance)
  
  # Least-cost paths (with distances)
  lcps <- leastcostpath::create_FETE_lcps(
    x = cs,
    locations = pts_sf
  )
  
  return(lcps)
}

#############
##BEISPIEL
##############
# library(terra)
# library(sf)
# library(leastcostpath)

# # Raster (Resistance)
# r <- rast(nrows = 20, ncols = 20)
# values(r) <- runif(ncell(r)) + 0.1

# # Punkte
# points <- data.frame(
#   x = runif(4, xmin(r), xmax(r)),
#   y = runif(4, ymin(r), ymax(r))
# )

# # Test
# lcps <- calculate_cost_distance(r, points, "x", "y")

# lcps

# plot(r)
# plot(sf::st_geometry(lcps), add = TRUE, col = "red", lwd = 2)
