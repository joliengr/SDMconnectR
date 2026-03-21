#' Extract environmental values for point data
#'
#' This function extracts environmental raster values at given point locations.
#'
#' @param points A data frame containing point coordinates
#' @param raster A SpatRaster object with environmental variables
#' @param x_col Name of the longitude column in points
#' @param y_col Name of the latitude column in points
#' @return A data frame combining the original points with extracted raster values
#' @examples
#' library(terra)
#' r1 <- rast(nrows=10, ncols=10)
#' values(r1) <- runif(ncell(r1))
#' r2 <- rast(nrows=10, ncols=10)
#' values(r2) <- runif(ncell(r2))
#' env <- c(r1, r2)
#' points <- data.frame(
#'   x = runif(5, xmin(env), xmax(env)),
#'   y = runif(5, ymin(env), ymax(env))
#' )
#' extract_env(points, env, "x", "y")
#' @export


extract_env <- function(points, raster, x_col, y_col) {
  
  # Checks
  if (!is.data.frame(points)) stop("points must be a data frame")
  
  if (!(x_col %in% names(points)) || !(y_col %in% names(points))) {
    stop("x_col and y_col must exist in points")
  }
  
  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a SpatRaster object (terra package)")
  }
  
  # Extract coordinates
  coords <- points[, c(x_col, y_col)]
  
  # Extract raster values
  vals <- terra::extract(raster, coords)
  
  # Remove ID column added by terra
  vals <- vals[, -1, drop = FALSE]
  
  # Combine
  result <- cbind(points, vals)
  
  return(result)
}


#################################
#BEISPIEL
######################

# library(terra)

# # Beispiel-Raster
# r1 <- rast(nrows=10, ncols=10)
# values(r1) <- runif(ncell(r1))

# r2 <- rast(nrows=10, ncols=10)
# values(r2) <- runif(ncell(r2))

# env <- c(r1, r2)

# # Beispiel-Punkte
# points <- data.frame(
#   x = runif(5, xmin(env), xmax(env)),
#   y = runif(5, ymin(env), ymax(env))
# )

# extract_env(points, env, "x", "y")