#' Generate pseudo-absence points
#'
#' Creates pseudo-absence points based on observed presence data for species distribution modeling.
#'
#' @param data A data frame containing presence points with coordinates.
#' @param x_col Character. Name of the longitude column.
#' @param y_col Character. Name of the latitude column.
#' @param n Integer. Number of pseudo-absence points to generate.
#' @param min_dist Numeric. Minimum distance to presence points to avoid overlap (optional).
#' @return A data frame combining original presence points (presence = 1) and pseudo-absence points (presence = 0).
#' @details
#' The function generates `n` pseudo-absence points randomly within the bounding box
#' defined by the presence points. If `min_dist` is provided, pseudo-absence points
#' closer than this distance to any presence point are discarded.
#' @examples
#' \dontrun{
#' # Example presence data
#' data <- data.frame(
#'   lon = c(10, 11, 12),
#'   lat = c(50, 51, 52)
#' )
#' 
#' # Generate 5 pseudo-absence points
#' result <- generate_pseudo_absence(
#'   data,
#'   x_col = "lon",
#'   y_col = "lat",
#'   n = 5,
#'   min_dist = 0.5
#' )
#' 
#' head(result)
#' }
#' @export


generate_pseudo_absence <- function(data, x_col, y_col, n, min_dist = NULL) {

  # Checks
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!(x_col %in% names(data)) || !(y_col %in% names(data))) {
    stop("x_col and y_col must exist in data")
  }

  # Bounding box
  xmin <- min(data[[x_col]])
  xmax <- max(data[[x_col]])
  ymin <- min(data[[y_col]])
  ymax <- max(data[[y_col]])

  # Presence coordinates
  pres_coords <- data[, c(x_col, y_col)]

  pseudo_list <- list()
  count <- 0

  while (count < n) {
    x_rand <- runif(1, xmin, xmax)
    y_rand <- runif(1, ymin, ymax)

    # Distance check
    if (!is.null(min_dist)) {
      dists <- sqrt((pres_coords[[x_col]] - x_rand)^2 +
                    (pres_coords[[y_col]] - y_rand)^2)

      if (min(dists) < min_dist) {
        next
      }
    }

    count <- count + 1
    pseudo_list[[count]] <- c(x_rand, y_rand)
  }

  pseudo_abs <- as.data.frame(do.call(rbind, pseudo_list))
  names(pseudo_abs) <- c(x_col, y_col)
  pseudo_abs$presence <- 0

  # Presence data
  presence_data <- data[, c(x_col, y_col)]
  presence_data$presence <- 1

  result <- rbind(presence_data, pseudo_abs)

  return(result)
}
