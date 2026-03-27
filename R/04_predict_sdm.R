#' Predict species distribution on raster data
#'
#' This function applies a fitted SDM model to environmental raster data
#' and returns a raster of predicted suitability values.
#'
#' @param model A fitted model object (e.g. from fit_sdm_glm)
#' @param raster A SpatRaster object with environmental predictor variables
#' @param type Type of prediction (default = "response")
#' @return A SpatRaster with predicted suitability values
#' @examples
#' library(terra)
#' r1 <- rast(nrows=20, ncols=20)
#' values(r1) <- runif(ncell(r1))
#' r2 <- rast(nrows=20, ncols=20)
#' values(r2) <- runif(ncell(r2))
#' env <- c(r1, r2)
#' names(env) <- c("temp", "precip")
#' points <- data.frame(
#'   x = runif(50, xmin(env), xmax(env)),
#'   y = runif(50, ymin(env), ymax(env)),
#'   presence = rbinom(50, 1, 0.5)
#' )
#' data_model <- extract_env(points, env, "x", "y")
#' model <- fit_sdm_glm(
#'   data = data_model,
#'   response = "presence",
#'   predictors = c("temp", "precip")
#' )
#' pred_raster <- predict_sdm(model, env)
#' plot(pred_raster)
#' @export


predict_sdm <- function(model, raster, type = "response") {
  
  # Checks
  if (is.null(model)) stop("model must be provided")
  
  if (!inherits(raster, "SpatRaster")) {
    stop("raster must be a SpatRaster object (terra package)")
  }
  
  # Predict
  pred <- terra::predict(raster, model, type = type)
  
  return(pred)
}
