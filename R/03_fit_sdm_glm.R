#' Fit a species distribution model using GLM
#'
#' Fits a binomial Generalized Linear Model (GLM) for species distribution modeling (SDM).
#' You can provide either a model formula or a response column plus predictor columns.
#'
#' @param data A data frame containing presence/absence response and predictor variables.
#' @param formula Optional formula for the model (overrides `response` and `predictors` if provided).
#' @param response Character. Name of the response column (used if `formula` is NULL).
#' @param predictors Character vector. Names of predictor columns (used if `formula` is NULL).
#' @return A fitted `glm` object with `family = binomial`.
#' @details
#' This function allows flexible model specification:
#' - If `formula` is provided, it is used directly.
#' - If `response` and `predictors` are provided, a formula is generated internally.
#' The output can be used for prediction and evaluation of species distribution.
#' @examples
#' \dontrun{
#' set.seed(42)
#' 
#' # Simulated presence/absence and environmental predictors
#' n <- 100
#' data <- data.frame(
#'   presence = rbinom(n, 1, 0.5),
#'   temp = rnorm(n, mean = 20, sd = 5),
#'   precip = rnorm(n, mean = 1000, sd = 200),
#'   elevation = rnorm(n, mean = 500, sd = 100)
#' )
#' 
#' # Fit model using response + predictors
#' model1 <- fit_sdm_glm(
#'   data = data,
#'   response = "presence",
#'   predictors = c("temp", "precip", "elevation")
#' )
#' summary(model1)
#' 
#' # Fit model using formula
#' model2 <- fit_sdm_glm(
#'   data = data,
#'   formula = presence ~ temp + precip + elevation
#' )
#' summary(model2)
#' }
#' @export

fit_sdm_glm <- function(data, formula = NULL, response = NULL, predictors = NULL) {
  
  if (is.null(formula)) {
    if (is.null(response) || is.null(predictors)) {
      stop("Provide either a formula or response and predictors")
    }
    
    formula <- as.formula(
      paste(response, "~", paste(predictors, collapse = " + "))
    )
  }
  
  model <- glm(formula, data = data, family = binomial)
  return(model)
}

