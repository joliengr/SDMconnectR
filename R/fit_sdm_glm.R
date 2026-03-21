#' Fit a species distribution model using GLM
#'
#' This function fits a binomial GLM-based species distribution model (SDM)
#' using either a formula or a response + predictor columns.
#'
#' @param data A data frame containing presence/absence and predictors
#' @param formula Optional model formula (overrides response and predictors if provided)
#' @param response Name of the response column (used if formula is NULL)
#' @param predictors Vector of predictor column names (used if formula is NULL)
#' @return A fitted glm object (family = binomial)
#' @examples


fit_sdm_glm <- function(data, formula) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  if (!inherits(formula, "formula")) {
    stop("formula must be of class 'formula'")
  }
  
  model <- glm(formula, data = data, family = binomial)
  return(model)
}

#OPTION 2

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



################################################
### BEISPIEL SPÄTER LÖSCHEN
###############################


# set.seed(42)

# # Simulierte Daten
# n <- 100
# data <- data.frame(
#   presence = rbinom(n, 1, 0.5),   # 0/1 Zielvariable (Pseudo-Absence + Presence)
#   temp = rnorm(n, mean = 20, sd = 5),
#   precip = rnorm(n, mean = 1000, sd = 200),
#   elevation = rnorm(n, mean = 500, sd = 100)
# )

# model1 <- fit_sdm_glm(
#   data = data,
#   response = "presence",
#   predictors = c("temp", "precip", "elevation")
# )

# summary(model1)

# model2 <- fit_sdm_glm(
#   data = data,
#   formula = presence ~ temp + precip + elevation
# )

# summary(model2)