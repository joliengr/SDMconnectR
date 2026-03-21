#' Evaluate species distribution model
#'
#' This function evaluates a fitted SDM using AUC and optional threshold-based metrics.
#'
#' @param model A fitted GLM (binomial) from fit_sdm_glm()
#' @param data Data frame used for evaluation (with response and predictors)
#' @param response Name of the response column in data
#' @param threshold Optional numeric threshold to convert probabilities to presence/absence (default = 0.5)
#' @return A list with AUC and, if threshold is provided, confusion matrix
#' @examples
#' library(pROC)
#' set.seed(123)
#' data <- data.frame(
#'   x1 = runif(50),
#'   x2 = runif(50),
#'   presence = sample(0:1, 50, replace = TRUE)
#' )
#' model <- fit_sdm_glm(data, response = "presence", predictors = c("x1", "x2"))
#' eval <- evaluate_sdm(model, data, response = "presence", threshold = 0.5)
#' eval
#' @export

evaluate_sdm <- function(model, data, response, threshold = 0.5) {
  
  if (!inherits(model, "glm")) stop("model must be a GLM object")
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!(response %in% names(data))) stop("response column not found in data")
  
  # Predicted probabilities
  preds <- predict(model, newdata = data, type = "response")
  
  # Load pROC for AUC
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Please install the 'pROC' package")
  }
  
  auc <- pROC::roc(data[[response]], preds)$auc
  
  result <- list(AUC = as.numeric(auc))
  
  # Confusion matrix if threshold is given
  if (!is.null(threshold)) {
    pred_class <- ifelse(preds >= threshold, 1, 0)
    cm <- table(Predicted = pred_class, Observed = data[[response]])
    result$ConfusionMatrix <- cm
  }
  
  return(result)
}


# ##############################
# ##BEISPIEL
# #########################

# library(pROC)

# # Beispiel-Daten
# set.seed(123)
# data <- data.frame(
#   x1 = runif(50),
#   x2 = runif(50),
#   presence = sample(0:1, 50, replace = TRUE)
# )

# # Fit SDM
# model <- fit_sdm_glm(data, response = "presence", predictors = c("x1", "x2"))

# # Evaluate
# eval <- evaluate_sdm(model, data, response = "presence", threshold = 0.5)
# eval