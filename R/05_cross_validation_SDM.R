#' Cross-validation for SDM (GLM)
#'
#' Performs k-fold cross-validation for a species distribution model.
#'
#' @param data Data frame with response and predictors
#' @param response Name of response column (e.g. "presence")
#' @param predictors Vector of predictor column names
#' @param k Number of folds (default = 5)
#' @param threshold Threshold for confusion matrix (default = 0.5)
#' @return List with mean AUC and all fold results
#' @export

cv_sdm_glm <- function(data, response, predictors, k = 5, threshold = 0.5) {
  
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!(response %in% names(data))) stop("response column not found")
  
  set.seed(123)
  
  # Shuffle data
  data <- data[sample(nrow(data)), ]
  
  # Create folds
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  
  auc_values <- c()
  results <- list()
  
  for (i in 1:k) {
    
    # Split
    test_idx <- which(folds == i)
    train_data <- data[-test_idx, ]
    test_data  <- data[test_idx, ]
    
    # Fit model
    model <- fit_sdm_glm(
      data = train_data,
      response = response,
      predictors = predictors
    )
    
    # Predict
    preds <- predict(model, newdata = test_data, type = "response")
    
    # AUC
    roc_obj <- pROC::roc(test_data[[response]], preds)
    auc <- as.numeric(roc_obj$auc)
    
    auc_values <- c(auc_values, auc)
    
    # Optional confusion matrix
    pred_class <- ifelse(preds >= threshold, 1, 0)
    cm <- table(Predicted = pred_class, Observed = test_data[[response]])
    
    results[[i]] <- list(
      AUC = auc,
      ConfusionMatrix = cm
    )
  }
  
  return(list(
    mean_AUC = mean(auc_values),
    sd_AUC = sd(auc_values),
    fold_results = results
  ))
}