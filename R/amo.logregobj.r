#' Logreg objective for XGBoost
#'
#' Logreg objective for XGBoost. Source: https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R#L14
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amo.logregobj <- function(preds
                      , dtrain) {

  labels <- getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)

  return(list(grad = grad, hess = hess))

}
