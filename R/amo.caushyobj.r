#' Cauchy objective 3 for XGBoost
#'
#' Cauchy objective 3 for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amo.caushyobj3 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  c <- 3  #the lower the "slower/smoother" the loss is. Cross-Validate.
  x <-  preds-labels
  grad <- x / (x ^ 2 / c ^ 2 + 1)
  hess <- - c ^ 2 * ( x ^ 2 - c ^ 2) / (x ^ 2 + c ^ 2) ^ 2

  return(list(grad = grad, hess = hess))

}
