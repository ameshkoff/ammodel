#' Cauchy Log objective for XGBoost
#'
#' Cauchy Log objective for XGBoost. Be careful! May contain errors. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amo.caushyobj.log <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  grad <- tanh(preds - labels)
  hess <- 1 - grad * grad

  return(list(grad = grad, hess = hess))

}
