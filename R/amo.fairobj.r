#' Fair objective 2 for XGBoost
#'
#' Fair objective 2 for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amo.fairobj2 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}

#' Fair objective 2.2 for XGBoost
#'
#' Fair objective 2.2 for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amo.fairobj2.2 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- 2.2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}

#' Fair objective 2.5 for XGBoost
#'
#' Fair objective 2.5 for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amo.fairobj2.5 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- 2.5
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}

#' Fair objective 1.5 for XGBoost
#'
#' Fair objective 1.5 for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amo.fairobj1.5 <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- 1.5
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}

#' Fair objective e for XGBoost
#'
#' Fair objective e for XGBoost. Source: https://www.kaggle.com/dmi3kno/allstate-claims-severity/farons-xgb-starter-with-custom-objective
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amo.fairobje <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  con <- exp(1)
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2

  return(list(grad = grad, hess = hess))

}
