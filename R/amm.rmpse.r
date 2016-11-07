#' RMPSE Metric for XGBoost
#'
#' RMPSE Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amm.rmpse <- function(preds
                      , dtrain) {

  labels <- getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- sqrt(mean((epreds / elab - 1) ^ 2))

  return(list(metric = "amm.rmpse", value = err))

}