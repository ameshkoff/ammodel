#' RMPSE Metric for XGBoost
#'
#' RMPSE Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amm_rmpse <- function(preds
                      , dtrain) {

  labels <- getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- sqrt(mean((epreds / elab - 1) ^ 2))

  return(list(metric = "amm_rmpse", value = err))

}
