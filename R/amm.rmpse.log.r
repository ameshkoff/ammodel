#' RMPSE log Metric for XGBoost
#'
#' RMPSE log Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amm_rmpse_log <- function(preds
                          , dtrain) {

  labels <- getinfo(dtrain, "label")
  elab <- exp(as.numeric(labels)) - 1
  epreds <- exp(as.numeric(preds)) - 1
  err <- sqrt(mean((epreds / elab - 1) ^ 2))

  return(list(metric = "amm_rmpse_log", value = err))

}
