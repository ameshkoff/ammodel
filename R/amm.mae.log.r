#' MAE log Metric for XGBoost
#'
#' MAE log Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amm.mae.log <- function(preds, dtrain) {

  labels <- getinfo(dtrain, "label")
  elab <- exp(as.numeric(labels)) - 1
  epreds <- exp(as.numeric(preds)) - 1
  err <- mae(eab, epreds)

  return(list(metric = "amm.mae.log", value = err))

}
