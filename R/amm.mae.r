#' MAE Metric for XGBoost
#'
#' MAE Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...

amm.mae <- function(preds
                    , dtrain) {

  labels <- getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab, epreds)

  return(list(metric = "amm.mae", value = err))

}
