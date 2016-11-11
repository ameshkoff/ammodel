#' MAE Metric for XGBoost
#'
#' MAE Metric for XGBoost
#'
#' @param preds vector
#' @param dtrain vector
#' @return Vector
#' @seealso ...
#' @export

amm_mae <- function(preds
                    , dtrain) {

  labels <- xgboost::getinfo(dtrain, "label")
  elab <- as.numeric(labels)
  epreds <- as.numeric(preds)
  err <- mae(elab, epreds)

  return(list(metric = "amm_mae", value = err))

}
