#' MCLogLoss Metric for XGBoost
#'
#' MCLogLoss Metric for XGBoost. Be careful! May contain errors
#'
#' @param actual vector
#' @param predicted vector
#' @param eps param
#' @return Vector
#' @seealso ...
#' @export

amm.mclogloss <- function(actual
                      , predicted
                      , eps = 1e-15) {
  predicted[predicted < eps] <- eps;
  predicted[predicted > 1 - eps] <- 1 - eps;
  -1/nrow(actual)*(sum(actual*log(predicted)))
}
