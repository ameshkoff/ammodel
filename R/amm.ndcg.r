#' NDCG internal function
#'
#' NDCG internal function. Source: https://www.kaggle.com/datadave/airbnb-recruiting-new-user-bookings/ndcg-score-r/code
#'
#' @param r ...
#' @param k ...
#' @return ...
#' @seealso ...
#' @export

amm.dcg_at_k <- function (r, k=min(5, length(r)) ) {
  #only coded alternative formulation of DCG (used by kaggle)
  r <- as.vector(r)[1:k]
  sum(( 2^r - 1 )/ log2( 2:(length(r)+1)) )
}

#' NDCG internal function
#'
#' NDCG internal function. Source: https://www.kaggle.com/datadave/airbnb-recruiting-new-user-bookings/ndcg-score-r/code
#'
#' @param r ...
#' @param k ...
#' @return ...
#' @seealso ...
#' @export

amm.ndcg_at_k <- function(r, k=min(5, length(r)) ) {
  r <- as.vector(r)[1:k]
  if (sum(r) <= 0) return (0)     # no hits (dcg_max = 0)
  dcg_max = dcg_at_k(sort(r, decreasing=TRUE)[1:k], k)
  return ( dcg_at_k(r, k) / dcg_max )
}

#' NDCG Metric for XGBoost
#'
#' NDCG Metric for XGBoost. Source: https://www.kaggle.com/datadave/airbnb-recruiting-new-user-bookings/ndcg-score-r/code
#'
#' @param preds matrix or data.frame: one row for each observation, one column for each prediction. Columns are sorted from left to right descending in order of likelihood.
#' @param dtrain vector
#' @return Vector: one row for each observation
#' @seealso ...

amm_ndcg <- function(preds, dtrain) {

  preds <- as.matrix(preds)
  dtrain <- as.vector(dtrain)

  stopifnot( length(dtrain) == nrow(preds))
  r <- apply( cbind( dtrain, preds), 1
              , function(x) ifelse( x == x[1], 1, 0))[ -1, ]
  if ( ncol(preds) == 1) r <-  rbind( r, r)  #workaround for 1d matrices
  as.vector( apply(r, 2, amm.ndcg_at_k) )
}
