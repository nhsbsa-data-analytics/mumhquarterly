#' fast_agg_pred
#'
#' @param w -  A vector of weights
#' @param lmObject - A fitted linear regression model object
#' @param newdata - A data frame or list containing the predictor variables for which predictions are to be made
#' @param alpha - The confidence level for the confidence interval and prediction interval. Default is 0.95.
#'
#' @return - A list containing the aggregation mean, variance, confidence interval (CI), and prediction interval (PI)
#' @export
#'
#'
#' @details This function performs an aggregation prediction based on a linear regression model.
#' It calculates the mean of the aggregation using weights and the linear predictor from the inout data
#'
#'
#' @examples
#' Fit a linear regression model
#' lmObject <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#'
#' Make aggregation predictions
#' newdata <- data.frame(Sepal.Width = c(3.5, 4.2), Petal.Length = c(1.4, 5.1))
#' predictions <- fast_agg_pred(w = c(0.5, 0.5), lmObject = lmObject, newdata = newdata)
#'

fast_agg_pred <- function (w, lmObject, newdata, alpha = 0.95) {

  ## input checking

  if (!inherits(lmObject, "lm")) stop("'lmObject' is not a valid 'lm' object!")
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  if (length(w) != nrow(newdata)) stop("length(w) does not match nrow(newdata)")

  ## extract "terms" object from the fitted model, but delete response variable
  tm <- delete.response(terms(lmObject))

  ## linear predictor matrix
  Xp <- model.matrix(tm, newdata)

  ## predicted values by direct matrix-vector multiplication
  pred <- c(Xp %*% coef(lmObject))

  ## mean of the aggregation
  agg_mean <- c(crossprod(pred, w))

  ## residual variance
  sig2 <- c(crossprod(residuals(lmObject))) / df.residual(lmObject)

  ## efficiently compute variance of the aggregation without matrix-matrix computations

  QR <- lmObject$qr   ## qr object of fitted model
  piv <- QR$pivot     ## pivoting index
  r <- QR$rank        ## model rank / numeric rank

  u <- forwardsolve(t(QR$qr), c(crossprod(Xp, w))[piv], r)

  agg_variance <- c(crossprod(u)) * sig2

  ## adjusted variance of the aggregation
  agg_variance_adj <- agg_variance + c(crossprod(w)) * sig2

  ## t-distribution quantiles
  Qt <- c(-1, 1) * qt((1 - alpha) / 2, lmObject$df.residual, lower.tail = FALSE)

  ## names of CI and PI
  NAME <- c("lower", "upper")

  ## CI
  CI <- setNames(agg_mean + Qt * sqrt(agg_variance), NAME)

  ## PI
  PI <- setNames(agg_mean + Qt * sqrt(agg_variance_adj), NAME)

  ## return
  list(mean = agg_mean, var = agg_variance, CI = CI, PI = PI)
}
