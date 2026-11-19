
#' Predict with a Kernel Ridge Regression Model
#'
#' @description
#' Predicts responses for new observations
#' using a fitted Gaussian KRR model.
#'
#' @param object A fitted KRR model of class "krr".
#' @param newdata Matrix of new predictor values.
#' @param ... Additional arguments (not used).
#'
#' @return A matrix of predicted response values.
#'
#' @rdname predict
#' @export
predict.krr = function(object, newdata, ...){
  X = object$X
  n = nrow(X)

  n_new = nrow(newdata)
  K_new = matrix(0, ncol = n, nrow = n_new)

  for(i in 1:n_new)
    for(j in 1:n)
      K_new[i, j] = exp(-object$rho *
                          sum((newdata[i, ] - X[j, ])^2))

  pred_values = K_new %*% object$coefficients
  return(pred_values)
}
