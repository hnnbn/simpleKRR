
#' Fit a Kernel Ridge Regression Model
#'
#' @description
#' Fits a kernel ridge regression model using
#' a Gaussian kernel.
#'
#' @param X Matrix of training predictors.
#' @param y Vector of training responses.
#' @param rho Gaussian kernel parameter.
#' @param lambda Regularization parameter.
#'
#' @return A list of class "krr" containing
#' coefficients, fitted_values, rho, X, and y.
#'
#' @rdname fit
#' @export
krr = function(X, y, rho = 1, lambda = 0.0001){
  n = nrow(X)
  K = matrix(0, ncol = n, nrow = n)
  for(i in 1:n)
    for(j in 1:n)
      K[i, j] = exp(-rho * sum((X[i, ] - X[j, ])^2))

  coef_hat = solve(K + diag(lambda, n), y)

  rslt = list("coefficients" = coef_hat,
              "fitted_values" = K %*% coef_hat,
              "rho" = rho,
              "X" = X,
              "y" = y)
  class(rslt) = "krr"

  return(rslt)
}
