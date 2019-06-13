
#' Cross Validation Auto ARIMA
#'
#' @description This function finds the ARIMA model that minimizes average RMSE via grid search and time-series cross validation.
#'      All cominations of (p, d, q) and (P, D, Q) terms are considered, as well as the inclusion of exogenous regressors.
#'
#' @param y A numeric vector
#' @param x A numeric matrix of dimension n,k where n is the length of \code{y} and k is the number of potential regressors
#' @param p Max AR order
#' @param d Max degree of differencing
#' @param q Max MA order
#' @param P Max seasonal AR order, defaults to 0 if period is \code{NA}
#' @param D Max seasonal degree of differencing, defaults to 0 if period is \code{NA}
#' @param Q Max seasonal MA order, defaults to 0 if period is \code{NA}
#' @param period Frequency of the time series
#' @param max_parameters Max sum of AR, MA, seasonal AR, seasonal MA, and k
#' @param initialWindow The initial number of consecutive values in each training set sample, defaults to 70\% of length(y)
#' @param horizon The number of consecutive values in test set sample
#' @param fixedWindow Logical, if \code{FALSE}, all training samples start at 1
#' @param skip Integer, how many (if any) resamples to skip to thin the total amount
#' @param include.mean Should the ARMA model include a mean/intercept term?
#'      The default is \code{TRUE} for undifferenced series, and it is ignored for ARIMA models with differencing.
#' @param transform.pars Logical; if true, the AR parameters are transformed to ensure that they remain in the region of stationarity.
#'      Not used for \code{method = "CSS"}. For \code{method = "ML"}, it has been advantageous to set \code{transform.pars = FALSE}
#'      in some cases, see also \code{fixed}.
#' @param fixed Optional numeric vector of the same length as the total number of parameters. If supplied, only \code{NA} entries
#'      in \code{fixed} will be varied. \code{transform.pars = TRUE} will be overridden (with a warning) if any AR parameters are
#'      fixed. It may be wise to set \code{transform.pars = FALSE} when fixing MA parameters, especially near non-invertibility.
#' @param Optional numeric vector of initial parameter values. Missing values will be filled in, by zeroes except for regression
#'      coefficients. Values already specified in \code{fixed} will be ignored.
#' @param method Fitting method: maximum likelihood or minimize conditional sum-of-squares. The default (unless there are missing
#'      values) is to use conditional-sum-of-squares to find starting values, then maximum likelihood. Can be abbreviated.
#'
#' @return A list containing the following:
#'      \itemize{
#'           \item\code{best_model} The ARIMA model with the lowest RMSE
#'           \item\code{best_model_params} The best ARIMA model parameters, and RMSE
#'           \item\code{best_index} THe row number of the best ARIMA model in the grid
#'           \item\code{grid} The grid of all possible ARIMA models, and corresponding average RMSE found via CV
#'           \item\code{inputs} A list of inputs provided to \code{auto_arima_cv()}
#'           }
#'
#'
#' @details
#'      \enumerate{
#'         \item If a parallel backend is registered, this function runs in parallel via \%dopar\%.
#'         \item If a model does not converge it does not appear in the average RMSE calculation.
#'         }
#'
#' @seealso \code{\link{arima}}, \code{\link{auto.arima}}
#'
#' @export
#'
#' @examples
#' D <- ts(matrix(rnorm(150), ncol = 3))
#' y <- D[-1,1]
#' x <- D[-nrow(D),-1]
#' # x variables are lagged by 1, with NAs removed
#'
#' doParallel::registerDoParallel() # setup parallel backend
#'
#' model <- auto_arima_cv(y = y)
#'
#' foreach::registerDoSEQ() # return to sequential processing
#'

auto_arima_cv <- function(y, x = NULL,
                          p = 2, d = 1, q = 2,
                          P = 1, D = 1, Q = 1,
                          period = NA, max_parameters = NULL,
                          initialWindow = as.integer(0.7*length(y)),
                          horizon = 1, fixedWindow = T, skip = 0,
                          include.mean = TRUE, transform.pars = TRUE,
                          fixed = NULL, init = NULL,
                          method = c("CSS-ML", "ML", "CSS")){

  if(!is.null(x)){
    if(length(y) != nrow(x)){stop("Make sure x and y are the same length.")}
  }

  `%:%` <- foreach::`%:%`
  `%dopar%` <- foreach::`%dopar%`

  TD <- caret::createTimeSlices(1:length(y),
                                initialWindow = initialWindow,
                                horizon = horizon,
                                fixedWindow = fixedWindow,
                                skip = skip)

  if(is.na(period)){P <- D <- Q <- 0}

  grid <- list("p" = 0:p, "d" = 0:d, "q" = 0:q,
               "P" = 0:P, "D" = 0:D, "Q" = 0:Q)

  if(!is.null(x)){

    if(is.null(colnames(x))){colnames(x) <- paste0('X', 1:ncol(x))}

    for(i in colnames(x)){grid[[i]] <- 0:1}

    }

  grid <- expand.grid(grid)

  if(!is.null(max_parameters)){
    grid <- grid[rowSums(grid[, -c(2, 5)]) <= max_parameters ,]
  }

  grid <- as.matrix(grid)

  rmse_cv <- if(is.null(x)){

    foreach::foreach(i = 1:nrow(grid), .combine = rbind, .multicombine = T) %:%

      foreach::foreach(train = TD$train, test = TD$test,
                       .combine = c, .multicombine = T) %dopar% {

        tryCatch(
          sqrt(mean(
              (predict(
                  arima(x = y[train],
                        order = grid[i,1:3],
                        seasonal = list(order = grid[i,4:6], period = period),
                        include.mean = include.mean,
                        transform.pars = transform.pars,
                        fixed = fixed,
                        init = init,
                        method = method),
                  horizon)$pred - y[test])^2)),
          error = function(c) NA)

      }

  } else {

    foreach::foreach(i = 1:nrow(grid), .combine = rbind, .multicombine = T) %:%

      foreach::foreach(train = TD$train, test = TD$test,
                       .combine = c, .multicombine = T) %dopar% {

       xreg <- colnames(x)[which(grid[i,-c(1:6)]==1)]

       tryCatch(
         sqrt(mean(
           (predict(
             arima(x = y[train],
                   order = grid[i,1:3],
                   seasonal = list(order = grid[i,4:6], period = period),
                   xreg = x[train, xreg],
                   include.mean = include.mean,
                   transform.pars = transform.pars,
                   fixed = fixed,
                   init = init,
                   method = method),
             horizon,
             newxreg = x[test, xreg])$pred - y[test])^2)),
         error = function(c) NA)

      }

  }

  grid <- cbind(grid, rmse_cv = rowMeans(rmse_cv, na.rm = T))

  best_index <- which.min(grid[,'rmse_cv'])
  best_model_params <- grid[best_index,]

  best_model <- if(is.null(x)) {

    arima(x = y,
          order = best_model_params[1:3],
          seasonal = list(order = best_model_params[4:6], period = period),
          include.mean = include.mean,
          transform.pars = transform.pars,
          fixed = fixed,
          init = init,
          method = method)

  } else {

    best_xreg <- colnames(x)[which(best_model_params[-c(1:6, ncol(grid))]==1)]
    if(length(best_xreg)==0){
      xreg <- NULL
    }else{
      xreg <- matrix(x[, best_xreg], dimnames = list(NULL, best_xreg))
      }

    arima(x = y,
          order = best_model_params[1:3],
          seasonal = list(order = best_model_params[4:6], period = period),
          xreg = xreg,
          include.mean = include.mean,
          transform.pars = transform.pars,
          fixed = fixed,
          init = init,
          method = method)

  }

  list(best_model = best_model,
       best_model_params = best_model_params,
       best_index = best_index,
       grid = grid,
       inputs = list(y = y,
                     x = x,
                     p = p, d = d, q = q,
                     P = P, D = D, Q = Q,
                     period = period,
                     max_parameters = max_parameters,
                     initialWindow = initialWindow,
                     horizon = horizon,
                     fixedWindow = fixedWindow,
                     skip = skip,
                     include.mean = include.mean,
                     transform.pars = transform.pars,
                     fixed = fixed,
                     init = init,
                     method = method
                     ))

}

