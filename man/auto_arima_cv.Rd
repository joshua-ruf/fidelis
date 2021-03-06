% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_arima_cv.R
\name{auto_arima_cv}
\alias{auto_arima_cv}
\title{Cross Validation Auto ARIMA}
\usage{
auto_arima_cv(y, x = NULL, p = 2, d = 1, q = 2, P = 1, D = 1,
  Q = 1, period = NA, max_parameters = NULL,
  initialWindow = as.integer(0.7 * length(y)), horizon = 1,
  fixedWindow = T, skip = 0, include.mean = TRUE,
  transform.pars = TRUE, fixed = NULL, init = NULL,
  method = c("CSS-ML", "ML", "CSS"))
}
\arguments{
\item{y}{A numeric vector}

\item{x}{A numeric matrix of dimension n,k where n is the length of \code{y} and k is the number of potential regressors}

\item{p}{Max AR order}

\item{d}{Max degree of differencing}

\item{q}{Max MA order}

\item{P}{Max seasonal AR order, defaults to 0 if period is \code{NA}}

\item{D}{Max seasonal degree of differencing, defaults to 0 if period is \code{NA}}

\item{Q}{Max seasonal MA order, defaults to 0 if period is \code{NA}}

\item{period}{Frequency of the time series}

\item{max_parameters}{Max sum of AR, MA, seasonal AR, seasonal MA, and k}

\item{initialWindow}{The initial number of consecutive values in each training set sample, defaults to 70\% of length(y)}

\item{horizon}{The number of consecutive values in test set sample}

\item{fixedWindow}{Logical, if \code{FALSE}, all training samples start at 1}

\item{skip}{Integer, how many (if any) resamples to skip to thin the total amount}

\item{include.mean}{Should the ARMA model include a mean/intercept term?
The default is \code{TRUE} for undifferenced series, and it is ignored for ARIMA models with differencing.}

\item{transform.pars}{Logical; if true, the AR parameters are transformed to ensure that they remain in the region of stationarity.
Not used for \code{method = "CSS"}. For \code{method = "ML"}, it has been advantageous to set \code{transform.pars = FALSE}
in some cases, see also \code{fixed}.}

\item{fixed}{Optional numeric vector of the same length as the total number of parameters. If supplied, only \code{NA} entries
in \code{fixed} will be varied. \code{transform.pars = TRUE} will be overridden (with a warning) if any AR parameters are
fixed. It may be wise to set \code{transform.pars = FALSE} when fixing MA parameters, especially near non-invertibility.}

\item{method}{Fitting method: maximum likelihood or minimize conditional sum-of-squares. The default (unless there are missing
values) is to use conditional-sum-of-squares to find starting values, then maximum likelihood. Can be abbreviated.}

\item{Optional}{numeric vector of initial parameter values. Missing values will be filled in, by zeroes except for regression
coefficients. Values already specified in \code{fixed} will be ignored.}
}
\value{
A list containing the following:
     \itemize{
          \item\code{best_model} The ARIMA model with the lowest RMSE
          \item\code{best_model_params} The best ARIMA model parameters, and RMSE
          \item\code{best_index} THe row number of the best ARIMA model in the grid
          \item\code{grid} The grid of all possible ARIMA models, and corresponding average RMSE found via CV
          \item\code{inputs} A list of inputs provided to \code{auto_arima_cv()}
          }
}
\description{
This function finds the ARIMA model that minimizes average RMSE via grid search and time-series cross validation.
     All cominations of (p, d, q) and (P, D, Q) terms are considered, as well as the inclusion of exogenous regressors.
}
\details{
\enumerate{
        \item If a parallel backend is registered, this function runs in parallel via \%dopar\%.
        \item If a model does not converge it does not appear in the average RMSE calculation.
        }
}
\examples{
D <- ts(matrix(rnorm(150), ncol = 3))
y <- D[-1,1]
x <- D[-nrow(D),-1]
# x variables are lagged by 1, with NAs removed

doParallel::registerDoParallel() # setup parallel backend

model <- auto_arima_cv(y = y)

foreach::registerDoSEQ() # return to sequential processing

}
\seealso{
\code{\link{arima}}, \code{\link{auto.arima}}
}
