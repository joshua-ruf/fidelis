% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummy.r
\name{dummy}
\alias{dummy}
\title{Create Binary Variables}
\usage{
dummy(D, var, rm.var = T)
}
\arguments{
\item{D}{A data.table or data.frame}

\item{var}{A character string of the desired variable to dummy}

\item{rm.var}{Logical, defaults to TRUE where the original variable will be removed}
}
\description{
This function creates k-1 binary variables, where k is the number of unique values of \code{var}.
    It prints to the console which value will be used as the reference, and therefore won't have a binary variable.
}
\note{
In keeping with data.table's methodology this function does not need to be assigned to a new object
}
