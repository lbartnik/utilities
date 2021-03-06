% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/container.R
\name{container_sort}
\alias{container_sort}
\alias{sort.container}
\title{Sort container.}
\usage{
container_sort(x, ..., decreasing = FALSE)

\method{sort}{container}(x, decreasing = FALSE, ...)
}
\arguments{
\item{x}{a container object; see \link{as_container}.}

\item{...}{optional; extract these names from each element and use their
values to sort.}

\item{decreasing}{logical; should the sort be decreasing or increasing?}
}
\description{
Sort container.
}
\examples{
c <- as_container(list(list(time = 2), list(time = 1)))
container_sort(c, time)

c <- as_container(as.list(3:1))
sort(c)
}
