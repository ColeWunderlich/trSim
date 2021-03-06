% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcDistances.R
\docType{methods}
\name{calcDistances}
\alias{calcDistances}
\title{Calculate Distance and Weight Matrix}
\usage{
\S4method{calcDistances}{trModel}(object, diffusionFn = NULL)
}
\arguments{
\item{object}{A \code{\linkS4class{trModel}} object}
}
\value{
The input \code{\linkS4class{trModel}} object where the \code{@distance} slot has been replaced by the
calculated distance matrix.
}
\description{
Calculates a matrix of distance values where each row represents a receiver and each column represents a transmitter. Then
applies the diffusion/weight function to the distances to produce the weight matrix. Returns the input \code{\linkS4class{trModel}} object
with the \code{@distance} and \code{@weight} matrixes updated.
}
