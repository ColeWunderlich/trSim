setGeneric("calcDistanceMatrix", function(object) standardGeneric("calcDistanceMatrix"))

#'Calculate Distance Matrix
#'
#'Produces a matrix of distance values where each row represents a receiver and each column represents a transmitter.
#'
#' @param object A \code{\linkS4class{trModel}} object
#'
#' @return  The input \code{\linkS4class{trModel}} object where the \code{@@distance} slot has been replaced by the
#'calculated distance matrix.
#'
#' @name calcDistanceMatrix
#' @include class-trModel.R
#' @export

setMethod("calcDistanceMatrix", signature(object="trModel"), function(object)
{
  points=object@points
  ridx = which(points[,'type']==-1)
  tidx = which(points[,'type']==1)

  dist = vapply(ridx,FUN.VALUE = numeric(length(tidx)),FUN = function(a) vapply(tidx,FUN.VALUE=numeric(1),FUN=function(x,y) {
    sqrt((points[x,1]-points[y,1])^2+(points[x,2]-points[y,2])^2) },y=a))

  object@distance = dist
  invisible(object)
})
