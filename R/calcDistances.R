setGeneric("calcDistances", function(object,...) standardGeneric("calcDistances"))

#'Calculate Distance and Weight Matrix
#'
#'Calculates a matrix of distance values where each row represents a receiver and each column represents a transmitter. Then
#'applies the diffusion/weight function to the distances to produce the weight matrix. Returns the input \code{\linkS4class{trModel}} object
#'with the \code{@@distance} and \code{@@weight} matrixes updated.
#'
#' @param object A \code{\linkS4class{trModel}} object
#'
#' @return  The input \code{\linkS4class{trModel}} object where the \code{@@distance} slot has been replaced by the
#'calculated distance matrix.
#'
#' @name calcDistances
#' @include class-trModel.R
#' @export

setMethod("calcDistances", signature(object="trModel"), function(object, diffusionFn=NULL)
{
  points=object@points
  ridx = which(points[,'type']==-1)
  tidx = which(points[,'type']==1)

  #create a receiver X transmitter matrix
  object@distance[ ] = vapply(tidx,FUN.VALUE = numeric(length(ridx)),FUN = function(a) vapply(ridx,FUN.VALUE=numeric(1),FUN=function(x,y) {
    sqrt((points[x,1]-points[y,1])^2+(points[x,2]-points[y,2])^2) },y=a))

  # object@distance = dist

  #apply weight function
  if(!is.null(diffusionFn))
  {
    cat("\nUsing provided diffusion function.  Replacing diffusion function in provided trModel\n")
    object@diffusionFn = diffusionFn
  }
  # weight = matrix(0,nrow(dist),ncol(dist)) #hack to preserve matrix shape
  object@weight[ ] = vapply(object@distance,FUN.VALUE = numeric(1),FUN=object@diffusionFn)

  # object@weight = weight

  invisible(object)
})
