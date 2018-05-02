setGeneric("randomEnergy",function(object,...) standardGeneric("randomEnergy"))
#' Calculate a random energy/cost state for simmulated annealing
#'
#'
#' @export

setMethod("randomEnergy", signature(object="trModel"), function(object)
{

  #initialize
  weight = object@weight
  wrow = nrow(weight)
  wcol = ncol(weight)

  #permute rows and cols of weight matrix
  w2 = weight[sample(1:wrow),sample(1:wcol)]

  prob = sum(object@count*log(w2))
  return(prob)
})

