setGeneric("simAnneal",function(object,...) standardGeneric("simAnneal"))
#' Simulated Annealing
#'
#'
#' @export

setMethod("simAnneal", signature(object="trModel"), function(object)
{
  points=object@points
  ridx = which(points[,'type']==-1)
  tidx = which(points[,'type']==1)

  #permute positions
  ridx = sample(ridx,replace = F)
  tidx = sample(tidx,replace = F)

  #above not needed??

  w2 = object@weight[sample(1:nrow(object@weight)),sample(1:ncol(object@weight))]

  prob = exp(sum(object@count*log(w2)))
  return(prob)
})

