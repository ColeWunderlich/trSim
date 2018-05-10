setGeneric("calcTstart",function(object){standardGeneric("calcTstart")})

#'Calculate Start Temp for simulated annealing
#'
#'The goal of this function is to determine the appropriate starting temp so that the 'average' swap has a \code{targetProb} chance of
#'being accepted.  Obtained by simulating \code{nswaps} swaps and then calculating the temperature using
#'\eqn{\sigma / ln(targetProb) = Tstart}
#'
#'@param object The \code{\linkS4class{trModel}} object
#'@param nswaps How many random energies/swaps should be calculated.
#'@param targetProb  The target probability desired for setting the initial temperature. \code{Tstart} calculated as \eqn{\sigma / ln(targetProb)}
#'@export
#'


calcTstart = function(object,nswaps,targetProb)
{
  enrg = replicate(nswaps,randomEnergy(object))
  tstart = -sd(enrg)/log(targetProb)
}
