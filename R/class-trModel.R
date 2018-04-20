#' Class for modeling a transmitter receiver experiment
#'
#' @import ggplot2
#' @exportClass trModel

##Notes:
# currently diffusionFn is expected to work on a per-element basis, reqire it to work on a per-matrix basis instead??

setClass(Class = "trModel",slots = c(nreceiver="numeric",ntransmitter="numeric", space = "matrix", spaceRes = 'numeric',
                                                           points="matrix",reads="matrix",distance="matrix",weight="matrix",diffusionFn="function"))
#' @export
trModel = function(nreceiver,ntransmitter,spaceRes,diffusionFn=function(x){})
{
  trm=new("trModel",nreceiver=nreceiver,ntransmitter=ntransmitter,spaceRes=spaceRes,diffusionFn=diffusionFn)

  #initialize matricies
  matx = matrix(0,nreceiver,ntransmitter)
  trm@reads = matx
  trm@distance = matx
  trm@weight = matx

  invisible(trm)
}
