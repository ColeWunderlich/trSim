#' class for experiment matrix
#'
#' @exportClass trModel

setClass(Class = "trModel",slots = c(nreceiver="numeric",ntransmitter="numeric", space = "matrix", spaceRes = 'numeric',
                                                           points="matrix",reads="matrix",distance="matrix",weights="matrix",diffusionFn="function"))
#' @export
trModel = function(nreceiver,ntransmitter,spaceRes,diffusionFn=function(x){})
{
  new("trModel",nreceiver=nreceiver,ntransmitter=ntransmitter,spaceRes=spaceRes,diffusionFn=diffusionFn)
}
