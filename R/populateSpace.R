setGeneric("populateSpace", function(object,...) standardGeneric("populateSpace"))

#'Populate Space with Transmitters and Receivers
#'
#'Populates a grid that is \code{spaceResXspaceRes} with transmitters and receivers.  Transmitters are represented by a \code{1} and
#'receivers by a \code{-1}.
#'
#' @param spaceRes  The resolution of the space grid. The grid will be symmetric with \code{spaceRes} columns and \code{spaceRes} rows.
#' If provided, the returned \code{moodel} will have it's \code{@@spaceRes} parameter replaced with the user provided parameter.
#'
#' @name populateSpace
#' @details
#' Currently uses sampling without replacement, may want to consider allowing replacement and handling collisions appropriately.
#' @include class-trModel.R
#' @export



setMethod("populateSpace",signature(object="trModel"),function(object, spaceRes=NULL)
{
  if(!is.null(spaceRes))
    object@spaceRes = spaceRes
  else
    spaceRes = object@spaceRes

  matx = matrix(0,nrow=spaceRes,ncol=spaceRes)

  #sample without replacement
  x=sample(spaceRes,size=(object@ntransmitter+object@nreceiver))
  y=sample(spaceRes,size=(object@ntransmitter+object@nreceiver))
  pts=cbind(x,y,type=c(rep(1,object@ntransmitter),rep(-1,object@nreceiver)))
  object@points=pts

  #populate receivers
  for(i in 1:object@nreceiver)
  {
    matx[pts[i,1],pts[i,2]] = -1
  }

  #populate transmitters
  for(i in object@nreceiver:nrow(pts))
  {
    matx[pts[i,1],pts[i,2]] = 1
  }
  object@space = matx
  invisible(object)
})
