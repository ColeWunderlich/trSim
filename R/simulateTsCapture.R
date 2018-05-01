setGeneric("simulateTsCapture",function(object,ncaptures) standardGeneric("simulateTsCapture"))

#'Simulate the Capture of Transmitter Signals by Receiver Beads
#'
#'
#' @name simulateTsCapture
#'
#' @details
#' Currently models the probability that receiver x captures a signal for a given number of sucessfull capture events.
#' Does not model varying capture rates among the various transmitters and/or receivers. Also, does not simulate reads.
#'
#' @include class-trModel.R
#' @export

setMethod("simulateTsCapture", signature(object="trModel",ncaptures="numeric"), function(object,ncaptures)
{
  object@count[ ] = rmultinom(1,ncaptures,object@weight)
  # object@count = matrix(rmultinom(1,ncaptures,object@weight),nrow=trm@nreceiver,ncol=object@ntransmitter)
  invisible(object)
})
