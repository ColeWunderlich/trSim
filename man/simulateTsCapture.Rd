% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateTsCapture.R
\docType{methods}
\name{simulateTsCapture}
\alias{simulateTsCapture}
\title{Simulate the Capture of Transmitter Signals by Receiver Beads}
\usage{
\S4method{simulateTsCapture}{trModel,numeric}(object, ncaptures)
}
\description{
Simulate the Capture of Transmitter Signals by Receiver Beads
}
\details{
Currently models the probability that receiver x captures a signal for a given number of sucessfull capture events.
Does not model varying capture rates among the various transmitters and/or receivers. Also, does not simulate reads.
}
