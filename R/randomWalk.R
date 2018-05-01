#' Random Walk
#'

randWalkLoop = function(nsteps,nwalkers,sigma)
{
  xloc = matrix(0,nsteps,nwalkers)
  yloc = matrix(0,nsteps,nwalkers)
  dist = matrix(0,nsteps,nwalkers)
  for(w in 1:nwalkers)
  {
    angles = runif(nsteps,0,2*pi)
    stepSize = rnorm(nsteps,0,sd=sigma)
    stepx = cos(angles) * stepSize
    stepy = sin(angles) * stepSize
    xloc[,w] = cumsum(stepx)
    yloc[,w] = cumsum(stepy)
    dist[,w] = sqrt(xloc[,w]^2+yloc[,w]^2)
  }
  return(list(xloc=xloc,yloc=yloc,dist=dist))
}

plots = function(res)
{
  require(ggplot2)
  qplot(res$xloc[,10],res$yloc[,10]) #single walk
  plim = max(res$xloc[nrow(res$xloc),],res$yloc[nrow(res$yloc),])
  qplot(res$xloc[nrow(res$xloc),],res$yloc[nrow(res$yloc),],xlim=c(-plim,plim),ylim=c(-plim,plim)) # where they ended up
  qplot(res$dist[nrow(res$dist),])

}
