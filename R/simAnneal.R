setGeneric("simAnneal",function(object,tstart,tstop,...) standardGeneric("simAnneal"))
#' Simulated Annealing
#'
#'@param object A \code{\linkS4class{trModel}} object
#'@param tstart The starting temperature
#'@param tstop The final temperature, once the temperature drops below this value the process will stop
#'@param alpha The multiplicitave temperature decrement
#'@param n The number of iterations per temperature before moving to the next temperature
#'
#' @export

setMethod("simAnneal", signature(object="trModel",tstart='numeric',tstop="numeric"), function(object,tstart,tstop,alpha,n)
{

  #initialize
  weight = object@weight
  counts = object@count
  wrow = nrow(weight)
  wcol = ncol(weight)
  #identity indicies
  rrow = 1:wrow
  tcol = 1:wcol

  #permute rows and cols of weight matrix to obtain random starting position
  rrow = sample(rrow)
  tcol = sample(tcol)
  w = weight[rrow,tcol]
  prob1 = sum(counts*log(w))

  #cheat
  endProb = sum(counts*log(weight))

  t = tstart
  oldProb = 100 #dummy value
  count=0
  #note: as E decreases we move from - to 0 in log(p) space => maximize log(p)
  #since all probabilities are negative I removed the (-) from the classical e^-(dE/t) = P
  while(t>=tstop)
  {
    #cheat
    if(prob1==endProb)
    {
      warning(paste0("\nSolution reached early!\tt=",t,"\n"))
      break()
    }
     cat("\nOldProb:\t",oldProb,"\tProb1:\t",prob1,"\n")
    if(prob1==oldProb)
    {
      count = count+1
      if(count>100)
      {
        warning(paste0("\nSolution stability reached at t=",t,". Terminating search.\n"))
        break()
      }
    }else {count=0}

    oldProb = prob1

    #choose to permute either cols(2) or rows (1)
    for(i in 1:n)
    {
      rand = sample(1:2,1)
      if(rand==1)
      {
        #swap a pair of rows
        swap = sample(1:wrow,2)
        prob2 = prob1 - sum(counts[swap,]*log(w[swap,])) + sum(counts[swap, ] * log(w[c(swap[2],swap[1]), ]))
        if(prob2>prob1)
        {
          #improvement so accept
          rrow[swap] = rrow[c(swap[2],swap[1])]
          w[swap, ] = w[c(swap[2],swap[1]), ]
          prob1=prob2
        }else
        {
          p = exp((prob2-prob1)/t)
          randp = runif(1)
          if(p>randp)
          {
            rrow[swap] = rrow[c(swap[2],swap[1])]
            w[swap, ] = w[c(swap[2],swap[1]), ]
            prob1=prob2
          }
        }
      }else
      {
        #swap a pair of cols
        swap = sample(1:wcol,2)
        prob2 = prob1 - sum(counts[ ,swap]*log(w[ ,swap])) + sum(counts[ ,swap] * log(w[ ,c(swap[2],swap[1])]))
        if(prob2>prob1)
        {
          #improvement so accept
          tcol[swap] = tcol[c(swap[2],swap[1])]
          w[ ,swap] = w[ ,c(swap[2],swap[1])]
          prob1=prob2
        }else
        {
          p = exp((prob2-prob1)/t)
          randp = runif(1)
          if(p>randp)
          {
            tcol[swap] = tcol[c(swap[2],swap[1])]
            w[ ,swap] = w[ ,c(swap[2],swap[1])]
            prob1=prob2
          }
        }
      }
    }
    t = t*alpha
  }

  return(list(prob=prob1,rid=rrow,tid=tcol,temp=t,weights=w))
})

