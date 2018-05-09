setGeneric("simAnneal",function(object,tstart,tstop,...) standardGeneric("simAnneal"))
#' Simulated Annealing
#'
#'@param object A \code{\linkS4class{trModel}} object
#'@param tstart The starting temperature
#'@param tstop The final temperature, once the temperature drops below this value the process will stop
#'@param alpha The multiplicitave temperature decrement
#'@param n The number of iterations per temperature before moving to the next temperature
#'@param stability The number of temperature increments with the same end probability that causes early termination
#'
#' @export

setMethod("simAnneal", signature(object="trModel",tstart='numeric',tstop="numeric"), function(object,tstart,tstop,alpha,n,stability,endPlot=T,diagnostics=F)
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
  tcol = c(1,sample(2:wcol))
  w = weight[rrow,tcol]
  prob1 = sum(counts*log(w))

  #cheat
  targetProb = sum(counts*log(weight))

  t = tstart
  oldProb = 100 #dummy value
  count=0
  best = -Inf
  oldBest = -Inf

  # plt = ggplot(data.frame(x=0,y=targetProb),aes(x=x,y=y))+geom_point()+geom_hline(yintercept=targetProb,color='red')+ylim(-10^9,targetProb+10^3)+xlim(0,100)
  # show(plt)

  if(endPlot)
    pts = NULL
  #note: as E decreases we move from - to 0 in log(p) space => maximize log(p)
  #since all probabilities are negative I removed the (-) from the classical e^-(dE/t) = P
  while(t>=tstop)
  {
    #cheat
    # if(targetProb*1.1>= prob1 && prob1 >=targetProb*.90)
    if(prob1==targetProb)
    {
      warning(paste0("\nSolution reached early!\tt=",t,"\n"))
      break()
    }

    # if(best==oldBest)
    # {
    #   count = count+1
    #   if(count>stability)
    #   {
    #     warning(paste0("\nSolution stability reached at t=",t,". Terminating search.\n"))
    #     break()
    #   }
    # }else {count=0}

    if(endPlot)
      pts = c(pts,prob1)

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
        swap = sample(2:wcol,2) #CHANGE BACK
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

      if(prob1>best)
      {
        oldBest = best
        best = prob1
        brrow = rrow
        btcol = tcol
      }
    }
    t = t*alpha


    if(best==oldBest)
    {
      count = count+1
      if(count>stability)
      {
        warning(paste0("\nSolution stability reached at t=",t,". Terminating search.\n"))
        break()
      }
    }else {count=0}

    if(diagnostics)
      cat(sprintf("\nBest:\t%.5f\toldBest:\t%.5f\tCount:\t%d\n",best,oldBest,count))

    oldBest = best
  }
  if(endPlot)
    print(qplot(1:length(pts),pts)+geom_hline(yintercept = targetProb,color='red'))
  return(list(targetProb=targetProb,best=best,lastProb=prob1,BestVsTarg=paste0((best-targetProb)/targetProb,"% TargetProb"),bestrid=brrow,besttid=btcol,rid=rrow,tid=tcol,temp=t,weights=w,points=pts))
})

