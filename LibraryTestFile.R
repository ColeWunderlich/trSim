library(trSim)
# trm = trModel(15,5,100,function(x) {dnorm(x)*dnorm(0)}) #using stdev =1, same as dmvnorm(c(0,x))
trm = trModel(50,50,100,function(x) {dnorm(x,sd=20)*dnorm(0,sd=5)})
trm = populateSpace(trm)
plotSpace(trm)
trm=calcDistances(trm)
trm=simulateTsCapture(trm,10^9)
trm@count

res=simAnneal(trm, tstart=10000, tstop=100, alpha=.95, n=1000)
res[1:7]
# simAnneal(trm, tstart=1000, tstop=.1, alpha=.90, n=1000)
