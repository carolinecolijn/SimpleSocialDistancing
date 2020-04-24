library(deSolve)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)

# model definition 
# socdistmodel <- function(t,state,pars,sdtiming) {
#   with(as.list(c(state,pars)), { 
#     f = ifelse(sdtiming(t)==1, pars$f, 1) # if social distancing is on, use pars$f. Otherwise, f is 1. 
#     dSdt = -(R0/(D+1/k2))*(I+E2 + f*(Id+E2d))*S/N - r*S + ur*Sd
#     dE1dt = (R0/(D+1/k2))*(I+E2 + f*(Id+E2d))*S/N - k1*E1 -r*E1 + ur*E1d
#     dE2dt = k1*E1 -k2*E2 -r*E2 + ur*E2d
#     dIdt = k2*E2 - q*I -  I/D - r*I+ ur*Id
#     dQdt = q*I - Q/D -r*Q + ur*Qd
#     dRdt = I/D + Q/D -r*R+ur*Rd
#     
#     dSddt = -(f*R0/(D+1/k2))*(I+E2 + f*(Id+E2d))*Sd/N + r*S -ur*Sd
#     dE1ddt = (f*R0/(D+1/k2))*(I+E2 + f*(Id+E2d))*Sd/N - k1*E1d +r*E1 - ur*E1d
#     dE2ddt = k1*E1d - k2*E2d + r*E2 - ur*E2d
#     dIddt = k2*E2d - q*Id-  Id/D + r*I - ur*Id
#     dQddt = q*Id - Qd/D +r*Q - ur*Qd
#     dRddt = Id/D+Qd/D +r*R - ur*Rd
#     # dRdt = I/D; dr + ds + di =0, S+I+R = N --> R = N-S-I and we eliminate R 
#     list(c(dSdt,dE1dt, dE2dt, dIdt, dQdt,dRdt, dSddt,dE1ddt, dE2ddt, dIddt, dQddt,dRddt ))
#   })
# }


socdistmodel <- function(t,state,pars,sdtiming) {
  # with(as.list(c(state,pars)), { 
  f = ifelse(sdtiming(t)==1, pars$f, 1) # if social distancing is on, use pars$f. Otherwise, f is 1. 
  dSdt = -(pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["S"]]/pars[["N"]] - pars[["r"]]*state[["S"]] + pars[["ur"]]*state[["Sd"]]
  dE1dt = (pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["S"]]/pars[["N"]] - pars[["k1"]]*state[["E1"]] -pars[["r"]]*state[["E1"]] + pars[["ur"]]*state[["E1d"]]
  dE2dt = pars[["k1"]]*state[["E1"]] -pars[["k2"]]*state[["E2"]] -pars[["r"]]*state[["E2"]] + pars[["ur"]]*state[["E2d"]]
  dIdt = pars[["k2"]]*state[["E2"]] - pars[["q"]]*state[["I"]] -  state[["I"]]/pars[["D"]] - pars[["r"]]*state[["I"]]+ pars[["ur"]]*state[["Id"]]
  dQdt = pars[["q"]]*state[["I"]] - state[["Q"]]/pars[["D"]] -pars[["r"]]*state[["Q"]] + pars[["ur"]]*state[["Qd"]]
  dRdt = state[["I"]]/pars[["D"]] + state[["Q"]]/pars[["D"]] -pars[["r"]]*state[["R"]]+pars[["ur"]]*state[["Rd"]]
  
  dSddt = -(f*pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["Sd"]]/pars[["N"]] + pars[["r"]]*state[["S"]] -pars[["ur"]]*state[["Sd"]]
  dE1ddt = (f*pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["Sd"]]/pars[["N"]] - pars[["k1"]]*state[["E1d"]] +pars[["r"]]*state[["E1"]] - pars[["ur"]]*state[["E1d"]]
  dE2ddt = pars[["k1"]]*state[["E1d"]] - pars[["k2"]]*state[["E2d"]] + pars[["r"]]*state[["E2"]] - pars[["ur"]]*state[["E2d"]]
  dIddt = pars[["k2"]]*state[["E2d"]] - pars[["q"]]*state[["Id"]]-  state[["Id"]]/pars[["D"]] + pars[["r"]]*state[["I"]] - pars[["ur"]]*state[["Id"]]
  dQddt = pars[["q"]]*state[["Id"]] - state[["Qd"]]/pars[["D"]] +pars[["r"]]*state[["Q"]] - pars[["ur"]]*state[["Qd"]]
  dRddt = state[["Id"]]/pars[["D"]]+state[["Qd"]]/pars[["D"]] +pars[["r"]]*state[["R"]] - pars[["ur"]]*state[["Rd"]]
  # dRdt = I/pars["D"]]; dr + ds + di =0, S+I+R = pars["N"]] --> R = pars["N"]]-S-I and we eliminate R 
  list(c(dSdt,dE1dt, dE2dt, dIdt, dQdt,dRdt, dSddt,dE1ddt, dE2ddt, dIddt, dQddt,dRddt ))
  # })
}

# simulate the model 50 times; return a concatenated data frame 
multisolve=function(params,timing,state,times,nReps) {
  # get random samples of R0
  
  rs=rnorm(nReps, mean = params$R0, sd=0.2)
  
  biglist=lapply(rs, function(x) {
    thispars=params; thispars$R0=x;
    return( as.data.frame(ode(y= state, times=times,  func=socdistmodel, parms=thispars,sdtiming=timing)))})
  
  names(biglist)=rs;
  return(bind_rows(biglist, .id="R0"))
}

socdist2 <- function(t,state,pars,sdprofile) {
  # with(as.list(c(state,pars)), { 
  #f = ifelse(sdtiming(t)==1, pars$f, 1) # if social distancing is on, use pars$f. Otherwise, f is 1. 
  f=sdprofile(t)
  dSdt = -(pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["S"]]/pars[["N"]] - pars[["r"]]*state[["S"]] + pars[["ur"]]*state[["Sd"]]
  dE1dt = (pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["S"]]/pars[["N"]] - pars[["k1"]]*state[["E1"]] -pars[["r"]]*state[["E1"]] + pars[["ur"]]*state[["E1d"]]
  dE2dt = pars[["k1"]]*state[["E1"]] -pars[["k2"]]*state[["E2"]] -pars[["r"]]*state[["E2"]] + pars[["ur"]]*state[["E2d"]]
  dIdt = pars[["k2"]]*state[["E2"]] - pars[["q"]]*state[["I"]] -  state[["I"]]/pars[["D"]] - pars[["r"]]*state[["I"]]+ pars[["ur"]]*state[["Id"]]
  dQdt = pars[["q"]]*state[["I"]] - state[["Q"]]/pars[["D"]] -pars[["r"]]*state[["Q"]] + pars[["ur"]]*state[["Qd"]]
  dRdt = state[["I"]]/pars[["D"]] + state[["Q"]]/pars[["D"]] -pars[["r"]]*state[["R"]]+pars[["ur"]]*state[["Rd"]]
  
  dSddt = -(f*pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["Sd"]]/pars[["N"]] + pars[["r"]]*state[["S"]] -pars[["ur"]]*state[["Sd"]]
  dE1ddt = (f*pars[["R0"]]/(pars[["D"]]+1/pars[["k2"]]))*(state[["I"]]+state[["E2"]] + f*(state[["Id"]]+state[["E2d"]]))*state[["Sd"]]/pars[["N"]] - pars[["k1"]]*state[["E1d"]] +pars[["r"]]*state[["E1"]] - pars[["ur"]]*state[["E1d"]]
  dE2ddt = pars[["k1"]]*state[["E1d"]] - pars[["k2"]]*state[["E2d"]] + pars[["r"]]*state[["E2"]] - pars[["ur"]]*state[["E2d"]]
  dIddt = pars[["k2"]]*state[["E2d"]] - pars[["q"]]*state[["Id"]]-  state[["Id"]]/pars[["D"]] + pars[["r"]]*state[["I"]] - pars[["ur"]]*state[["Id"]]
  dQddt = pars[["q"]]*state[["Id"]] - state[["Qd"]]/pars[["D"]] +pars[["r"]]*state[["Q"]] - pars[["ur"]]*state[["Qd"]]
  dRddt = state[["Id"]]/pars[["D"]]+state[["Qd"]]/pars[["D"]] +pars[["r"]]*state[["R"]] - pars[["ur"]]*state[["Rd"]]
  # dRdt = I/pars["D"]]; dr + ds + di =0, S+I+R = pars["N"]] --> R = pars["N"]]-S-I and we eliminate R 
  list(c(dSdt,dE1dt, dE2dt, dIdt, dQdt,dRdt, dSddt,dE1ddt, dE2ddt, dIddt, dQddt,dRddt ))
  # })
}

multisolve2=function(params,timing,state,times,nReps) {
  # get random samples of R0
  
  rs=rnorm(nReps, mean = params$R0, sd=0.2)
  biglist=lapply(rs, function(x) { 
    thispars=params; thispars$R0=x; 
    return( as.data.frame(ode(y= state, times=times,  func=socdist2, parms=thispars,sdprofile=timing)))})
  names(biglist)=rs;
  return(bind_rows(biglist, .id="R0"))
}

# collect the number of SYMPTOMATIC cases by day with lower 25, median and upper 75 quantile- this shows how numbers vary with R0
getCasesbyDay2 = function(df, times, nReps,startDate=dmy("12-03-2020")) {
  CaseInfo=t(vapply(times, function(x) {
    wherenow = which(df$time==x)
    normals=df$I[wherenow]
    selfisols=df$Id[wherenow]
    return(quantile(normals+selfisols,probs = c(0.1,0.5,0.9)))
  }, FUN.VALUE = c(2,3,4)))
  return(data.frame(times=times,dates=startDate+times, lower25=CaseInfo[,1], median=CaseInfo[,2], upper75=CaseInfo[,3]))
}


# collect the total infectious cases, symptomatic and pre-symptomatic
getAllCasesbyDay2= function(df, times, nReps,startDate=dmy("12-03-2020")) {
  CaseInfo=t(vapply(times, function(x) {
    wherenow = which(df$time==x)
    normals=(df$I+df$E1+df$E2)[wherenow]
    selfisols=(df$Id+df$E1d+df$E2d)[wherenow]
    return(quantile(normals+selfisols,probs = c(0.1,0.5,0.9)))
  }, FUN.VALUE = c(2,3,4)))
  return(data.frame(times=times,dates=startDate+times, lower25=CaseInfo[,1], median=CaseInfo[,2], upper75=CaseInfo[,3]))
}

# get the numbers who have ever been infected, along the simulation 
getEverInfbyDay= function(df, times, nReps,startDate=dmy("12-03-2020")) {
  myN=rowSums(df[1,3:14])
  CaseInfo=t(vapply(times, function(x) {
    wherenow = which(df$time==x)
    normals=df$S[wherenow]
    selfisols=df$Sd[wherenow]
    return(quantile(myN-(normals+selfisols),probs = c(0.1,0.5,0.9)))
  }, FUN.VALUE = c(2,3,4)))
  return(data.frame(times=times,dates=startDate+times, lower25=CaseInfo[,1], median=CaseInfo[,2], upper75=CaseInfo[,3]))
}

# pull out information about the timing of the peak, the size of the peak (max # of symptomatic cases at any one time), R0
getSummaryInfo2 = function(df, times, nReps) {
  peaktimes=vapply(1:nReps, function(x) {
    k=length(times) # because of the stupid reshape
    ii=((x-1)*k + 1):(k*x)
    thisd=df[ii,]
    return(thisd$time[which.max(thisd$I+thisd$Id)])}, FUN.VALUE = 1)
  peaksizes=vapply(1:nReps, function(x) {
    k=length(times)
    ii=((x-1)*k + 1):(k*x)
    thisd=df[ii,]
    return(max(thisd$I+thisd$Id))}, FUN.VALUE = 1)
  R0s=vapply(1:nReps, function(x) {
    k=length(times)
    ii1=((x-1)*k + 1)
    return(as.numeric(df$R0[ii1]))    }, FUN.VALUE = 1)
  return(data.frame(peaktimes=peaktimes, peaksizes=peaksizes,R0s=R0s))
}

# take in 3 solutions in list form, and make  ggplots
makePlots = function(tt2, times, type="symp", shortterm=30,PopScale=TRUE, popSize=N, startDate=dmy("12-03-2020")) {
  if (type == "symp") {
    cbd1 = getCasesbyDay2(tt2[[1]], times,nReps, startDate= startDate)
    cbd2 = getCasesbyDay2(tt2[[2]], times,nReps, startDate= startDate)
    cbd3 = getCasesbyDay2(tt2[[3]], times,nReps,  startDate= startDate) }
  if (type=="all") {
    cbd1 = getAllCasesbyDay2(tt2[[1]], times,nReps, startDate= startDate)
    cbd2 = getAllCasesbyDay2(tt2[[2]], times,nReps, startDate= startDate)
    cbd3 = getAllCasesbyDay2(tt2[[3]], times,nReps, startDate= startDate)
  }
  if (type=="ever") {
    cbd1 = getEverInfbyDay(tt2[[1]], times,nReps, startDate= startDate)
    cbd2 = getEverInfbyDay(tt2[[2]], times,nReps, startDate= startDate)
    cbd3 = getEverInfbyDay(tt2[[3]], times,nReps, startDate= startDate)
  }
  if (PopScale ==TRUE) {
    cbd1[,3:5]=cbd1[,3:5]/popSize
    cbd2[,3:5]=cbd2[,3:5]/popSize
    cbd3[,3:5]=cbd3[,3:5]/popSize
  }
  
  p1=ggplot(data=cbd1) + geom_line(aes(x=dates,y=median))+
    geom_ribbon(aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.5,fill="gray") + 
    geom_line(data=cbd2,aes(x=dates,y=median))+
    geom_ribbon(data=cbd2, aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.3,fill="blue")+
    geom_line(data=cbd3,aes(x=dates,y=median))+
    geom_ribbon(data=cbd3,aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.3,fill="green") + theme(text = element_text(size = 15)) 
  p2=ggplot(data=filter(cbd1,times<shortterm)) + geom_line(aes(x=dates,y=median))+
    geom_ribbon(aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.5,fill="gray") + geom_line(data=filter(cbd2,times<shortterm),aes(x=dates,y=median))+
    geom_ribbon(data=filter(cbd2,times<shortterm), aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.3,fill="blue")+
    geom_line(data=filter(cbd3,times<shortterm),aes(x=dates,y=median))+
    geom_ribbon(data=filter(cbd3,times<shortterm),aes(x=dates,ymin = lower25, ymax = upper75), alpha = 0.3,fill="green") + theme(text = element_text(size = 15)) 
  return(list(p1,p2))
}