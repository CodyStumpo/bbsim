#outcomes
# 1 = sngl, 2 = dbl, 3 = trpl, 4 = HR, 5 = BB, 6 = HBP, 7 = K, 8 = GBout, 9 = FBout

# functions that for each outcome, transition each sim to next state of inning, runs, runers, outs
singles = function(state,outcomes, batterCharacteristics){
  #make single a function of state that returns a data.frame. then can do state = single(state)
  x=which(outcomes == 1)
  
  ## runners on third score
  scoreFromThird=x[which(state$third[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[x]=0
  
  ## runners on second move to third or score with runner-specific percentage
  onSecond=x[which(state$second[x]>0)]
  rnd=runif(length(onSecond),0,1)
  scoreFromSecond = onSecond[rnd < batterCharacteristics[state$second[onSecond],"takeXB"]]
  state$runs[scoreFromSecond]=state$runs[scoreFromSecond]+1
  
  secondToThird=setdiff(onSecond,scoreFromSecond)
  state$third[secondToThird]=state$second[secondToThird]
  state$second[onSecond]=0
  
  ## runners on first move to second or third with runner-specific percentage 
  onFirst=x[which(state$first[x]>0)]
  rnd2=runif(length(onFirst),0,1) #more realistic to correlate rnd2 to rnd for ball in gap, weak OF arm, etc..?
  ### need to disallow moving to third if runner is already there (that just advanced from second and stopped)!
  thirdFromFirst = setdiff(onFirst[rnd2 < batterCharacteristics[state$first[onFirst],"takeXB"]],secondToThird)
  state$third[thirdFromFirst]=state$first[thirdFromFirst]
  
  firstToSecond=setdiff(onFirst,thirdFromFirst)
  state$second[firstToSecond]=state$first[firstToSecond]
  state$first[onFirst]=0
  
  #batter reaches first
  state$first[x]=i
  return(state)
}
doubles = function(state,outcomes, batterCharacteristics){
  #make double a function of state that returns a data.frame. then can do state = double(state)
  x=which(outcomes == 2)
  
  ## runners on third score
  scoreFromThird=x[which(state$third[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[x]=0
  
  ## runners on second score 
  scoreFromSecond=x[which(state$second[x]>0)]
  state$runs[scoreFromSecond]=state$runs[scoreFromSecond]+1
  state$second[x]=0
  
  ## runners on first move to third or score with runner-specific percentage
  onFirst=x[which(state$first[x]>0)]
  scoreFromFirst = onFirst[runif(length(onFirst),0,1) < batterCharacteristics[state$first[onFirst],"takeXB"]]
  state$runs[scoreFromFirst]=state$runs[scoreFromFirst]+1
  
  firstToThird=setdiff(onFirst,scoreFromFirst)
  state$third[firstToThird]=state$first[firstToThird]
  state$first[onFirst]=0
  
  #batter reaches second
  state$second[x]=i
  return(state)
}
triples = function(state,outcomes, batterCharacteristics){
  #make triple a function of state that returns a data.frame. then can do state = triple(state)
  x=which(outcomes == 3)
  
  ## runners on third score
  scoreFromThird=x[which(state$third[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[x]=0
  
  ## runners on second score 
  scoreFromSecond=x[which(state$second[x]>0)]
  state$runs[scoreFromSecond]=state$runs[scoreFromSecond]+1
  state$second[x]=0
  
  ## runners on first score
  scoreFromFirst=x[which(state$first[x]>0)]
  state$runs[scoreFromFirst]=state$runs[scoreFromFirst]+1
  state$first[x]=0
  
  #batter reaches third
  state$third[x]=i
  return(state)
}
homers = function(state,outcomes, batterCharacteristics){
  #make homer a function of state that returns a data.frame. then can do state = homer(state)
  x=which(outcomes == 4)
  
  ## runners on third score
  scoreFromThird=x[which(state$third[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[x]=0
  
  ## runners on second score 
  scoreFromSecond=x[which(state$second[x]>0)]
  state$runs[scoreFromSecond]=state$runs[scoreFromSecond]+1
  state$second[x]=0
  
  ## runners on first score
  scoreFromFirst=x[which(state$first[x]>0)]
  state$runs[scoreFromFirst]=state$runs[scoreFromFirst]+1
  state$first[x]=0
  
  #batter scores
  state$runs[x]=state$runs[x]+1
  return(state)
}
bbHbp = function(state,outcomes, batterCharacteristics){
  #make bbHbp a function of state that returns a data.frame. then can do state = bbHbp(state)
  x=which(outcomes %in% c(5,6))
  
  ## runners on third score if bases loaded, otherwise stay put
  scoreFromThird=x[which(state$third[x]>0 & state$second[x]>0 & state$first[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[scoreFromThird]=0
  
  ## runners on second move to third if man on first too, otherwise stay put
  secondToThird=x[which(state$second[x]>0 & state$first[x]>0)]
  state$third[secondToThird]=state$second[secondToThird]
  state$second[secondToThird]=0
  
  ## runners on first move to second 
  firstToSecond=x[which(state$first[x]>0)]
  state$second[firstToSecond]=state$first[firstToSecond]
  state$first[firstToSecond]=0
  
  #batter reaches first
  state$first[x]=i
  return(state)
}
k = function(state,outcomes, batterCharacteristics){
  #make k a function of state that returns a data.frame. then can do state = k(state)
  x=which(outcomes == 7)
  
  ## one more out
  state$outs[x]=(state$outs[x]+1) %% 3
  
  ## move to next inning if that was third out
  newInning=x[which(state$outs[x]==0)] 
  state$inning[newInning]=state$inning[newInning]+1
  state$first[newInning]=0
  state$second[newInning]=0
  state$third[newInning]=0
  return(state)
}
gbout = function(state,outcomes, batterCharacteristics){
  #make gbout a function of state that returns a data.frame. then can do state = gbout(state)
  x=which(outcomes == 8)
  ## one more out
  state$outs[x]=(state$outs[x]+1) %% 3
  
  #TODO: deal with baserunners
  
  ## move to next inning if that was third out
  newInning=x[which(state$outs[x]==0)] 
  state$inning[newInning]=state$inning[newInning]+1
  state$first[newInning]=0
  state$second[newInning]=0
  state$third[newInning]=0
  
  return(state)
} #TODO: baserunners
fbout = function(state,outcomes, batterCharacteristics){
  #make fbout a function of state that returns a data.frame. then can do state = fbout(state)
  x=which(outcomes == 9)
  ## one more out
  state$outs[x]=(state$outs[x]+1) %% 3
  
  #TODO: deal with baserunners
  
  ## move to next inning if that was third out
  newInning=x[which(state$outs[x]==0)] 
  state$inning[newInning]=state$inning[newInning]+1
  state$first[newInning]=0
  state$second[newInning]=0
  state$third[newInning]=0
  
  return(state)
} #TODO: baserunners

#Sometimes before a plate appearance can lead to a batter outcome, there is a runner event
#balk, wild pitch, passed ball, everyone goes up a base (can add complexity to WP/PB later for getting thrown out)
runners90 = function(state){
  manOn = which (state$first + state$second + state$third > 0)
  whichOutcomes = manOn[which((runif(length(manOn),0,1) < bkWpPb) == TRUE)] #can make pitcher/catcher (&lead runner/batter) specific later
  x = whichOutcomes
  ## runners on third score
  scoreFromThird=x[which(state$third[x]>0)]
  state$runs[scoreFromThird]=state$runs[scoreFromThird]+1
  state$third[x]=0
  
  ## runners on second move to third 
  secondToThird=x[which(state$second[x]>0)]
  state$third[secondToThird]=state$second[secondToThird]
  state$second[secondToThird]=0
  
  ## runners on first move to second 
  firstToSecond=x[which(state$first[x]>0)]
  state$second[firstToSecond]=state$first[firstToSecond]
  state$first[firstToSecond]=0
  
  return(state)
}

#for each sim iteration, get batter i's outcomes
getBatterOutcomes = function(i){
  batter=batterOutcomeRanges[i,]
  sims = runif(simlength)
  findInterval(sims, batter)+1
}

#for each sim iteration, see what effect batter has on state
runBatterOutcomes=function(i, outcomes, batterCharacteristics){
  #to avoid wasting time (check if actually saves time) only run if length(whichOutcomes) > 0
  state=runners90(state) #this has to be run before the other outcomes
  
  #TODO SB/CS - also before other outcomes although random if before or after balk/wildpitch/passedBall
  
  #for those outcomes where next batter hits a single, double, etc... play it out
  state=singles(state,outcomes, batterCharacteristics)
  state=doubles(state,outcomes, batterCharacteristics)
  state=triples(state,outcomes, batterCharacteristics)
  state=homers(state,outcomes, batterCharacteristics)
  state=bbHbp(state,outcomes, batterCharacteristics)
  state=k(state,outcomes, batterCharacteristics)
  state=gbout(state,outcomes, batterCharacteristics) 
  state=fbout(state,outcomes, batterCharacteristics)
  return(state)
}


#Now simulate given order 1-9. then repeat for each permutation of lineup
#should set order of batters 1-9 as permutation of batters (identity permutation to start, then others)
simlength=1000 #can do about 25k / second on macbook pro
init0=rep(0,simlength)
#starting state
state=data.frame(inning=init0+1,
                 runs=init0,
                 first=init0,second=init0,third=init0,
                 outs=init0)

i=1
final=(state-state)[1,]
final=final[-1,]

while(simlength > 0){

outcomes = getBatterOutcomes(i)
state=runBatterOutcomes(i, outcomes, batterCharacteristics)
i=(i %% 9) + 1

continue=which(state$inning < 10)
gameOver=which(state$inning == 10)
simlength=length(continue)
if (length(gameOver) > 0) final=rbind(final, state[gameOver,]) 
if (length(continue) > 0) state=state[continue,]
}

#hist(final$runs, breaks = seq(from=-1, to=max(final$runs)), freq=FALSE)


#Now, try different batting orders
library(e1071)
orders=permutations(9) #362,880 possible orders - will take 30 minutes to go through brute force at 100 sims