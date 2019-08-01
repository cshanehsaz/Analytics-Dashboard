#MAKE ALL THE CUSTOM MIN PAYOUT DISTROS WORK WITH ONTRA CUT

source('scoring_metrics.R')

payout.def <- rep(.1, 10)
stocks.probs.def <- rep(.1, 10)

stocks.stocks <- c('A','B','C','D','E','F','G','H','I','J')
stocks.probs1 <- c(rep(.1, 10))
stocks.probs2 <- c(.15, .15, .125, .125, .1, .1, .075, .075, .05, .05)
#creates a random ordering of stocks
stocks.sample <- function(probVec = stocks.probs1) {
  output <- sample(
    x = stocks.stocks,
    replace = FALSE,
    prob = probVec
  )
  return(output)
}
#changes from letters to numbers based on the real list.
#If user input was B,C,A,D,E and real list was A,B,C,D,E
#then output would be 2,3,1,4,5
stocks.convert <- function(x) {
  temp <- rep(0,length(x))
  for(i in 1:length(x)) {
    temp[i] <- match(stocks.stocks[i], x)
  }
  return(temp)
}

#runs a simulation of num.its people, num.cols columns/companies, with probability
#   distribution for each company given by prob
#returns matrix of 1-10 for each position
#CHANGE TO APPLY RATHER THAN FOR LOOP
sim <- function(num.its = 10, num.cols = 10, prob = stocks.probs.def) {
  sim.raw <- matrix(data = rep(0, num.its*num.cols), nrow = num.its, ncol = num.cols, 
                    dimnames = list(NULL, c('A','B','C','D','E','F','G','H','I','J')))
  for(i in 1:num.its) {
    sim.raw[i,] <- stocks.convert(stocks.sample(prob))
    
  }
  return(sim.raw)
}

#gives a score to each player based on the real distribution "real"
#returns vector with length equal to the number of entrants, one score for each
sim.scores <- function(sim.raw = NULL, score.function = cScoreFinal, 
                       real = c(1:10), general = FALSE, exponent = 2) {
  sim.scores.vec <- rep(0, nrow(sim.raw))
  if(general==TRUE){
    for(i in 1:nrow(sim.raw)) {
      sim.scores.vec[i] <- score.function(x = sim.raw[i,], y = real, exp = exponent)
    }
  }
  else{
    for(i in 1:nrow(sim.raw)) {
      sim.scores.vec[i] <- score.function(x = sim.raw[i,], y = real)
    }
  }
  return(sim.scores.vec)
}
testrun <- sim(num.its = 1000)
sim.scores(sim.raw = testrun, score.function = cScoreFinal)

#allows you to do a full sim, prints out hist, and returns the score vector

#stocks - list of stocks that will be chosen and sampled from
#sim.prob - list of probabilities for chance that each item is chosen next. 
#           e.g. .5, .25, .25 means stock A has 50% chance of being the first chosen
#score - scoring metric used. Default is cScoreReversed (squared)
#payout - payout distribution method, currently by decile
#num.entries - number of people participating in the simulation
#general - whether or not to use generalized cScore
#genExp - if using generalized cScore, what exponent to use
#general switches it so that you can choose the exponent by which to do the
#difference between correct and false
sim.run <- function(stocks = stocks.stocks, sim.prob = stocks.probs.def, 
                    score = cScoreFinal, payout = payout.def,
                    num.entries = 1000, general = FALSE, genExp = 2) {
  sim.data <- sim(num.its = num.entries, prob = sim.prob)
  sim.data.scored <- sim.scores(sim.raw = sim.data, score.function = score, 
                                general = general, exponent = genExp)
  index <- which(sim.data.scored==max(sim.data.scored))
  print(sim.data[index,])
  #hist(sim.data.scored)
  return(sim.data.scored)
}

#sample sim.run usage
# testrun <- sim.run(num.entries = 10000, sim.prob = stocks.probs2,
#                    score = cScoreReversedGeneral, general = TRUE, genExp = 1.5)

# #make the leftover allow each person to get at least 1cent back
# payout.equation.steep <- function(data, buyin = 1, cut = .1, steepness = 1.07) {
#   num <- length(data)
#   payout.structure <- rep(0, num)
#   prize <- buyin * (1-cut) * num
#   for (i in 1:num) {
#     payout.structure[num - i] <- max(prize * (1 / (steepness^i) - 1/ (steepness^(i+1))), .01) 
#   }
#   return(payout.structure)
# }
# # testrunpayout <- payout.equation.steep(testrun)
# # sum(testrunpayout)
# # testrunpayout
# 
# payout.equation.medium <- function(data, buyin = 1, cut = .1){
#   num <- length(data)
#   payout.structure <- rep(1, num+1)
#   basepay <- (num * buyin * (1-cut)) ^ (1/num)
#   for (i in 1:num){
#     #payout.structure[i] <- (basepay^i) - (basepay^(i-1)) LOGIC of following equation
#     payout.structure[i+1] <- max((basepay^i) * (1-(1/basepay)), .01)
#   }
#   return(payout.structure)
# }

#Exponential approximation.
payout.equation.steepApprox <- function(data, buyin = 1, cut = .1, min = 0, deg = 40){
  num <- length(data)
  payout.structure <- rep(0, num)
  for (i in 1:num){
    print((deg+1)*(buyin*(1-cut)-min)/num^deg)
    payout.structure[i] <- (deg+1)*(buyin*(1-cut)-min)/num^deg * i^deg + min
  }
  return(payout.structure)
}

#replaces medium original
#f(pos) = (deg+1)*(b*(1-c)-m)/n^deg * pos^2 + m
payout.equation.medium2 <- function(data, buyin = 1, cut = .1, min = 0, deg = 6){
  num <- length(data)
  payout.structure <- rep(0, num)
  for (i in 1:num){
    print((deg+1)*(buyin*(1-cut)-min)/num^deg)
    payout.structure[i] <- (deg+1)*(buyin*(1-cut)-min)/num^deg * i^deg + min
  }
  return(payout.structure)
}

#MAKE THIS WORK WITH CUT
payout.equation.shallow <- function(data, buyin = 1, cut = .1, min = 0){
  num <- length(data)
  payout.structure <- rep(0, num)
  for (i in 1:num) {
    payout.structure[i] <- 2*(buyin*(1-cut)-min) / num * i + min
  }
  return(payout.structure)
}

#EVENTUALLY MAKE THIS CHANGE WITH THE SIM CHARACTERISTICS
best <- c('A','B','C','D','E','F','G','H','I','J')
sim.examples <- list(
  best = best,
  worst = rev(best),
  medium1 = c('A','I','B','H','J','F','D','E','G','C'),
  medium2 = c('E','D','G','I','A','B','F','C','J','H'),
  medium3 = c('I','B','E','F','G','J','H','C','A','D')
)

#add parameters to be able to use this in any sim
sim.examples.scores <- function(scoreMethod){
  examples <- list(
    best = c('A','B','C','D','E','F','G','H','I','J'),
    worst = rev(best),
    medium1 = c('A','I','B','H','J','F','D','E','G','C'),
    medium2 = c('E','D','G','I','A','B','F','C','J','H'),
    medium3 = c('I','B','E','F','G','J','H','C','A','D')
  )
  examples.con <- lapply(X = examples, FUN = stocks.convert)
  examples.matrix <- matrix(unlist(examples.con), nrow = length(examples.con), byrow = TRUE)
  return(sim.scores(examples.matrix))
}

#tells you how much money an outcome would have made
#pass in some particular outcome, all of the scores, all of the payouts
outcome.payout <- function(outcome, scores, payouts){
  scores.sorted <- sort(scores)
  position <- max(which(scores.sorted < outcome))
  position.payout <- payouts[position]
  return(position.payout)
}
