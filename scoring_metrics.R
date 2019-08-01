#Scoring models
#uses raw squares of distance from reality. Makes top and bottom most important in accuracy
#lower score is better
cScore <- function(x, y) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + (x[i] - y[i])^2
  }
  return(score)
}

#same as cScore, but takes the square root
cScoreSQRT <- function(x, y) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + (x[i] - y[i])^2
  }
  return(sqrt(score))
}

#cScoreAbsolute
cScoreAbsolute <- function(x, y) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + abs(x[i] - y[i])
  }
  return(score)
}

#cScoreAbsoluteReversed
cScoreAbsoluteReversed <- function(x, y, max = 9) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + max - abs(x[i] - y[i])
  }
  return(score)
}

#cScoreReversed
cScoreReversed <- function(x, y, max = 9) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + (max - abs(x[i] - y[i]))^2
  }
  return(score)
}

#cScoreReversedGeneral
cScoreReversedGeneral <- function(x, y, max = 9, exp = 2) {
  score <- 0
  for(i in 1:length(x)) {
    score <- score + (max - abs(x[i] - y[i]))^exp
  }
  return(score)
}

#cScoreReversedKendall
cScoreReversedKendall <- function(x, y, max = 9) {
  score <- 0
  for(i in 1:length(x)) {
    kendall <- cor.test(x,y, method = "kendall")
    score <- score + ( ((max - abs(x[i] - y[i]))^2) * (1 + kendall$estimate))
    
  }
  return(score)
}


#kendall coefficient for scoring
kendallScore <- function(x, y) {
  score <- cor.test(x, y, method = "kendall")
  return(score$estimate)
}


#takes max delta^x - delta^x
#x and y are both vectors of length 10
#maybe try to generalize this
#get it right - max delta pts
#get it perfectly wrong - 1 pts
#min~66  max~203.5
cScoreFinal <- function(x, y, expon = 1.54) {
  score.total <- 0
  
  for (i in seq(length(x))) {
    delta.max <- max(i - 1, length(x) - i)
    delta.max.root <- delta.max^(1/(length(x) - 1))
    delta.i <- abs(x[i] - y[i])
    score.i <- delta.max^expon / delta.max.root^delta.i
    score.total <- score.total + score.i
  }
  
  return(score.total)
}


